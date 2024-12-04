#define _GNU_SOURCE // for accept4
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <errno.h>

#define PORT 8080
#define BACKLOG 4096
#define BUF_SIZE 1024
#define MAX_EVENTS 8192

static void *worker_thread(void *arg);
static void handle_client(int client_fd);

typedef struct {
    int thread_id;
} thread_arg_t;

int main() {
    int thread_pool_size = sysconf(_SC_NPROCESSORS_ONLN);
    if (thread_pool_size == -1) {
        perror("sysconf");
        return 1;
    }
    pthread_t threads[thread_pool_size];
    thread_arg_t args[thread_pool_size];
    for (int i = 0; i < thread_pool_size; ++i) {
        args[i].thread_id = i;
        pthread_create(&threads[i], NULL, worker_thread, &args[i]);
    }
    for (int i = 0; i < thread_pool_size; ++i) {
        pthread_join(threads[i], NULL);
    }
    return 0;
}

void *worker_thread(void *arg) {
    thread_arg_t *args = (thread_arg_t *)arg;
    int thread_id = args->thread_id;
    int server_fd;
    struct sockaddr_in server_addr;
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        pthread_exit(NULL);
        return NULL;
    }
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(int)) == -1) {
        perror("setsockopt");
        close(server_fd);
        pthread_exit(NULL);
        return NULL;
    }
    int flags;
    if ((flags = fcntl(server_fd, F_GETFL, 0)) == -1 || fcntl(server_fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        perror("fcntl");
        close(server_fd);
        pthread_exit(NULL);
        return NULL;
    }
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(PORT);
    server_addr.sin_addr.s_addr = INADDR_ANY;
    memset(&(server_addr.sin_zero), '\0', 8);
    if (bind(server_fd, (struct sockaddr *)&server_addr, sizeof(struct sockaddr)) == -1) {
        perror("bind");
        close(server_fd);
        pthread_exit(NULL);
    }
    if (listen(server_fd, BACKLOG) == -1) {
        perror("listen");
        close(server_fd);
        pthread_exit(NULL);
    }
    printf("Thread %d: listen %d\n", thread_id, PORT);
    int epoll_fd = epoll_create1(0);
    if (epoll_fd == -1) {
        perror("epoll_create1");
        close(server_fd);
        pthread_exit(NULL);
    }

    struct epoll_event event;
    event.data.fd = server_fd;
    event.events = EPOLLIN | EPOLLET; // Edge Triggered
    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, server_fd, &event) == -1) {
        perror("epoll_ctl: server_fd");
        close(server_fd);
        close(epoll_fd);
        pthread_exit(NULL);
    }
    struct epoll_event events[MAX_EVENTS];
    while (1) {
        int n = epoll_wait(epoll_fd, events, MAX_EVENTS, -1);
        if (n == -1) {
            if (errno == EINTR) {
                continue;
            }
            perror("epoll_wait");
            break;
        }
        for (int i = 0; i < n; ++i) {
            int fd = events[i].data.fd;
            if (events[i].events & (EPOLLERR | EPOLLHUP)) {
                close(fd);
            } else if (fd == server_fd) {
                while (1) {
                    struct sockaddr_in client_addr;
                    socklen_t sin_size = sizeof(struct sockaddr_in);
                    int client_fd = accept4(server_fd, (struct sockaddr *)&client_addr, &sin_size, SOCK_NONBLOCK);
                    if (client_fd == -1) {
                        if (errno == EAGAIN || errno == EWOULDBLOCK) {
                            break;
                        } else {
                            perror("accept4");
                            break;
                        }
                    }
                    struct epoll_event client_event;
                    client_event.data.fd = client_fd;
                    client_event.events = EPOLLIN | EPOLLET; // Edge Triggered
                    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, client_fd, &client_event) == -1) {
                        perror("epoll_ctl: client_fd");
                        if (close(client_fd) == -1) {
                            perror("close 2");
                        }
                        continue;
                    }
                }
            } else {
                char buf[BUF_SIZE];
                while (1) {
                  int recv_len = recv(fd, buf, BUF_SIZE - 1, 0);
                  if (recv_len == 0) {
                      if (epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, NULL) == -1) {
                        perror("epoll_ctl: delete");
                        break;
                      }
                      if (close(fd) == -1) {
                          perror("close 3");
                      }
                      break;
                  } else if (recv_len == -1) {
                      if (errno == EAGAIN || errno == EWOULDBLOCK) {
                          break;
                      } else {
                          if (epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, NULL) == -1) {
                              perror("epoll_ctl: delete");
                              break;
                          }
                          if (close(fd) == -1) {
                              perror("close 4");
                          }
                          break;
                      }
                  } else {
                      char* response = "HTTP/1.1 200 OK\r\nDate: Mon, 02 Dec 2024 07:40:51 GMT\r\nContent-Length: 5\r\nContent-Type: text/plain; charset=utf-8\r\n\r\nhello";
                      if (send(fd, response, strlen(response), 0) == -1) {
                          perror("send");
                      }
                  }
                }
            }
        }
    }

    close(server_fd);
    close(epoll_fd);
    pthread_exit(NULL);
    return NULL;
}
