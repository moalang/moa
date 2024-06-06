Features
1. serve contents via HTTP
2. provide transactional database
3. handle WebSocket

A multiplex server handles TLS and forwards HTTP request to an app server, which is a dedicated process to handle single application.
Terms:
- Multiplex server handles TLS terminating and request forwarding
- App server handles single user's application in an isolated process
