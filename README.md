# The Moa Programming Language
Moa is an open source programming language which helps programming for fun!

# Getting started

1. Install moa
```
$ mkdir -p ~/moa/bin
$ curl https://github.com/moa/bin/moa > ~/moa/bin/moa
$ export PATH=$PATH:~/moa/bin
```

2. At the command prompt, create a new web application
```
$ echo 'puts "Hello, Moa"' > main.moa
```

3. Run the program
```
$ moa run main.moa
Hello, Moa
```
4. Compile to the program as another programming language
```
$ moa js main.moa
console.log('Hello, moa')

$ moa go main.moa
package main

func main() {
  println("Hello, moa")
}
```



# For web development

1. At the command prompt, create a new web application
```
$ moa -web create
```

2. Start the web server
```
$ moa -web run
```

3. Go to `http://localhost:3000` and you'll see the wellcome page

4. Edit `src/{index.mhtml, style.mcss, api.moa}` then you'll see updates by browser auto reloading
```
$ tree .
├─api.moa
├─index.mhtml
├─script.mjs
└─style.mcss
```

5. Deploy to server
```
$ moa -web deploy your_name@hostname:/path/to/dir
```
