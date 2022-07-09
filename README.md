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
$ moa new
```

3. Start the web server
```
$ moa server
```

4. Go to `http://localhost:3000` and you'll see the wellcome page

5. Edit `src/{index.mhtml, style.mcss, api.moa}` then you'll see updates by browser auto reloading
```
$ tree .
├─api.moa
├─index.mhtml
├─script.mjs
└─style.mcss
```

6. Compile files for production environment
```
$ moa release
```

```
$ tree .
├─release
│  ├─api.js
│  ├─log
│  ├─data
│  └─public
│      ├─index.html
│      ├─script.js
│      └─style.css
├─api.moa
├─index.mhtml
├─script.mjs
└─style.mcss
```

At the command prompt on server, serve your app
```
$ cd release && node index.js
```
