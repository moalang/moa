"use strict"
import {Script} from "vm"
import readline from "readline"
import vm from "vm"
import {$, Glob} from "bun"
import {parse} from "./parse.js"
import {infer} from "./infer.js"
import {evaluate, generate} from "./evaluate.js"

if (import.meta.main) {
  const tojs = async () => {
    const files = process.argv[3] ? process.argv.slice(2) : await Array.fromAsync(new Glob("**/*.moa").scan("."))
    const result = await Promise.all(files.map(async file => generate(parse(await Bun.file(file).text(), file))))
    const js = result.join("\n")
  }
  if (process.argv[2] === "build") {
    Bun.write("/tmp/build-moa.js", await tojs())
    $("bun build /tmp/build-moa.js --compile --outfile a")
  } else if (process.argv[2] === "run") {
    console.log(new vm.Script(await tojs()).runInNewContext({}))
  } else if (process.argv[2] === "repl") {
    // TODO: improve formatting for syntax tree
    const context = vm.createContext({ console, Math, Date })
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: "> "
    })
    let ast, js
    rl.prompt()
    rl.on("line", line => {
      if (line.startsWith(":")) {
        if (line === ":exit" || line === ":quit" || line === ":e" || line == ":q") {
          rl.close()
        }
        if (line === ":ast") {
          console.dir(ast, {depth: null})
        }
        if (line === ":js") {
          console.log(js)
        }
      } else {
        try {
          ast = infer(parse(line, "repl.moa"))
          js = generate(ast)
          const result = new vm.Script(js, { timeout: 1000 }).runInContext(context)
          console.log(result)
        } catch (e) {
          console.dir(ast, {depth: null})
          console.log(js)
          console.log(e)
        }
      }
      rl.prompt()
    })
    rl.on("close", () => console.log("Bye"))
  } else if (process.argv[2] === "test") {
    console.log("implementing...")
  } else if (process.argv[2] === "version") {
    console.log("moa version 0.0.1 (Bun)")
  } else {
    console.log(`Moa is a programming language

Usage: moa <command> [...arguments]

Commands:
  moa build [...files]     compile to an executable file
  moa run   [...files]     compile and run
  moa repl  [...files]     start a shell
  moa test  [...files]     run tests
  moa version              display Moa version`)
  }
}
