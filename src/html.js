module.exports = () => {
  const _version = '0.0.1'
  const singles = 'area base br col embed hr img input link meta param source track wbr'.split(' ')
  const parse = template => {
    const tokens = template.split(/("[^"]*?"|#.+|[a-z][a-z0-9_]*="[^"]*"|[a-z][a-z0-9_]*=[^ \n]*"|[^ \n]+| +|\n[ \n]*)/).filter(t => t.replace(/ +/, ''))
    const initial = []
    let stack = [[initial], initial]
    let level = 1
    let line = 1
    for (const t of tokens) {
      if (t.startsWith('\n')) {
        line += 1
        const lv = 1 + t.split('\n').slice(-1)[0].length / 2
        if (lv > level + 1) {
          throw Error(`Invalid Indent at line ${line}`)
        }
        if (lv == level) {
          // do nothing
        } else if (lv > level) {
          const nest = []
          if (lv > level) {
            stack.push(nest)
          } else {
            stack[lv] = nest
          }
          stack[level].push(nest)
        } else {
          const latest = []
          stack[lv] = latest
          stack[lv - 1].push(latest)
        }
        level = lv
      } else {
        if (t.match(/^[a-z][a-z0-9]*$/)) {
          if (stack[level].length) {
            const list = [t]
            stack[level].push(list)
            stack[level] = list
          } else {
            stack[level].push(t)
          }
        } else {
          stack[level].push(t)
        }
      }
    }
    return stack[0]
  }
  const _render = (template, params) => {
    const get = key => {
      let p = params
      for (const part of key.split('.')) {
        if (part in p) {
          p = p[part]
        } else {
          console.log(`${part} is not found in ${key}`)
          return ''
        }
      }
      return p
    }
    const generate = node => Array.isArray(node) ? tag(node) : value(node)
    const value = node => node.startsWith('"') ? node.slice(1, -1) : get(node.slice(1))
    const tag = ([name, ...items]) => {
      let head = name
      const bodies = []
      for (const item of items) {
        if (typeof item === 'string' && item.match(/^[a-z][a-z0-9_]*=/)) {
          head += ' ' + item
        } else {
          bodies.push(generate(item))
        }
      }
      return `<${head}>${bodies.join('')}` + (singles.includes(name) ? '' : `</${name}>`)
    }
    const nodes = parse(template)
    return nodes.map(generate).join('\n')
  }
  return { _version, _render }
}

