lines = `wc *.moa`.split("\n")[0...-1].map{|s| s.split(/ +/)[1...].map{|s| s.match(/^[0-9]+$/) ? s.to_i : s }}
max = lines.map{|a| a[0] }.max
puts lines.sort{|x| -x[0]}.map{|a| "#{(a[0].to_f / max * 100).ceil}%\t#{a[0]}\t#{a[-1]}"}.join("\n")
