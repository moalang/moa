def moa_branch(target, conds)
  target = target.__call
  conds.each do |(cond, body)|
    return body.__call if moa_branch_eq(target, cond)
  end
  raise Exception.new('Unexpected branch ' + target.inspect + ' ' + conds.inspect)
end
def moa_branch_eq(target, cond)
  is_moa_error = target.instance_of?(MoaError)
  return target == cond unless cond.instance_of?(Symbol)
  return !is_moa_error if cond == :ok
  return is_moa_error if cond == :err
  return target._tag == cond
end
def ok(v)
  v
end
class MoaError
  def initialize(message)
    @m = message
  end
  def to_s
    @m
  end
  def or(v)
    v
  end
end
def _method(name, &body)
  method(Object.define_method(name, body))
end
def err(s)
  MoaError.new(s)
end
class String
  def has(x)
    include?(x)
  end
end
class Proc
  def or(x)
    p = self
    lambda { |*args| p.call(*args).or(x) }
  end

  def to_s
    "lambda:#{source_location[1]}"
  end
end
class Object
  def or(_)
    self
  end
  def err?
    instance_of?(MoaError)
  end
  def __call
    instance_of?(Proc) ? self.call.__call : self
  end
  def run!
    v = __call
    v.instance_of?(Method) ? v.call.run! : v
  end
  def to_s
    self.to_s
  end
  def to_i
    self.to_int
  end
end
class MoaArray < Array
  def map(f)
    super() { |x| f.call(x) }
  end
  def count
    size
  end
end
