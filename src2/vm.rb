require 'byebug'

def moa_branch(target, conds)
  target = target.run!
  conds.each do |(cond, body)|
    return body.run! if moa_branch_eq(target, cond)
  end
  raise Exception.new('Unexpected branch ' + target.inspect + ' ' + conds.inspect)
end
def moa_branch_eq(target, cond)
  return true if cond == :_
  return target if target.instance_of?(MoaPanic)
  is_moa_error = target.instance_of?(MoaError)
  return target == cond unless cond.instance_of?(Symbol)
  return !is_moa_error if cond == :ok
  return is_moa_error if cond == :err
  return target._tag == cond
end
def ok
  lambda { |v| v }
end
def err
  lambda { |s| MoaError.new(s) }
end
def panic
  lambda do |s|
    MoaPanic.new(s)
  end
end
class MoaStmt
  def initialize(f)
    @f = f
  end
  def call
    @f.call
  end
  def or(x)
    p = @f
    MoaStmt.new(lambda { |*args| p.call(*args).or(x) })
  end
  def to_s
    @f.to_s
  end
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
class MoaPanic
  def initialize(message)
    @m = message
  end
  def to_s
    @m
  end
  def or(_)
    self
  end
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
    return true if instance_of?(MoaError)
    if instance_of?(MoaPanic)
      if $DEBUG
        byebug
      else
        return true
      end
    end
  end
  def __call(*args)
    instance_of?(Proc) && (arity == args.size || arity == -1) ? self.call(*args).__call : self
  end
  def run!
    v = __call
    ret = v.instance_of?(Method) || v.instance_of?(MoaStmt) ? v.call.run! : v
    byebug if $DEBUG && ((ret.instance_of?(Array)) && ret.any?{|x| x.instance_of?(Proc)})
    ret
  end
end
class Array
  def map(*f)
    if block_given?
      super() { |*x| yield *x }
    else
      super() { |x| f[0].call(x) }
    end
  end
  def find(*f)
    x = if block_given?
          super() { |*x| yield *x }
        else
          super() { |x| f[0].call(x) }
        end
    return x if x
    MoaError.new("not found")
  end
  def has(x)
    include?(x)
  end
  def count
    size
  end
end
class String
  def count
    size
  end
end
