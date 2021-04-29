sealed class Term()
class Lambda(val v: String, val body: Term): Term()
class Id(val name: String) : Term()
class Apply(val fn: Term, val arg: Term) : Term()
class Let(val v: String, val defn: Term, val body: Term) : Term()

sealed class Type()
class TypeVariable(): Type() {
  companion object {
    var nextVariableName: Char = 'a'
  }
  val name: String = "${nextVariableName++}"
  var instance: Type = UnkownT
  override fun toString(): String = name
}

open class TypeOperator(val name: String, val types: List<Type>) : Type() {
  override fun toString() = name + " " + types.map{it.toString()}.joinToString(",")
}
class FunctionT(name: String, types: List<Type>): TypeOperator(name, types) {
  constructor(fromType: Type, toType: Type): this(" ", listOf(fromType, toType))
  override fun toString() = types[0].toString()+" "+types[1].toString()
}
val IntegerT = TypeOperator("I", emptyList())
val BooleanT = TypeOperator("B", emptyList())
val UnkownT = TypeOperator("U", emptyList())

fun analyse(node: Term, env: Map<String,Type>):Type {
  return when(node) {
    is Id -> getType(node.name, env)
    is Apply -> {
      val funType = analyse(node.fn, env)
      val argType = analyse(node.arg, env)
      val resultType = TypeVariable()
      unify(FunctionT(argType, resultType), funType)
      resultType
    }
    is Lambda -> {
      val argType = TypeVariable()
      val resultType = analyse(node.body, env+(node.v to argType))
      FunctionT(argType, resultType)
    }
    is Let -> {
      val newType = TypeVariable()
      val newEnv = env+(node.v to newType)
      val defnType = analyse(node.defn,newEnv)
      unify(newType,defnType)
      analyse(node.body,newEnv)
    }
  }
}
fun getType(name: String, env: Map<String,Type>):Type {
  return env[name] ?: if (isIntegerTLiteral(name))
    IntegerT
  else
    throw Exception("Undefined symbol ${name}")
}
fun unify(t1: Type, t2: Type) {
  val a = prune(t1)
  val b = prune(t2)
  if (a is TypeVariable) {
    if (a!=b) {
      if(occursInType(a,b))
        throw Exception("recursive unification")
      else
        a.instance = b
    }
  } else if (a is TypeOperator && b is TypeVariable) {
    unify(b,a)
  } else if (a is TypeOperator && b is TypeOperator) {
    if (a.name != b.name || a.types.count() != b.types.count()) {
      throw Exception("Type missmtach ${a} != ${b}")
    }
    a.types.zip(b.types).forEach{(p,q) -> unify(p,q)}
  }
}
fun prune(t:Type):Type {
  return if (t is TypeVariable && t.instance != UnkownT) {
    t.instance = prune(t.instance)
    t.instance
  } else t
}
fun occursIn(t:Type, types:Iterable<Type>) = types.any{occursInType(t,it)}
fun occursInType(t1:Type,t2input:Type):Boolean {
  val t2=prune(t2input)
  return if (t2==t1) true
  else if (t2 is TypeOperator) occursIn(t1, t2.types)
  else false

}
fun isIntegerTLiteral(name:String) = name.toLongOrNull()!=null
fun dump(node: Term) : String {
    return when (node) {
        is Id -> node.name
        is Apply -> "(" + dump(node.fn) + " " + dump(node.arg) + ")"
        is Lambda -> node.v + " -> " + dump(node.body)
        is Let -> dump(node.body) + " : " + node.v + " " + dump(node.defn)
    }
}
fun main() {
    val var1 = TypeVariable()
    val env = mapOf(
        "true" to BooleanT,
        "false" to BooleanT,
        "if" to FunctionT(BooleanT, FunctionT(var1, FunctionT(var1, var1))),
        "prev" to FunctionT(IntegerT, IntegerT),
        "zero" to FunctionT(IntegerT, BooleanT),
        "times" to FunctionT(IntegerT, FunctionT(IntegerT, IntegerT))
    )

    listOf(
      Id("5"),              // 5 : int
      Lambda("n", Id("5")), // n -> 5 : a->int
      Lambda("n", Id("n")), // n -> n : a->a
      Lambda("n", Lambda("m", Id("5"))), // n -> m -> 5 : a->b->int
      Let("dec", Lambda("n", Apply(Id("prev"), Id("n"))), Apply(Id("dec"), Id("1"))),// fun dec(n) = n - 1; dec(1)
      Let("factorial",   // fun factorial(n) = if zero(n) 1 else n * factorial(n-1): int
      Lambda("n",
      Apply(Apply(Apply(Id("if"),
      Apply(Id("zero"),Id("n"))),
      Id("1")),
      Apply(Apply(Id("times"),Id("n")),Apply(Id("prev"),Id("n"))))),
      Apply(Id("factorial"), Id("5")))
    ).forEach {
      print(prune(analyse(it, env)))
      print("\t| ")
      println(dump(it))
    }
  }
