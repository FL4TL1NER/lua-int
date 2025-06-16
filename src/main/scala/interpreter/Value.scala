package interpreter

import nodes.Nodes.NodeFuncbody
import Memory.Address

object Value {
    sealed trait Value

    final case class Nil() extends Value
    final case class Num(a: Long | Double) extends Value
    final case class Str(s: String) extends Value
    final case class Bool(a: Boolean) extends Value
    final case class Function(a: NodeFuncbody) extends Value
    final case class Function2(f: Seq[Value] => Seq[Value]) extends Value
    final case class Table(a: Map[Num | Str | Bool, Address]) extends Value
}