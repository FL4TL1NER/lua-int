package interpreter

import scala.annotation.tailrec

import cats.data.StateT

import Value.*

object Stack {
    import Memory.Address

    type Stack = Seq[Map[String, Option[Address]]]
    type StackOp[A] = Either[String, A]
    type StackState[A] = StateT[StackOp, Stack, A]

    val emptyStack: Stack = Seq(Map.empty)

    val push: StackState[Unit] = StateT(input => {
        Right(input:+Map.empty, ())
    })
    val pop: StackState[Map[String, Option[Address]]] = StateT(input => {
        if input.length <= 1 then
            Left("attempt to pop global")
        else
            Right(input.dropRight(1), input.last)
    })

    def addGlobal(name: String, address: Option[Address]): StackState[Unit] = StateT(input => {
        Right((input.head + (name -> address)) +: input.tail, ())
    })
    def addLocal(name: String, address: Option[Address]): StackState[Unit] = StateT(input => {
        Right(input.dropRight(1) :+ (input.last + (name -> address)), ())
    })

    def getAddress(name: String): StackState[Option[Address]] = StateT(input => {
        @tailrec
        def getAdrRec(seq: Seq[Map[String, Option[Address]]]): Option[Address] = {
            if seq.length == 0 then
                None
            else
                seq.last.find((k, v) => k == name) match
                    case None => getAdrRec(seq.dropRight(1))
                    case Some(k, v) => v
        }
        Right(input, getAdrRec(input))
    })
}

// @main
// def test() = {
//     import Stack.*
//     print((addGlobal("dvach", Some(148L)).run(emptyStack)).toString)
// }
