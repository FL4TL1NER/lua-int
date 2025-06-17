package interpreter

import cats.data.StateT

import Value.*

object Memory {
    type MemoryOp[A] = Either[String, A]
    type MemoryState[A] = StateT[MemoryOp, Memory, A]
    type Address = Long
    type Memory = Map[Address, Entry]

    final case class Entry(refCount: Long, refList: Seq[Address],value: Value)

    val emptyMemory: Memory = Map.empty

    def newEntry(v: Entry): MemoryState[Address] = StateT(input => {
        val newAddress: Address = if input.size > 0 then input.keys.max + 1 else 0
        Right(input + (newAddress -> v), newAddress)
    })

    def incRefCount(a: Address): MemoryState[Unit] = StateT(input => {
        val newEntry = Entry(input(a).refCount+1, input(a).refList, input(a).value)
        Right((input.updated(a, newEntry), ()))
    })

    def decRefCount(a: Address): MemoryState[Unit] = StateT(input => {
        if input(a).refCount-1 < 0 then
            Left("ref count < 0")
        else
            val newEntry = Entry(input(a).refCount-1, input(a).refList, input(a).value)
            Right((input.updated(a, newEntry), ()))
    })

    def removeEntry(address: Address): MemoryState[Unit] = StateT(input => {
        if input(address).refList.length > 0 then
            val states = input(address).refList.map(a => (decRefCount(a)))
            val new_state = input.removed(address)
            states.tail.foldLeft[MemoryState[Unit]](
                states.head
            )(
                (a, b) => a.flatMap((Unit) => b)
            ).run(new_state)
        else
            Right(input.removed(address), ())
    })

    def changeEntry(address: Address, v: Entry => Entry): MemoryState[Unit] = StateT(input => {
        Right(input + (address -> v(input(address))), ())
    })

    def addRef(where: Address, to: Address): MemoryState[Unit] = (
        for
            _ <- StateT[MemoryOp, Memory, Unit](input => {
                val entry = input(where)
                val new_entry = Entry(entry.refCount, entry.refList:+to, entry.value)
                Right(input + (where -> input(where)), ())
            })
            _ <- incRefCount(to)
        yield
            ()
    )

    def getEntry(address: Address): MemoryState[Entry] = StateT(input => {
        Right(input, input(address))
    })

    val collectGarbage: MemoryState[Unit] = StateT(input => {
        val states: List[MemoryState[Unit]] = (input.filter((k, v) => v.refCount == 0).map((k, v) => removeEntry(k))).toList
        if states.length > 0 then
            states.reduce((a, b) => a.flatMap((Unit) => b)).run(input)
        else
            Right(input, ())
    })
}

// @main
// def test() = {
//     import Memory.*
//     newEntry(Entry(1, Seq.empty, Num(10.4))).run(Memory.emptyMemory) match
//         case Left(value) => print(value)
//         case Right(state, address) =>
//             print(state.toString())
//             decRefCount(address).flatMap((_) => collectGarbage).run(state).foreach(a => print(a.toString()))
// }
