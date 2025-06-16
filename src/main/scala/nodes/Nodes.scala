package nodes

object Nodes {
    case class EndNodeNumeral(a: Long | Double) extends NodeSimpleexp
    case class EndNodeLiteralString(string: String) extends NodeSimpleexp with NodeArgs
    case class EndNodeName(name: String)

    case class NodeChunk(block: NodeBlock)
    case class NodeBlock(stats: Seq[NodeStat],retstat: Option[NodeRetstat])

    sealed trait NodeStat
    case class NodeStatEmpty() extends NodeStat
    case class NodeVarlistAssignment(varlist: NodeVarlist,explist: NodeExplist) extends NodeStat
    case class NodeLabel(name: EndNodeName) extends NodeStat
    case class NodeBreak() extends NodeStat
    case class NodeGoto(name: EndNodeName) extends NodeStat
    case class NodeDoEnd(block: NodeBlock) extends NodeStat
    case class NodeWhile(exp: NodeExp, block: NodeBlock) extends NodeStat
    case class NodeRepeat(block: NodeBlock, exp: NodeExp) extends NodeStat
    case class NodeIf(exp: NodeExp, blockThen: NodeBlock, blockElse: Option[NodeBlock]) extends NodeStat
    case class NodeForInt(name: EndNodeName, exp1: NodeExp,exp2: NodeExp,exp3: NodeExp, block: NodeBlock) extends NodeStat
    case class NodeForExpr(namelist: NodeNamelist, explist: NodeExplist, block: NodeBlock) extends NodeStat
    case class NodeFunction(funcname: NodeFuncname, funcbody: NodeFuncbody) extends NodeStat with NodeSimpleexp
    case class NodeLocalFunction(name: EndNodeName, funcbody: NodeFuncbody) extends NodeStat
    case class NodeLocalNamelist(attnamelist: NodeAttnamelist,explist: Option[NodeExplist]) extends NodeStat

    case class NodeAttnamelist(namelist: Seq[(EndNodeName,Option[EndNodeName])])
    case class NodeRetstat(explist: Option[NodeExplist])
    case class NodeFuncname(name: EndNodeName,nameList: Seq[EndNodeName],self: Option[EndNodeName])

    case class NodeNamelist(namelist: Seq[EndNodeName])
    case class NodeVarlist(varlist: Seq[NodeVar])
    case class NodeExplist(explist: Seq[NodeExp])

    sealed trait NodeExp extends NodeField
    case class NodeBinOpExp(binOp: BinOp,nodeExp: NodeExp,nodeExp2: NodeExp) extends NodeExp
    case class NodeUnOpExp(unOp: UnOp,nodeExp: NodeExp) extends NodeExp

    sealed trait NodeSimpleexp extends NodeExp
    case class NodeNil() extends NodeSimpleexp
    case class NodeBoolean(value: Boolean) extends NodeSimpleexp
    case class NodeVararg() extends NodeSimpleexp with NodeParlist
    case class NodeFunctiondef(funcbody: NodeFuncbody) extends NodeSimpleexp
    case class NodeValue(start: EndNodeName | NodeExp,tail: Seq[NodeVar_index | NodeFunctioncall_args]) extends NodeSimpleexp
    case class NodeTableconstructor(fields: Seq[NodeField]) extends NodeSimpleexp with NodeArgs

    case class NodeFunctioncall(value: NodeValue) extends NodeStat
    case class NodeVar(value: NodeValue)

    case class NodeVar_index(nodeExp: NodeExp)
    case class NodeFunctioncall_args(self: Option[EndNodeName],args: NodeArgs)

    sealed trait NodeArgs
    case class NodeArgsBracets(explist: Option[NodeExplist]) extends NodeArgs

    case class NodeFuncbody(parlist: Option[NodeParlist], block: NodeBlock)

    sealed trait NodeParlist
    case class NodeParlistNamelist(namelist: NodeNamelist,vararg: Option[NodeVararg]) extends NodeParlist

    sealed trait NodeField
    case class NodeFieldExpExp(exp1: NodeExp,exp2: NodeExp) extends NodeField
    
    object BinOp:
        def find(s: String) = {BinOp.values.find(_.op == s)}
        
    enum BinOp(val op: String):
        case Addition       extends BinOp("+")
        case Substaction    extends BinOp("-")
        case Multiplication extends BinOp("*")
        case Division       extends BinOp("/")
        case FloorDivision  extends BinOp("//")

        case Modulo         extends BinOp("%")
        case Exponentiation extends BinOp("^")

        case BitwiseAND     extends BinOp("&")
        case BitwiseOR      extends BinOp("|")
        case BitwiseXOR     extends BinOp("~")
        case RightShift     extends BinOp(">>")
        case LeftShift      extends BinOp("<<")

        case Equality       extends BinOp("==")
        case Inequality     extends BinOp("~=")
        case LessThan       extends BinOp("<")
        case GreaterThan    extends BinOp(">")
        case LessOrEqual    extends BinOp("<=")
        case GreaterOrEqual extends BinOp(">=")

        case And            extends BinOp("and")
        case Or             extends BinOp("or")

        case Concatenation  extends BinOp("..")
    end BinOp

    object UnOp:
        def find(s: String) = {UnOp.values.find(_.op == s)}
    
    enum UnOp(val op: String):
        case Minus          extends UnOp("-")
        case BitwiseNOT     extends UnOp("~")
        case Length         extends UnOp("#")
        case NOT            extends UnOp("not")
    end UnOp
}
