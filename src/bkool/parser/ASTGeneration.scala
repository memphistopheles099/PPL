package bkool.parser
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream
import bkool.utils._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree._

class ASTGeneration extends BKOOLBaseVisitor[Object] {

  override def visitProgram(ctx:BKOOLParser.ProgramContext) = 
    Program(if(ctx.clslist()==null)List()else ctx.clslist().accept(new ASTClassListGen))
}

class ASTDeclGen extends BKOOLBaseVisitor[List[Decl]]{
  override def visitVar_dec(ctx: BKOOLParser.Var_decContext) = ctx.idlist().accept(new ASTIdListGen).map((x:Id)=>VarDecl(x,ctx.getChild(2).accept(new ASTTypeGen)))
  override def visitCst_dec(ctx: BKOOLParser.Cst_decContext) = List(ConstDecl(Id(ctx.getChild(2).getText),ctx.getChild(1).accept(new ASTTypeGen),ctx.expr().accept(new ASTExprGen)))
  override def visitVarcondecl(ctx: BKOOLParser.VarcondeclContext) = ctx.getChild(0).accept(this)
}

class ASTParameterGen extends BKOOLBaseVisitor[List[ParamDecl]]{
  override def visitPrmlist(ctx:BKOOLParser.PrmlistContext) =
    if (ctx.prmlist()==null) ctx.param().accept(this)
    else ctx.param().accept(this)++ctx.prmlist().accept(this)
    
  override def visitParam(ctx:BKOOLParser.ParamContext) =
    ctx.idlist().accept(new ASTIdListGen).map((x:Id)=>ParamDecl(x, if(ctx.`type`()!=null)ctx.`type`().accept(new ASTTypeGen) else ctx.arr_type.accept(new ASTTypeGen)))
}

class ASTClassListGen extends BKOOLBaseVisitor[List[ClassDecl]]{
  override def visitClslist(ctx: BKOOLParser.ClslistContext) = 
    if (ctx.clslist()==null) ctx.cls_dec().accept(this)
    else ctx.cls_dec().accept(this)++ctx.clslist().accept(this)
  override def visitCls_dec(ctx: BKOOLParser.Cls_decContext) =     
    List(ClassDecl(new Id(ctx.ID(0).getText),if (ctx.ID(1)!=null)new Id(ctx.ID(1).getText) else new Id(""),if(ctx.mbrlist()==null) List() else ctx.mbrlist().accept(new ASTMemberListGen)))
}

class ASTMemberListGen extends BKOOLBaseVisitor[List[MemDecl]]{
  override def visitAttributedec(ctx:BKOOLParser.AttributedecContext) =
    if(ctx.att_dec().cst_dec()!=null) 
      List(AttributeDecl(if(ctx.att_dec().STATIC()==null)Instance else Static,ctx.att_dec().cst_dec().accept(new ASTDeclGen)(0)))++{if(ctx.mbrlist()==null)List() else ctx.mbrlist().accept(this)}
    else ctx.att_dec().var_dec().idlist().accept(new ASTIdListGen).map 
    { x => VarDecl(x, ctx.att_dec().var_dec.getChild(2).accept(new ASTTypeGen) )}.map 
    { x => AttributeDecl(if(ctx.att_dec().STATIC()==null)Instance else Static,x) }++{if(ctx.mbrlist()==null)List() else ctx.mbrlist().accept(this)}
  
  override def visitMethoddecl(ctx: BKOOLParser.MethoddeclContext) =
    if (ctx.mbrlist()==null) List(ctx.mth_dec().accept(new ASTMemberGen))
    else ctx.mth_dec().accept(new ASTMemberGen)::ctx.mbrlist().accept(this)
    
  override def visitConstructdecl(ctx: BKOOLParser.ConstructdeclContext) =
    if(ctx.mbrlist()==null) List(ctx.ctr_dec().accept(new ASTMemberGen))
    else ctx.ctr_dec().accept(new ASTMemberGen)::ctx.mbrlist().accept(this)
}

class ASTMemberGen extends BKOOLBaseVisitor[MemDecl]{
  override def visitMth_dec (ctx: BKOOLParser.Mth_decContext) =
    MethodDecl(if (ctx.STATIC()==null) Instance else Static, Id(ctx.ID.getText),
      if(ctx.prmlist()!=null)ctx.prmlist().accept(new ASTParameterGen) else List(),
      ctx.getChild(0).accept(new ASTTypeGen),
      ctx.blk_stm().accept(new ASTStmtGen))
  override def visitCtr_dec(ctx: BKOOLParser.Ctr_decContext) = MethodDecl(Instance,Id(ctx.ID().getText),if(ctx.prmlist()!=null)ctx.prmlist().accept(new ASTParameterGen) else List(),null,ctx.blk_stm().accept(new ASTStmtGen))
      
}

class ASTIdListGen extends BKOOLBaseVisitor[List[Id]]{
  override def visitIdlist(ctx:BKOOLParser.IdlistContext) = if (ctx.idlist()==null) List(Id(ctx.ID().getText))
    else List(Id(ctx.ID().getText))++ctx.idlist().accept(this)
}

class ASTTypeGen extends BKOOLBaseVisitor[Type] {
  override def visitVoidtype(ctx: BKOOLParser.VoidtypeContext) = VoidType
  override def visitType(ctx:BKOOLParser.TypeContext) = ctx.getChild(0).getText match {
    case "bool" => BoolType
    case "integer" => IntType
    case "float" => FloatType
    case "string" => StringType
    case "void" => VoidType
    case _ => ClassType(ctx.getChild(0).getText)
  }
  override def visitArr_type(ctx: BKOOLParser.Arr_typeContext) =
    ArrayType(IntLiteral(Integer.parseInt(ctx.INT_L().getText)),ctx.getChild(0).accept(this))
}

class ASTExprListGen extends BKOOLBaseVisitor[List[Expr]]{
  override def visitExplist(ctx: BKOOLParser.ExplistContext) = 
    if (ctx.explist()!=null)ctx.expr().accept(new ASTExprGen)::ctx.explist().accept(this)
    else List(ctx.expr().accept(new ASTExprGen))
}

class ASTStmtGen extends BKOOLBaseVisitor[Stmt]{
  override def visitBlk_stm(ctx: BKOOLParser.Blk_stmContext) = {
    var body: List[BKOOLParser.Body_blkContext] = ctx.body_blk().asScala.toList;
    var listdec:List[BKOOLParser.Body_blkContext] = body.filter { x => x.varcondecl()!=null }
    var liststmt:List[BKOOLParser.Body_blkContext] = body.filter { x => x.stmt()!=null }
    var finallistdec:List[Decl] = List();
    listdec.map { x => finallistdec=finallistdec++x.varcondecl().accept(new ASTDeclGen) }
    Block(finallistdec, liststmt.map { x => x.stmt().accept(new ASTStmtGen) })
  }
  override def visitAssignstmt(ctx: BKOOLParser.AssignstmtContext) = Assign(ctx.lhs().accept(new ASTLHSGen),ctx.expr().accept(new ASTExprGen))
  override def visitIfstmt(ctx: BKOOLParser.IfstmtContext) = 
    If(ctx.expr().accept(new ASTExprGen),ctx.stmt(0).accept(this), if(ctx.stmt(1)!=null)Some(ctx.stmt(1).accept(this))else None)
  override def visitMethodcallstmt(ctx: BKOOLParser.MethodcallstmtContext) = Call(ctx.getChild(0).accept(new ASTExprGen),Id(ctx.ID().getText), if (ctx.explist()!=null)ctx.explist().accept(new ASTExprListGen)else List())
  override def visitWhilestmt(ctx: BKOOLParser.WhilestmtContext) = While(ctx.expr().accept(new ASTExprGen),if(ctx.stmt()!=null)ctx.stmt().accept(this)else ctx.blk_stm().accept(new ASTStmtGen))
  override def visitBreakstmt(ctx: BKOOLParser.BreakstmtContext) = Break;    
  override def visitContinuestmt(ctx: BKOOLParser.ContinuestmtContext) = Continue;
  override def visitReturnstmt(ctx: BKOOLParser.ReturnstmtContext) = Return(ctx.expr.accept(new ASTExprGen))
  override def visitBlk_stm_stmt(ctx: BKOOLParser.Blk_stm_stmtContext) = ctx.blk_stm().accept(this)
}

class ASTExprGen extends BKOOLBaseVisitor[Expr]{
  override def visitNewexp(ctx: BKOOLParser.NewexpContext) = NewExpr(new Id(ctx.ID().getText),if(ctx.explist()!=null)ctx.explist().accept(new ASTExprListGen)else List())
  override def visitCallmed(ctx: BKOOLParser.CallmedContext) = CallExpr(if(ctx.getChild(0).getText=="self") SelfLiteral else ctx.getChild(0).accept(this),Id(ctx.ID().getText),if (ctx.explist()!=null)ctx.explist().accept(new ASTExprListGen)else List())
  override def visitCallatt(ctx: BKOOLParser.CallattContext) = FieldAccess(ctx.expr.accept(new ASTExprGen),Id(ctx.ID().getText))
  override def visitIndexexp(ctx: BKOOLParser.IndexexpContext) = ArrayCell(ctx.expr(0).accept(new ASTExprGen),ctx.expr(1).accept(new ASTExprGen))
  override def visitBracket(ctx: BKOOLParser.BracketContext) = ctx.expr().accept(this)
  override def visitConcat(ctx: BKOOLParser.ConcatContext) = BinaryOp(ctx.CONCAT().getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitBinmul(ctx: BKOOLParser.BinmulContext) = BinaryOp(ctx.MULOP().getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitBinadd(ctx: BKOOLParser.BinaddContext) = BinaryOp(ctx.ADDOP().getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitAndorexp(ctx: BKOOLParser.AndorexpContext) = BinaryOp(ctx.getChild(1).getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitEqualcomp(ctx: BKOOLParser.EqualcompContext) = BinaryOp(ctx.getChild(1).getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitThancomp(ctx: BKOOLParser.ThancompContext) = BinaryOp(ctx.getChild(1).getText,ctx.expr(0).accept(this),ctx.expr(1).accept(this))
  override def visitIntegerexp(ctx: BKOOLParser.IntegerexpContext) = UnaryOp(ctx.getChild(0).getText,ctx.expr().accept(this))
  override def visitNotexp(ctx: BKOOLParser.NotexpContext) = UnaryOp(ctx.getChild(0).getText,ctx.expr().accept(this))
  override def visitIntexp(ctx: BKOOLParser.IntexpContext) = IntLiteral(Integer.parseInt(ctx.getChild(0).getText))
  override def visitFltexp(ctx: BKOOLParser.FltexpContext) = FloatLiteral(ctx.getChild(0).getText.toFloat)
  override def visitStringexp(ctx: BKOOLParser.StringexpContext) = StringLiteral(ctx.getChild(0).getText)
  override def visitBooleanexp(ctx: BKOOLParser.BooleanexpContext) = BooleanLiteral(if (ctx.getChild(0).getText=="true") true else false)
  override def visitIdexp(ctx: BKOOLParser.IdexpContext) = Id(ctx.getChild(0).getText)
  override def visitSelfexp(ctx:BKOOLParser.SelfexpContext) = SelfLiteral
  override def visitNullexp(ctx:BKOOLParser.NullexpContext) = NullLiteral
}

class ASTLHSGen extends BKOOLBaseVisitor[LHS]{
  override def visitLhs(ctx: BKOOLParser.LhsContext) = ctx.children.size() match{
    case 1 => Id(ctx.ID().getText)
    case 3 => FieldAccess(ctx.expr(0).accept(new ASTExprGen),Id(ctx.ID().getText))
    case 4 => ArrayCell(ctx.expr(0).accept(new ASTExprGen),ctx.expr(1).accept(new ASTExprGen))
    case 5 => CallExpr(ctx.expr(0).accept(new ASTExprGen),Id(ctx.ID().getText),List())
    case 6 => CallExpr(ctx.expr(0).accept(new ASTExprGen),Id(ctx.ID().getText),ctx.explist().accept(new ASTExprListGen))
  }
}