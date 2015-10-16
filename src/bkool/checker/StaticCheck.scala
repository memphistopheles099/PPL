package bkool.checker

/**
 * @author nhphung
 */

import bkool.parser._
import bkool.utils._
import java.io.{PrintWriter, File}
import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConverters._

case class SymList(sl:List[(String, Kind)]) extends Context

class BKVisitor extends Visitor{
  override def visit(ast: AST, c: Context): Object = ast.accept(this,c)
  
  override def visitProgram(ast: Program, c: Context): Object = 
    ast.decl.foldLeft(List(("io",Class)):List[(String,Kind)])((a,b)=>visit(b,SymList(a)).asInstanceOf[List[(String,Kind)]])
  
  override def visitVarDecl(ast: VarDecl, c: Context): Object =
    if (c.asInstanceOf[SymList].sl exists(_._1==ast.variable.name)) throw Redeclared(Variable,ast.variable.name)
    else (ast.variable.name,Variable)::c.asInstanceOf[SymList].sl
  
  override def visitConstDecl(ast: ConstDecl, c: Context): Object =
    if (c.asInstanceOf[SymList].sl exists(_._1==ast.id.name)) throw Redeclared(Variable,ast.id.name)
    else (ast.id.name,Variable)::c.asInstanceOf[SymList].sl
  
  override def visitClassDecl(ast: ClassDecl, c: Context): Object = 
    if (c.asInstanceOf[SymList].sl exists(_._1== ast.name.name)) throw Redeclared(Class, ast.name.name)
    else {
      ast.decl.foldLeft(List():List[(String,Kind)])((a,b)=> visit(b,SymList(a)).asInstanceOf[List[(String,Kind)]])
      return (ast.name.name,Class)::c.asInstanceOf[SymList].sl
    }
  
  override def visitMethodDecl(ast: MethodDecl, c: Context): Object =
    if(c.asInstanceOf[SymList].sl exists(_._1==ast.name.name)) throw Redeclared(Method, ast.name.name)
    else {
      /*
       * 
       * There's more
       * 
       */
      return (ast.name.name,Method)::c.asInstanceOf[SymList].sl
    }
  
  override def visitAttributeDecl(ast: AttributeDecl, c: Context): Object =
    ast.decl match{
    case ConstDecl(id, constType, const) =>
      if( c.asInstanceOf[SymList].sl exists (_._1==id.name)) throw Redeclared(Attribute, id.name)
      else (id.name,Attribute)::c.asInstanceOf[SymList].sl
    case VarDecl(variable, varType) =>
      if( c.asInstanceOf[SymList].sl exists (_._1==variable.name)) throw Redeclared(Attribute, variable.name)
      else (variable.name,Attribute)::c.asInstanceOf[SymList].sl
  }
  override def visitArrayCell(ast: bkool.utils.ArrayCell,c: bkool.utils.Context): Object = null
  override def visitArrayType(ast: bkool.utils.ArrayType,c: bkool.utils.Context): Object = null
  override def visitAssign(ast: bkool.utils.Assign,c: bkool.utils.Context): Object = null
  override def visitBinaryOp(ast: bkool.utils.BinaryOp,c: bkool.utils.Context): Object = null
  override def visitBlock(ast: bkool.utils.Block,c: bkool.utils.Context): Object = null
  override def visitBoolType(ast: bkool.utils.BoolType.type,c: bkool.utils.Context): Object = null
  override def visitBooleanLiteral(ast: bkool.utils.BooleanLiteral,c: bkool.utils.Context): Object = null
  override def visitBreak(ast: bkool.utils.Break.type,c: bkool.utils.Context): Object = null
  override def visitCall(ast: bkool.utils.Call,c: bkool.utils.Context): Object = null
  override def visitCallExpr(ast: bkool.utils.CallExpr,c: bkool.utils.Context): Object = null
  override def visitClassType(ast: bkool.utils.ClassType,c: bkool.utils.Context): Object = null
  override def visitContinue(ast: bkool.utils.Continue.type,c: bkool.utils.Context): Object = null
  override def visitFieldAccess(ast: bkool.utils.FieldAccess,c: bkool.utils.Context): Object = null
  override def visitFloatLiteral(ast: bkool.utils.FloatLiteral,c: bkool.utils.Context): Object = null
  override def visitFloatType(ast: bkool.utils.FloatType.type,c: bkool.utils.Context): Object = null
  override def visitId(ast: bkool.utils.Id,c: bkool.utils.Context): Object = null
  override def visitIf(ast: bkool.utils.If,c: bkool.utils.Context): Object = null
  override def visitInstance(ast: bkool.utils.Instance.type,c: bkool.utils.Context): Object = null
  override def visitIntLiteral(ast: bkool.utils.IntLiteral,c: bkool.utils.Context): Object = null
  override def visitIntType(ast: bkool.utils.IntType.type,c: bkool.utils.Context): Object = null
  override def visitNewExpr(ast: bkool.utils.NewExpr,c: bkool.utils.Context): Object = null
  override def visitNullLiteral(ast: bkool.utils.NullLiteral.type,c: bkool.utils.Context): Object = null
  override def visitParamDecl(ast: bkool.utils.ParamDecl,c: bkool.utils.Context): Object = null
  override def visitReturn(ast: bkool.utils.Return,c: bkool.utils.Context): Object = null
  override def visitSelfLiteral(ast: bkool.utils.SelfLiteral.type,c: bkool.utils.Context): Object = null
  override def visitStatic(ast: bkool.utils.Static.type,c: bkool.utils.Context): Object = null
  override def visitStringLiteral(ast: bkool.utils.StringLiteral,c: bkool.utils.Context): Object = null
  override def visitStringType(ast: bkool.utils.StringType.type,c: bkool.utils.Context): Object = null
  override def visitUnaryOp(ast: bkool.utils.UnaryOp,c: bkool.utils.Context): Object = null
  override def visitVoidType(ast: bkool.utils.VoidType.type,c: bkool.utils.Context): Object = null
  override def visitWhile(ast: bkool.utils.While,c: bkool.utils.Context): Object = null
}

class StaticChecker(ast:AST) {
  val astTree:AST = ast
  
  def convert(a:AST):(String, Kind) = a match {
    case AttributeDecl(kind, decl) => decl match {
      case ConstDecl(id, constType, const) => (id.toString(), Attribute)
      case ParamDecl(id, paramType) => (id.toString(), Parameter)
      case VarDecl(variable, varType) => (variable.toString(), Attribute)
    }
    case ClassDecl(name,parent, mem)=>(name.toString(),Class)
    case MethodDecl(kind, name, param, returnType, body) => (name.toString(), Method)
  }
  
  def checkAbove(elem:(String , Kind),upperList:List[(String , Kind)], parentName: Option[String]):(String, Kind)= {
    if (parentName == None){
      if (upperList exists (_._1== elem._1)) elem
      else null
    }
    else{
      upperList match {
        case List() => null
        case a::b => 
          if (elem._1==a._1&&(elem._2==Attribute||(elem._2==Method&&elem._1!=parentName.get))) elem 
          else checkAbove(elem, b, parentName)
      }
    }
  }
  
  def checkDup(list:List[(String , Kind)]):(String, Kind) = {
    list match{
      case List() => null
      case a:+b => if (checkDup(a)!=null) checkDup(a) else 
        if (a exists (_._1 == b._1)) b else null
    }
  }
  
  def check() = { ast.accept(new BKVisitor, null)
    /*
    var classList:List[(String, Kind)] = List()
    astTree match{
      case Program(decl) => {
        var classList:List[(String, Kind)] = List()
          //decl.foldLeft(List():List[(String, Kind)])((a,b)=> 
        for (x <- 0 to decl.size-1){
          classList=classList:+convert(decl(x))
          var classDup = checkDup(classList)
          if (classDup!= null) throw Redeclared(classDup._2,classDup._1)
          var memberList:List[(String, Kind)] = List()
          for (y <- 0 to decl(x).decl.size-1){
            var memberDup = checkAbove(convert(decl(x).decl(y)), classList,Some(decl(x).name.name))
            if (memberDup!=null) throw Redeclared(memberDup._2, memberDup._1)
            else {
              memberList=memberList:+convert(decl(x).decl(y))
              memberDup = checkDup(memberList)
              if (memberDup!=null) throw Redeclared(memberDup._2,memberDup._1)
            }
          }
        }/*
        else{
          for ( x <- 0 to classList.size-1){
            var memberList = decl(x).decl.foldLeft(List():List[(String, Kind)])((a:List[(String, Kind)],b:MemDecl)=>a:+convert(b))
            for (y <- 0 to memberList.size-1){
              var memberDup = checkAbove(memberList(y), classList,Some(classList(x)._1))
              if (memberDup!=null) throw Redeclared(memberDup._2, memberDup._1)
            }
            var memberDup = checkDup(memberList)
            if (memberDup!=null) throw Redeclared(memberDup._2,memberDup._1)
          }*/
      }
    }*/
  }
}