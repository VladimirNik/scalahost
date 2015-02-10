package scala.meta.internal.hosts.scalac.convert

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.Global

//TODO - add here trait with all classes and constants used in Deserialize

trait RefReps {

  val global: Global

  import global._

  trait RefRep {
    var sym: Symbol
    val ownerId: Int

    def isInitialized = sym ne null

    //TODO - add init symbol to else branch
    def initSym: Symbol = if (sym ne null) sym else { null}
  }

  case class NoSymbolRep() extends RefRep {
    var sym: NoSymbol.type = null

    val ownerId = -1

    override def toString = "NoSymbolRep"
  }

  case class RootSymbolRep() extends RefRep {
    var sym: RootSymbol = null

    val ownerId = -1

    override def toString = "RootSymbolRep"
  }

  case class LocalPackageSymbolRep(ownerId: Int, name: String) extends RefRep {
    var sym: Symbol = null

    override def toString = s"LocalPackageSymbolRep[name: $name, ownerId: $ownerId]"
  }

  case class LocalSymbolRep(ownerId: Int, treeId: Int) extends RefRep {
    var sym: Symbol = null

    override def toString = s"LocalSymbolRep[treeId: $treeId, ownerId: $ownerId]"
  }

  case class TypeSymbolRep(ownerId: Int, nameStr: String) extends RefRep {
    var sym: TypeSymbol = null
    val name: TypeName = newTypeName(nameStr)

    override def toString = s"TypeSymbolRep[nameStr: $nameStr, ownerId: $ownerId]"
  }

  case class TermSymbolRep(ownerId: Int, nameStr: String, erasedParamssCount: Int, erasedRet: Int) extends RefRep {
    var sym: TermSymbol = null
    var erasedParams: List[List[Int]] = Nil
    val name: TermName = newTermName(nameStr)

    override def toString = s"TermSymbolRep[nameStr: $nameStr, ownerId: $ownerId, erasedParamssCount: $erasedParamssCount, erasedParams: $erasedParams]"
  }

  case class ArraySymbolRep(ownerId: Int) extends RefRep {
    var sym = null

    override def toString = s"ArraySymbolRep[ownerId: $ownerId]"
  }

}
