package scala.meta.internal.hosts.scalac.convert

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.Global

//TODO - add here trait with all classes and constants used in Deserialize

trait RefReps {

  val global: Global

  import global._

  trait RefRep {
    var sym: Symbol = null
    val ownerId: Int

    def isInitialized = sym ne null

    //TODO - add init symbol to else branch
    def initSym: Symbol = if (sym ne null) sym else { null}
  }

  case class NoSymbolRep() extends RefRep {
    val ownerId = -1

    override def toString = "NoSymbolRep"
  }

  case class RootSymbolRep() extends RefRep {
    val ownerId = -1

    override def toString = "RootSymbolRep"
  }

  case class LocalPackageSymbolRep(ownerId: Int, name: String) extends RefRep {
    override def toString = s"LocalPackageSymbolRep[name: $name, ownerId: $ownerId]"
  }

  case class LocalSymbolRep(ownerId: Int, treeId: Int) extends RefRep {
    override def toString = s"LocalSymbolRep[treeId: $treeId, ownerId: $ownerId]"
  }

  case class TypeSymbolRep(ownerId: Int, nameStr: String) extends RefRep {
    val name: TypeName = newTypeName(nameStr)

    override def toString = s"TypeSymbolRep[nameStr: $nameStr, ownerId: $ownerId]"
  }

  case class TermSymbolRep(ownerId: Int, nameStr: String, erasedParamssCount: Int, erasedRet: Int) extends RefRep {
    var erasedParams: List[List[Int]] = Nil
    val name: TermName = newTermName(nameStr)

    override def toString = s"TermSymbolRep[nameStr: $nameStr, ownerId: $ownerId, erasedParamssCount: $erasedParamssCount, erasedParams: $erasedParams]"
  }

  case class ArraySymbolRep(ownerId: Int) extends RefRep {
    override def toString = s"ArraySymbolRep[ownerId: $ownerId]"
  }

}
