package scala.meta
package internal.hosts.scalac
package convert

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._

trait ConvertPhase {
  self: ScalahostPlugin =>

  object ConvertComponent extends NscPluginComponent {
    convSelf =>
    val global: self.global.type = self.global
    import global._

    // TODO: ideally we would like to save everything after the very end of typechecking, which is after refchecks
    // but unfortunately by then a lot of semantic stuff is already desugared to death (patmat, superaccessors, some code in refchecks)
    // therefore we run after typer and hope for the best (i.e. that we don't run into nonsense that we don't know how to convert,
    // and also that we don't encounter residual cyclic reference errors which are the reason why certain typechecks are delayed past typer)
    // btw this isn't such a big problem for persistence, but it definitely is for macro interpretation
    // let's hope that the research into runtime macros, which entails moving the typechecker to scala-reflect.jar will allow us to restructure things
    // so that delayed typechecks come right after typer, not intermingled with other logic
    override val runsAfter = List("typer")
    override val runsRightAfter = None
    val phaseName = "convert"
    override def description = "convert compiler trees to scala.meta"
    implicit val c = Scalahost.mkSemanticContext[global.type](global)

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        val punit = c.toScalameta(unit.body, classOf[Source])
        unit.body.appendMetadata("scalameta" -> punit)

        val tree = unit.body
        val rootMirror = global.rootMirror
        val RootClass = rootMirror.RootClass


//        val traverser = new Traverser {
//          override def traverse(tree: Tree) = {
//            val sym = tree.symbol
//            println()
//            println(s"tree: $tree")
//            println(s"sym: $sym")
//            println(s"showRaw(sym): ${showRaw(sym)}")
//            println("---")
//            if (sym != null && sym != NoSymbol) {
////              RootClass.ge
////              println(s"res: $res")
////              println(s"sym.ownerChain: ${sym.ownerChain}")
//              println(s"sym.info: ${if (sym.info.toString().size > 100) sym.info.toString.substring(0, 100) else sym.info}")
//              println(s"showRaw(sym.info): ${if (showRaw(sym.info).toString().size > 100) showRaw(sym.info).toString.substring(0, 100) else showRaw(sym.info)}")
//              println(s"sym.tpe: ${sym.tpe}")
//              println(s"showRaw(sym.tpe): ${showRaw(sym.tpe)}")
//              println("---")
//            }
//            if (tree.tpe != null  && tree.tpe != NoType) {
//              println(s"tree.tpe: ${tree.tpe}")
//              println(s"showRaw(tree.tpe): ${showRaw(tree.tpe)}")
////              if (sym != null)
////                println(s"sym.info =:= tree.tpe: ${sym.info =:= tree.tpe}")
//            }
//
//            println()
//            super.traverse(tree)
//          }
//        }

        val res = rootMirror.getModuleIfDefined("Main")
        println(s"res: $res")

//        traverser.traverse(tree)

        val serializer = new {
          val global: convSelf.global.type = convSelf.global
        } with Serialize
        serializer.serialize(unit.body)

        val file = serializer.serializer.getTestFile

        val deserializer = new {
          val global: convSelf.global.type = convSelf.global
        } with Deserialize

        deserializer.deserialize(file)
      }
    }
  }
}