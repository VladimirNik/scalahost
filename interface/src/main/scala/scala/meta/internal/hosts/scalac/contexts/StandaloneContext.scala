package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.invariants._
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.compat.Platform.EOL
import org.scalameta.reflection.mkGlobal
import scala.tools.nsc.reporters.StoreReporter
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}

class StandaloneContext(options: String) extends ScalahostSemanticContext(mkGlobal(options)) {
  private val reporter: StoreReporter = g.reporter.require[StoreReporter]
  def define(code: String): m.Tree = {
    g.reporter.reset()
    val gunit = g.newCompilationUnit(code, "<scalahost>")
    val gtree = g.newUnitParser(gunit).parse()
    if (reporter.hasErrors) sys.error("parse has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))
    val gtypedtree = {
      import g.{reporter => _, _}
      import analyzer._
      val run = new Run
      phase = run.namerPhase
      globalPhase = run.namerPhase
      val namer = newNamer(rootContext(gunit))
      namer.enterSym(gtree)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(gunit))
      val typedpkg = typer.typed(gtree).require[Tree]
      for (workItem <- gunit.toCheck) workItem()
      if (reporter.hasErrors) sys.error("typecheck has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))
      typedpkg.require[PackageDef]
    }
    val _ = toMtree.computeConverters // TODO: necessary because of macro expansion order
    toMtree(gtypedtree, classOf[mapi.Source])
  }
}
