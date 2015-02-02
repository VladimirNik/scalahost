package scala.meta.internal.hosts.scalac.convert

import java.io.{File, FileOutputStream, BufferedOutputStream, BufferedWriter}
import scala.tools.nsc.Global

trait Serialize extends SerializerUtils {

  val global: Global

  import global._

  import org.tukaani.xz.{LZMA2Options, XZOutputStream}
  import scala.reflect.internal.pickling.PickleBuffer

  import scala.collection.immutable.SortedMap
  import scala.collection.mutable
  import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
  import scala.runtime.BoxedUnit

  def phaseName: String = "Serialize"

  val serializer: LowLevelSerializer = new BinarySerializer

  val serTraverser = new SerializeTraverser

  val symbolOffsetsAprox = new mutable.HashMap[Symbol, Int]()

  //val annotationTransformer = mkTreeTransformer
  // used to recurse into annotations
  var annotationLevel = 0

  def writeString(str: String): Unit = {
    serializer.writeStringId(str)
  }

  def writeConst(c: Constant) = {
    serializer.writeConstantId(c)
  }

  def writeAnnots(t: DefTree): Unit = ()

  import serializer._

  def writeType(tp: Type): Unit = {
    tp match {
      case AnnotatedType(annot, tpe) =>
        writeNodeHeader(tp)
        annotationLevel += 1
        //annotationTransformer.macroTransform(annot.tree)
        annotationLevel -= 1
        writeType(tpe)
      case t: ThisType =>
        annotationLevel += 1
        //annotationTransformer.macroTransform(This(t.cls))
        annotationLevel -= 1
      case t: SingletonType =>
        annotationLevel += 1
        //annotationTransformer.macroTransform(tpd.ref(t.termSymbol))
        annotationLevel -= 1
      case t@TypeRef(prefix, sym, name) =>
        writeNodeHeader(tp)
        writeType(prefix)
        serializer.writeSymbolRef(t.typeSymbol)
    }
  }

  def singleRefTree(tree: Tree): Unit = {
    writeNodeHeader(tree)
    serializer.writeSymbolRef(tree.symbol)
  }

  def noActualFields(tree: Tree, writeSize: Boolean): Unit = {
    writeNodeHeader(tree)
  }

  // c
  def prepareForIdent(tree: Ident) = singleRefTree(tree)

  // r
  def prepareForSelect(tree: Select) = singleRefTree(tree)

  // c
  def prepareForThis(tree: This) = singleRefTree(tree)

  def prepareForSuper(tree: Super): Unit = {
    writeNodeHeader(tree)
    serializer.writeSymbolRef(tree.qual.symbol)
    serializer.writeSymbolRef(tree.symbol.owner) // todo: check that this indeed returns `mix`
  }

  def sizedTree(tree: Tree, args: Int): Unit = {
    writeNodeHeader(tree)
    /* putSizeField(tree) */
  }

  def closeSizedTree(tree: Tree): Tree = {
    closeSizeField(tree)
    tree
  }

  def prepareForApply(tree: Apply): Unit = sizedTree(tree, tree.args.size)

  def prepareForTypeApply(tree: TypeApply): Unit = sizedTree(tree, tree.args.size)

  def prepareForLiteral(tree: Literal): Unit = {
    writeNodeHeader(tree)
    writeConst(tree.value)
  }

  def prepareForNew(tree: New): Unit = noActualFields(tree, writeSize = false)

  def prepareForTyped(tree: Typed): Unit = noActualFields(tree, writeSize = true)

  def prepareForAssign(tree: Assign): Unit = noActualFields(tree, writeSize = true)

  def prepareForBlock(tree: Block): Unit = sizedTree(tree, tree.stats.size + 1)

  def prepareForIf(tree: If): Unit = noActualFields(tree, writeSize = true)

  def prepareForMatch(tree: Match): Unit = sizedTree(tree, tree.cases.size)

  def prepareForCaseDef(tree: CaseDef): Unit = noActualFields(tree, writeSize = true)

  def prepareForReturn(tree: Return): Unit = noActualFields(tree, writeSize = true)

  def prepareForTry(tree: Try): Unit = sizedTree(tree, tree.catches.size)

  def prepareForThrow(tree: Throw): Unit = noActualFields(tree, writeSize = true)

  def prepareForTypeTree(tree: TypeTree): Unit = {
    writeType(tree.tpe)
  }

  def prepareForSelectFromTypeTree(tree: SelectFromTypeTree): Unit = singleRefTree(tree)

  def prepareForBind(tree: Bind): Unit = {
    writeNodeHeader(tree)
    writeString(tree.name.toString)
    writeBoolean(tree.name.isTypeName)
  }

  def prepareForAlternative(tree: Alternative): Unit = sizedTree(tree, tree.trees.size)

  def prepareForTypeDef(tree: TypeDef): Unit = {
    symbolOffsetsAprox.put(tree.symbol, currentOffset)
    writeNodeHeader(tree)
    writeAnnots(tree)
    writeString(tree.name.toString)
  }

  def prepareForUnApply(tree: UnApply): Unit = {
    writeNodeHeader(tree)
    writeArgsCount(tree.args.size)
  }

  def prepareForValDef(tree: ValDef): Unit = {
    symbolOffsetsAprox.put(tree.symbol, currentOffset)
    writeNodeHeader(tree)
    writeAnnots(tree)
    writeString(tree.name.toString)
  }

  def prepareForDefDef(tree: DefDef): Unit = {
    symbolOffsetsAprox.put(tree.symbol, currentOffset)
    writeNodeHeader(tree)
    writeAnnots(tree)
    writeString(tree.name.toString)
    writeArgsCount(tree.tparams.size)
    writeArgsCount(tree.vparamss.size)
    for (i <- tree.vparamss)
      writeArgsCount(i.size)
  }

  def prepareForModuleDef(tree: ModuleDef): Unit = {
    symbolOffsetsAprox.put(tree.symbol, currentOffset)
    writeNodeHeader(tree)
    writeAnnots(tree)
    writeString(tree.name.toString)
  }

  def prepareForTemplate(tree: Template): Unit = {
    writeNodeHeader(tree)
    writeArgsCount(tree.parents.size)
    writeArgsCount(tree.body.size) // to be replaced by tree size
  }

  def prepareForPackageDef(tree: PackageDef): Unit = {
    writeNodeHeader(tree)
    writeString(tree.pid.symbol.fullName)
    writeArgsCount(tree.stats.size) // to be replaced by tree size
  }

  def prepareForImport(imp: Import): Unit = {
    writeNodeHeader(imp)
    writeArgsCount(imp.selectors.size)
  }

  def prepareForImportSelector(is: ImportSelector): Unit = {
    writeNodeHeader(is)
    writeString(is.name.toString)
    writeString(is.rename.toString)
  }

  def serialize(tree: Tree): Unit = {
    prepareForUnit(tree)
    transformUnit(tree)
  }

  def prepareForUnit(tree: Tree): Unit = {
    if (annotationLevel == 0) {  //TODO fix - some conditions should be done without dependency on annotationLevel
      serializer.init(tree)
      serTraverser.traverse(tree)
      addTreeIdToLocalRefs
    }
  }

  def transformUnit(tree: Tree): Unit = {
    if (annotationLevel == 0) flush(tree)
  }

  class SerializeTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = {
      //if TreeDef
      linkDefWithOffset(tree)
      tree match {
        case EmptyTree =>
          writeNodeHeader(EmptyTree)
        case ident: Ident =>
          prepareForIdent(ident)
        case s: Select =>
          prepareForSelect(s)
        case t: This =>
          prepareForThis(t)
        case s: Super =>
          prepareForSuper(s)
        case a: Apply =>
          prepareForApply(a)
        case ua: UnApply =>
          prepareForUnApply(ua)
        case ta: TypeApply =>
          prepareForTypeApply(ta)
        case l: Literal =>
          prepareForLiteral(l)
        case n: New =>
          prepareForNew(n)
        case t: Typed =>
          prepareForTyped(t)
        case a: Assign =>
          prepareForAssign(a)
        case b: Block =>
          prepareForBlock(b)
        case tr: If =>
          prepareForIf(tr)
        case m: Match =>
          prepareForMatch(m)
        case cd: CaseDef =>
          prepareForCaseDef(cd)
        case r: Return =>
          prepareForReturn(r)
        case t: Try =>
          prepareForTry(t)
        case t: Throw =>
          prepareForThrow(t)
        case b: Bind =>
          prepareForBind(b)
        case a: Alternative =>
          prepareForAlternative(a)
        case a: Annotated =>
          ()
        case vd: ValDef =>
          prepareForValDef(vd)
        case dd: DefDef =>
          prepareForDefDef(dd)
        case md: ModuleDef =>
          prepareForModuleDef(md)
        case t: Template =>
          prepareForTemplate(t)
        case pd: PackageDef =>
          prepareForPackageDef(pd)
        case i: Import =>
          prepareForImport(i)
        case is: ImportSelector =>
          prepareForImportSelector(is)
        case tt: TypeTree =>
          prepareForTypeTree(tt)
        //          case at: AnnotatedType =>
        //          case stt: SingletonTypeTree =>
        //          case sftt: SelectFromTypeTree =>
        //          case tbt: TypeBoundsTree =>
        //          case ett: ExistentialTypeTree =>
        //          case att: AppliedTypeTree =>
      }
      super.traverse(tree)
    }
  }

//  implicit class NewPickleBuffer(pb: PickleBuffer) {
//    def writeRawLong(b: Long): Unit = {
//      pb.writeByte(((b >>> 56) & 0xFF).toInt)
//      pb.writeByte(((b >>> 48) & 0xFF).toInt)
//      pb.writeByte(((b >>> 40) & 0xFF).toInt)
//      pb.writeByte(((b >>> 32) & 0xFF).toInt)
//      pb.writeByte(((b >>> 24) & 0xFF).toInt)
//      pb.writeByte(((b >>> 16) & 0xFF).toInt)
//      pb.writeByte(((b >>> 8) & 0xFF).toInt)
//      pb.writeByte(((b >>> 0) & 0xFF).toInt)
//    }
//
//    def writeRawInt(b: Int): Unit = {
//      pb.writeByte(((b >>> 24) & 0xFF).toInt)
//      pb.writeByte(((b >>> 16) & 0xFF).toInt)
//      pb.writeByte(((b >>> 8) & 0xFF).toInt)
//      pb.writeByte(((b >>> 0) & 0xFF).toInt)
//    }
//
//    /** append bytes bts[start..start + len - 1] */
//    def writeBytes(bts: Array[Byte], start: Int, len: Int): Unit = {
//      if(len > 0) {
//        pb.ensureCapacity(len)
//        //def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int)
//        Array.copy(bts, start, pb.bytes, pb.writeIndex, len)
//        pb.writeIndex += len
//      }
//    }
//
//    def writeBytes(bts: Array[Byte]): Unit = writeBytes(bts, 0, bts.length)
//  }

  trait LowLevelSerializer {
    def init(unit: Tree): Unit

    def writeSymbolRef(sym: Symbol, writeToTrees: Boolean = true): Unit

    def writeNodeHeader(nd: Any /* Tree | Type */): Unit

    def putSizeField(t: Tree): Unit

    def closeSizeField(t: Tree): Unit

    def writeArgsCount(count: Int): Unit

    def writeStringId(str: String): Unit

    def writeConstantId(str: Constant): Unit

    def writeBoolean(value: Boolean): Unit

    def currentOffset: Int

    def flush(tree: Tree): Unit

    def linkDefWithOffset(tree: Tree): Unit

    def addTreeIdToLocalRefs: Unit

    //TODO - remove - only for test purposes
    def getTestFile: File
  }

  class BinarySerializer extends LowLevelSerializer {

    //TODO - remove - only for test purposes
    var testFile: File = _
    def getTestFile = testFile

    val verifyClosing: Boolean = true


    val treesToBeClozed = new mutable.Stack[Tree]()

    def putSizeField(t: Tree): Unit = {
      starts += trees.writeIndex
      sizes += -1
      if (verifyClosing) treesToBeClozed.push(t)
    }

    def serializedSizeOfNat(int: Int): Int = {
      assert(int > 0)
      var acc = 1
      var j = int
      while (j != 0) {
        acc += 1
        j = j >>> 7
      }
      acc
    }

    def closeSizeField(t: Tree): Unit = {
      // to be rewritten to index lookup
      if (verifyClosing) {
        val toBeClosedNext = treesToBeClozed.pop()
        assert(t eq toBeClosedNext)
      }
      val idx = sizes.lastIndexOf(-1)
      var size = trees.writeIndex - starts(idx)
      var j = idx + 1
      while (j < sizes.length) {
        /**
         * Find the first index of element that doesn't compare less than key, otherwise -1
         */
        def lowerBound(key: Int, start: Int, length: Int): Int = {
          var len = length - start
          var first = start
          while (len > 0) {
            val half = len >>> 1
            val middle = first + half
            if (starts(middle) < key) {
              first = middle + 1
              len = len - half - 1
            } else {
              len = half
            }
          }
          if (first < length)
            first
          else
            -1
        }

        assert(sizes(j) != -1)
        size += serializedSizeOfNat(sizes(j))
        val nextSizeStart = lowerBound(starts(j) + sizes(j), j, starts.length)
        if (nextSizeStart > 0)
          j = nextSizeStart
        else j = sizes.length
      }
      sizes(idx) = size
    }

    val current = 0
    // there could be a class Line(start, end, next) here, but I've done (array-of-structs -> struct of arrays) by hand,
    // knowing that start[i] = end[i-1]

    val starts = new mutable.ArrayBuffer[Int]
    // this is boxed. could be reimplemented for speed
    val sizes = new mutable.ArrayBuffer[Int]()
    val trees = new PickleBuffer(new Array[Byte](1024), 0, 0)
    val MAGIC = 0x000A11CE
    val VERSION = 0
    val compilerName = "Scala 2.11.5"
    val compilerNameBytes = compilerName.getBytes("UTF-8")
    val referencesSectionName = "references"
    val referencesSectionNameBytes = referencesSectionName.getBytes("UTF-8")
    val treesSectionName = "trees"
    val treesSectionNameBytes = treesSectionName.getBytes("UTF-8")
    val constantsSectionName = "constants"
    val constantsSectionNameBytes = constantsSectionName.getBytes("UTF-8")
    val stringsSectionName = "strings"
    val stringsSectionNameBytes = stringsSectionName.getBytes("UTF-8")

    val treeOffsetsForDefs = mutable.HashMap[Symbol, Int]()
    def linkDefWithOffset(tree: Tree): Unit = {
      tree match {
        case _: DefTree => treeOffsetsForDefs.put(tree.symbol, serializer.currentOffset)
        case _ =>
      }
    }

    def flush(tree: Tree): Unit = {
      val output = new PickleBuffer(new Array[Byte](2048), 0, 0)
      output.writeRawInt(MAGIC)
      output.writeRawInt(VERSION)

      def writeAE(bt: Array[Byte], st: Int, until: Int): Unit = {
        output.writeRawInt(until - st)
        output.writeBytes(bt, st, until - st)
      }
      def writeByteArray(bt: Array[Byte]): Unit = writeAE(bt, 0, bt.size)
      def writePB(pb: PickleBuffer): Unit = writeAE(pb.bytes, 0, pb.writeIndex)

      writeByteArray(compilerNameBytes)

      writeByteArray(constantsSectionNameBytes)
      writePB(serializedConstants)

      writeByteArray(stringsSectionNameBytes)
      writePB(serializedStrings)

      //TODO - it should be done when we include size field
//      def flushTrees: Unit = {
//        var nextSizeIdx = 0
//
//        var copiedUntil = 0
//        while (nextSizeIdx < sizes.length) {
//          writeAE(trees.bytes, copiedUntil, starts(nextSizeIdx))
//          copiedUntil = starts(nextSizeIdx)
//          output.writeNat(sizes(nextSizeIdx))
//          nextSizeIdx = nextSizeIdx + 1
//        }
//        writeAE(trees.bytes, copiedUntil, trees.writeIndex)
//      }

      writeByteArray(treesSectionNameBytes)
      writePB(trees)
      //TODO uncomment it after addition of sized in trees section
//      flushTrees

      val f = java.io.File.createTempFile("serializedTree", ".tasty")
      f.createNewFile()

      //TODO - remove (only for test purposes)
      testFile = f

      val d = new BufferedOutputStream(new FileOutputStream(f))
      println(s"flushing ${output.writeIndex} bytes of tree: $tree to ${f.getCanonicalPath}")
      d.write(output.bytes, 0, output.writeIndex)
      d.close()


      val xzf = new File(f.getCanonicalPath + ".xz")

      val cf = new BufferedOutputStream(new FileOutputStream(xzf))
      val outxz = new XZOutputStream(cf, new LZMA2Options(6))

      println(s"Compressing to ${xzf.getCanonicalPath}")
      outxz.write(output.bytes, 0, output.writeIndex)
      outxz.close()
      val csize = xzf.length() - 64
      println(s"Compressed size $csize, compression ratio ${(csize - 64) * 1.0 / output.writeIndex}, + additional 64 bits of CRC64")
    }

    def init(unit: Tree): Unit = {
    }

    val serializedSymbols = new PickleBuffer(new Array[Byte](1024), 0, 0)
    val symbolOffsets = new mutable.HashMap[Symbol, Int]()

    def writeSymbolRef(sym: Symbol, writeToTrees: Boolean = true): Unit = {
      val id = getOrUpdateSymbol(sym)
      if (writeToTrees) trees.writeNat(id)
      else serializedSymbols.writeNat(id)
    }

    def getOrUpdateSymbol(sym: Symbol): Int = symbolOffsets.getOrElseUpdate(sym, addSymbol(sym))

    def addSymbol(sym: Symbol): Int = {
      val cur = serializedSymbols.writeIndex
      writeRefHeader(sym)
      sym match {
        case NoSymbol =>
        case _ if sym.isRoot | sym.isRootSymbol =>
        case _ if sym.hasPackageFlag =>
          writeSymbolRef(sym.owner, writeToTrees = false)
          writeStringIdFromRefSection(sym.fullName)
        //Local
        case _ if !sym.isLocatable =>
          writeSymbolRef(sym.owner, writeToTrees = false)
          //localDefinition (TODO)
        //External Type
        case _ if sym.isType =>
          writeSymbolRef(sym.owner, writeToTrees = false)
          writeStringIdFromRefSection(sym.fullName)
        //External Term
        case _ if sym.isTerm =>
          writeSymbolRef(sym.owner, writeToTrees = false)
          writeStringIdFromRefSection(sym.fullName)
          //erasedParamssCount
          serializedSymbols.writeNat(sym.paramss.size)
          //erasedParamsCount
          for (params <- sym.paramss) {
            serializedSymbols.writeNat(params.size)
            for (param <- params) {
              //erasedParams - TODO-get erased params
              writeSymbolRef(param.tpe.typeSymbolDirect, writeToTrees = false)
            }
          }
          //erasedRet
          //TODO-fix for non-methods
          writeSymbolRef(sym.tpe.resultType.typeSymbolDirect, writeToTrees = false)
        //case array => //TODO implement for Array
      }
      cur
    }

    def writeRefHeader(nd: Symbol): Unit = {
      val id = (nd: @unchecked) match {
        case NoSymbol => 0
        case _ if nd.isRoot => 1 //TODO-check isRoot for NoSymbol and RootClass equality (should be false, true)
        //Local
        //TODO - rewrite !sym.isLocatable
        case sym: Symbol if !sym.isLocatable => 2
        //External Type
        case sym: Symbol if sym.isType => 3
        //External Term
        case sym: Symbol if sym.isTerm => 4
        case sym: Symbol if sym.hasPackageFlag => 5
        //case array => 6 //TODO implement for Array
      }
      serializedSymbols.writeByte(id)
    }

    def writeStringIdFromRefSection(str: String): Unit = {
      val id = stringOffsets.getOrElseUpdate(str, addString(str))
      serializedSymbols.writeNat(id)
    }

    def addTreeIdToLocalRefs: Unit = {
      symbolOffsets.filter(_._1.isLocatable) foreach {
        x =>
          val sym = x._1
          val symOffset = x._2
          val treeIdValue = treeOffsetsForDefs.getOrElse(sym, -1)
          //treeIdPosition - position of treeId field in Local ref - (treeId field is after type field)
          val treeIdPosition = symOffset + 1
          serializedSymbols.patchNat(treeIdPosition, treeIdValue)
      }
    }

    //TODO implement modifiers rewriting
    def writeModifiersId(mod: Modifiers, idx: Int): Unit = ()

    val serializedStrings = new PickleBuffer(new Array[Byte](1024), 0, 0)
    val stringOffsets = new mutable.HashMap[String, Int]()

    def addString(str: String): Int = {
      val data = str.getBytes("UTF-8")
      val cur = serializedStrings.writeIndex
      serializedStrings.writeRawInt(data.length)
      serializedStrings.writeBytes(data)
      cur
    }
    def writeStringId(str: String): Unit = {
      val id = stringOffsets.getOrElseUpdate(str, addString(str))
      trees.writeNat(id)
    }

    val serializedConstants = new PickleBuffer(new Array[Byte](1024), 0, 0)
    val constantOffsets = new mutable.HashMap[Any, Int]()

    def serializeConstant(c: Constant): Array[Byte] = {
      //        import dotty.tools.dotc.core.Constants._
      c.tag match {
        case UnitTag => Array.apply(0.toByte)
        case NullTag => Array.apply(1.toByte)
        case BooleanTag if c.booleanValue => Array.apply(2.toByte)
        case BooleanTag if !c.booleanValue => Array.apply(3.toByte)
        case ByteTag => Array.apply(4.toByte, c.byteValue)
        case ShortTag =>
          val s = c.shortValue
          Array.apply(5.toByte, ((s >>> 8) & 0xFF).toByte, (s & 0xFF).toByte)
        case CharTag =>
          val a = c.charValue
          Array.apply(6.toByte, a.toString.getBytes("UTF-8"): _*)
        case IntTag =>
          val i = c.intValue
          Array.apply(7.toByte, ((i >>> 24) & 0xFF).toByte, ((i >>> 16) & 0xFF).toByte, ((i >>> 8) & 0xFF).toByte, (i & 0xFF).toByte)
        case LongTag =>
          val l = c.longValue
          Array.apply(8.toByte,
            ((l >>> 56) & 0xFF).toByte, ((l >>> 48) & 0xFF).toByte, ((l >>> 40) & 0xFF).toByte, ((l >>> 32) & 0xFF).toByte,
            ((l >>> 24) & 0xFF).toByte, ((l >>> 16) & 0xFF).toByte, ((l >>> 8) & 0xFF).toByte, (l & 0xFF).toByte)
        case FloatTag =>
          val i = java.lang.Float.floatToIntBits(c.floatValue)
          Array.apply(9.toByte, ((i >>> 24) & 0xFF).toByte, ((i >>> 16) & 0xFF).toByte, ((i >>> 8) & 0xFF).toByte, (i & 0xFF).toByte)
        case DoubleTag =>
          val l = java.lang.Double.doubleToRawLongBits(c.doubleValue)
          Array.apply(10.toByte,
            ((l >>> 56) & 0xFF).toByte, ((l >>> 48) & 0xFF).toByte, ((l >>> 40) & 0xFF).toByte, ((l >>> 32) & 0xFF).toByte,
            ((l >>> 24) & 0xFF).toByte, ((l >>> 16) & 0xFF).toByte, ((l >>> 8) & 0xFF).toByte, (l & 0xFF).toByte)
        case StringTag =>
          val a = c.stringValue
          val i = stringOffsets.getOrElseUpdate(a, addString(a))
          Array.apply(11.toByte, ((i >>> 24) & 0xFF).toByte, ((i >>> 16) & 0xFF).toByte, ((i >>> 8) & 0xFF).toByte, (i & 0xFF).toByte)
        case ClazzTag =>
          ???
        case EnumTag =>
          ???
      }
    }

    def writeConstantId(str: Constant): Unit = {
      val id = constantOffsets.getOrElseUpdate(str, {
        val data = serializeConstant(str)
        val cur = serializedConstants.writeIndex
        serializedConstants.writeBytes(data)
        cur
      })
      trees.writeNat(id)
    }

    def currentOffset: Int = trees.writeIndex

    def writeNodeHeader(nd: Any /* Tree | Type */): Unit = {
      val id = (nd: @unchecked) match {
        case EmptyTree => 0
        case _: Ident@unchecked => 1
        case _: Select@unchecked | _: TypeRef => 2
        case _: This@unchecked => 3
        case _: Super@unchecked => 4
        case _: Apply@unchecked => 5
        case _: UnApply@unchecked => 6
        //          case _: NamedArg@unchecked => 7
        //          case _: SeqLiteral@unchecked => 8
        case _: TypeApply@unchecked => 9
        case _: Literal@unchecked => 10
        case _: New@unchecked => 11
        case _: Typed@unchecked => 12
        case _: Assign@unchecked => 13
        case _: Block@unchecked => 14
        case _: If@unchecked => 15
        //          case _: Closure@unchecked => 16
        case _: Match@unchecked => 17
        case _: CaseDef@unchecked => 18
        case _: Return@unchecked => 19
        case _: Try@unchecked => 20
        case _: Throw@unchecked => 21
        case _: Bind@unchecked => 22
        case _: Alternative@unchecked => 23
        //24 is annotation
        case _: ValDef@unchecked => 25
        case _: DefDef@unchecked => 26
        case _: TypeDef@unchecked => 27
        case _: ModuleDef@unchecked => 28
        case _: Template@unchecked => 29
        case _: PackageDef@unchecked => 30
        case _: Import@unchecked => 31
        //          case _: Pair@unchecked => 32
        case _: AnnotatedType@unchecked => 33
        case _: SingletonType@unchecked => 34
        case _: SelectFromTypeTree@unchecked => 35
        //          case _: AndType@unchecked => 36
        //          case _: OrType@unchecked => 37
        case _: RefinedType@unchecked => 38
        //          case _: ByNameTypeTree@unchecked => 39
        //case _: RepeatedType => 40
        case _: TypeBounds@unchecked => 41
        //case _: ExistentialType => 42
        //case _: AppliedType => 43
      }
      trees.writeByte(id)
    }

    def writeBoolean(value: Boolean): Unit = {
      if (value) trees.writeByte(1)
      else trees.writeByte(0)
    }

    def writeArgsCount(count: Int): Unit = {
      trees.writeNat(count)
    }
  }
}