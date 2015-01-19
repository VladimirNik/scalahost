package scala.meta.internal.hosts.scalac.convert

import java.io.{File, FileOutputStream, BufferedOutputStream, BufferedWriter}

import scala.tools.nsc.Global

trait Serializer {

  val global: Global

  import global._

  import org.tukaani.xz.{LZMA2Options, XZOutputStream}
  import scala.reflect.internal.pickling.PickleBuffer

  import scala.collection.immutable.SortedMap
  import scala.collection.mutable
  import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
  import scala.runtime.BoxedUnit

  trait TreeTransform

  class Serialize extends TreeTransform {
    def phaseName: String = "Serialize"

    val serializer: LowLevelSerializer = new BinarySerializer

    val symbolOffsetsAprox = new mutable.HashMap[Symbol, Int]()

//    val annotationTransformer = mkTreeTransformer
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
//          annotationTransformer.macroTransform(annot.tree)
          annotationLevel -= 1
          writeType(tpe)
        case t: ThisType =>
          annotationLevel += 1
//          annotationTransformer.macroTransform(This(t.cls))
          annotationLevel -= 1
        case t: SingletonType =>
          annotationLevel += 1
//          annotationTransformer.macroTransform(tpd.ref(t.termSymbol))
          annotationLevel -= 1
//        case t: AndOrType =>
//          writeNodeHeader(tp)
//          writeType(t.tp1)
//          writeType(t.tp2)
        case t@TypeRef(prefix, sym, name) =>
          writeNodeHeader(tp)
          writeType(prefix)
          serializer.writeSymbolRef(t.typeSymbol)

      }
    }

    def singleRefTree(tree: Tree) = {
      writeNodeHeader(tree)
      serializer.writeSymbolRef(tree.symbol)
      this
    }

    def noActualFields(tree: Tree, writeSize: Boolean) = {
      writeNodeHeader(tree)
      this
    }

    // c
    def prepareForIdent(tree: Ident) = singleRefTree(tree)

    // r
    def prepareForSelect(tree: Select) = singleRefTree(tree)

    // c
    def prepareForThis(tree: This): TreeTransform = singleRefTree(tree)

    def prepareForSuper(tree: Super): TreeTransform = {
      writeNodeHeader(tree)
      serializer.writeSymbolRef(tree.qual.symbol)
      serializer.writeSymbolRef(tree.symbol.owner) // todo: check that this indeed returns `mix`
      this
    }

    def sizedTree(tree: Tree, args: Int): TreeTransform = {
      writeNodeHeader(tree)
      putSizeField(tree)
      this
    }

    def closeSizedTree(tree: Tree): Tree = {
      closeSizeField(tree)
      tree
    }

    def prepareForApply(tree: Apply): TreeTransform = sizedTree(tree, tree.args.size)

    def transformApply(tree: Apply): Tree = closeSizedTree(tree)

    def prepareForTypeApply(tree: TypeApply): TreeTransform = sizedTree(tree, tree.args.size)

    def transformTypeApply(tree: TypeApply): Tree = closeSizedTree(tree)

    def prepareForLiteral(tree: Literal): TreeTransform = {
      writeNodeHeader(tree)
      writeConst(tree.value)
      this
    }


//    override def prepareForPair(tree: Pair)(implicit ctx: Context): TreeTransform = {
//      writeNodeHeader(tree)
//      writeString(tree.left.show)
//      writeString(tree.right.show)
//      this
//    }

    def prepareForNew(tree: New): TreeTransform = noActualFields(tree, writeSize = false)

    def prepareForTyped(tree: Typed): TreeTransform = noActualFields(tree, writeSize = true)

    def transformTyped(tree: Typed): Tree = closeSizedTree(tree)

    def prepareForAssign(tree: Assign): TreeTransform = noActualFields(tree, writeSize = true)


    def transformAssign(tree: Assign): Tree = closeSizedTree(tree)

    def prepareForBlock(tree: Block): TreeTransform = sizedTree(tree, tree.stats.size + 1)

    def transformBlock(tree: Block): Tree = closeSizedTree(tree)

    def prepareForIf(tree: If): TreeTransform = noActualFields(tree, writeSize = true)

    def transformIf(tree: If): Tree = closeSizedTree(tree)

//    override def prepareForClosure(tree: tpd.Closure)(implicit ctx: Context): TreeTransform = sizedTree(tree, tree.env.size)

//    override def transformClosure(tree: tpd.Closure)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = closeSizedTree(tree)

    def prepareForMatch(tree: Match): TreeTransform = sizedTree(tree, tree.cases.size)

    def transformMatch(tree: Match): Tree = closeSizedTree(tree)

    def prepareForCaseDef(tree: CaseDef): TreeTransform = noActualFields(tree, writeSize = true)

    def transformCaseDef(tree: CaseDef): Tree = closeSizedTree(tree)

    def prepareForReturn(tree: Return): TreeTransform = noActualFields(tree, writeSize = true)

    def transformReturn(tree: Return): Tree = closeSizedTree(tree)

    def prepareForTry(tree: Try): TreeTransform = sizedTree(tree, tree.catches.size)

    def transformTry(tree: Try): Tree = closeSizedTree(tree)

    def prepareForThrow(tree: Throw): TreeTransform = noActualFields(tree, writeSize = true)

    def transformThrow(tree: Throw): Tree = closeSizedTree(tree)

//    override def prepareForSeqLiteral(tree: SeqLiteral)(implicit ctx: Context): TreeTransform = sizedTree(tree, tree.elems.size)
//
//    override def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context, info: TransformerInfo): Tree = closeSizedTree(tree)

    def prepareForTypeTree(tree: TypeTree): TreeTransform = {
      writeType(tree.tpe)
      this
    }

    def prepareForSelectFromTypeTree(tree: SelectFromTypeTree): TreeTransform = singleRefTree(tree)

    def prepareForBind(tree: Bind): TreeTransform = {
      writeNodeHeader(tree)
      writeString(tree.name.toString)
      writeBoolean(tree.name.isTypeName)
      this
    }

    def prepareForAlternative(tree: Alternative): TreeTransform = sizedTree(tree, tree.trees.size)

    def prepareForTypeDef(tree: TypeDef): TreeTransform = {
      symbolOffsetsAprox.put(tree.symbol, currentOffset)
      writeNodeHeader(tree)
      putSizeField(tree)
      writeAnnots(tree)
      writeString(tree.name.toString)
      this
    }


    def transformTypeDef(tree: TypeDef): Tree = closeSizedTree(tree)

    def prepareForUnApply(tree: UnApply): TreeTransform = {
      writeNodeHeader(tree)
      writeArgsCount(tree.args.size)
      putSizeField(tree)
      this
    }


    def transformUnApply(tree: UnApply): Tree = closeSizedTree(tree)

    def prepareForValDef(tree: ValDef): TreeTransform = {
      symbolOffsetsAprox.put(tree.symbol, currentOffset)
      writeNodeHeader(tree)
      putSizeField(tree)
      writeAnnots(tree)
      writeString(tree.name.toString)
      this
    }


    def transformValDef(tree: ValDef): Tree = closeSizedTree(tree)

    def prepareForDefDef(tree: DefDef): TreeTransform = {
      symbolOffsetsAprox.put(tree.symbol, currentOffset)
      writeNodeHeader(tree)
      putSizeField(tree)
      writeAnnots(tree)
      writeString(tree.name.toString)
      writeArgsCount(tree.tparams.size)
      writeArgsCount(tree.vparamss.size)
      for (i <- tree.vparamss)
        writeArgsCount(i.size)
      this
    }

    def transformDefDef(tree: DefDef): Tree = closeSizedTree(tree)

    def prepareForTemplate(tree: Template): TreeTransform = {
      writeNodeHeader(tree)
      putSizeField(tree)
      writeArgsCount(tree.parents.size)
      // writeArgsCount(tree.body.size) // to be replaced by tree size
      this
    }


    def transformTemplate(tree: Template): Tree = closeSizedTree(tree)

    def prepareForPackageDef(tree: PackageDef): TreeTransform = {
      writeNodeHeader(tree)
      putSizeField(tree)
      writeString(tree.pid.symbol.fullName)
      // writeArgsCount(tree.stats.size) // to be replaced by tree size
      this
    }


    def transformPackageDef(tree: PackageDef): Tree = closeSizedTree(tree)

    def prepareForUnit(tree: Tree): TreeTransform = {
      if (annotationLevel == 0) serializer.init(tree)
      this
    }


    def transformUnit(tree: Tree): Tree = {
      if (annotationLevel == 0) flush(tree)
      tree
    }

    implicit class NewPickleBuffer(pb: PickleBuffer) {
      def writeRawLong(b: Long): Unit = {
        pb.writeByte(((b >>> 56) & 0xFF).toInt)
        pb.writeByte(((b >>> 48) & 0xFF).toInt)
        pb.writeByte(((b >>> 40) & 0xFF).toInt)
        pb.writeByte(((b >>> 32) & 0xFF).toInt)
        pb.writeByte(((b >>> 24) & 0xFF).toInt)
        pb.writeByte(((b >>> 16) & 0xFF).toInt)
        pb.writeByte(((b >>> 8) & 0xFF).toInt)
        pb.writeByte(((b >>> 0) & 0xFF).toInt)
      }

      def writeRawInt(b: Int): Unit = {
        pb.writeByte(((b >>> 24) & 0xFF).toInt)
        pb.writeByte(((b >>> 16) & 0xFF).toInt)
        pb.writeByte(((b >>> 8) & 0xFF).toInt)
        pb.writeByte(((b >>> 0) & 0xFF).toInt)
      }

      /** append bytes bts[start..start + len - 1] */
      def writeBytes(bts: Array[Byte], start: Int, len: Int): Unit = {
        if(len > 0) {
          pb.ensureCapacity(len)
          Array.copy(bts, start, pb.bytes, pb.writeIndex, len)
          pb.writeIndex += len
        }
      }

      def writeBytes(bts: Array[Byte]): Unit = writeBytes(bts, 0, bts.length)
    }

    trait LowLevelSerializer {
      def init(unit: Tree): Unit

      def writeSymbolRef(sym: Symbol): Unit

      def writeNodeHeader(nd: Any /* Tree | Type */): Unit

      def putSizeField(t: Tree): Unit

      def closeSizeField(t: Tree): Unit

      def writeArgsCount(count: Int): Unit

      def writeStringId(str: String): Unit

      def writeConstantId(str: Constant): Unit

      def writeBoolean(value: Boolean): Unit

      def currentOffset: Int

      def flush(tree: Tree): Unit
    }

    class BinarySerializer extends LowLevelSerializer {

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

        def flushTrees: Unit = {
          var nextSizeIdx = 0

          var copiedUntil = 0
          while (nextSizeIdx < sizes.length) {
            writeAE(trees.bytes, copiedUntil, starts(nextSizeIdx))
            copiedUntil = starts(nextSizeIdx)
            output.writeNat(sizes(nextSizeIdx))
            nextSizeIdx = nextSizeIdx + 1
          }
          writeAE(trees.bytes, copiedUntil, trees.writeIndex)
        }

        writeByteArray(treesSectionNameBytes)
        flushTrees

        val f = java.io.File.createTempFile("serializedTree", ".tasty")
        f.createNewFile()

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


      def writeSymbolRef(sym: Symbol): Unit = {
        trees.writeNat(333333)
      }

      def serializeSymbol(sym: Symbol): Unit = {

      }

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

      def currentOffset: Int = 0

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
          //case _: moduleDef => 28 moduleDef
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

}