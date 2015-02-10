package scala.meta.internal.hosts.scalac.convert

import java.io.{File, FileOutputStream, BufferedOutputStream, BufferedWriter}
import scala.tools.nsc.Global

trait Deserialize extends SerializerUtils with TastyConstants with RefReps {

  val global: Global

  import global._

  import org.tukaani.xz.{LZMA2Options, XZOutputStream}
  import scala.reflect.internal.pickling.PickleBuffer

  import scala.collection.immutable.SortedMap
  import scala.collection.mutable
  import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
  import scala.runtime.BoxedUnit

  val deserializer = new BinaryDeserializer

  def deserialize(file: File) = deserializer.deserialize(file)

  class BinaryDeserializer {

    def convertToString(ar: Array[Byte]): String = new String(ar, "UTF-8")

    def deserialize(file: File): Unit = {
      import java.nio.file.Files
      import java.nio.file.Paths
      import java.nio.file.Path

      val path = Paths.get(file.getPath)
      val data = Files.readAllBytes(path)

      val input = new PickleBuffer(data, 0, 0)

      val magic: Int = input.readRawInt
      val version: Int = input.readRawInt

//      println(s"Magic: $magic, should be: ${0x000A11CE}")
//      println(s"version: $version, should be: 0")

      def readAE(pb: PickleBuffer): Array[Byte] = {
        val size = pb.readRawInt
        pb.readBytes(size)
      }
      def readByteArray(pb: PickleBuffer): Array[Byte] = readAE(pb)
      def readString(pb: PickleBuffer) = convertToString(readByteArray(pb))
      def readPB(pb: PickleBuffer): PickleBuffer = new PickleBuffer(readAE(pb), 0, 0)

      val compilerNameBytes = readString(input)

      val constantsSectionNameBytes = readString(input)
      val serializedConstants = readPB(input)

      //TODO - methods to deserialize strings section
      def readConstant: Constant = {
        val constType = serializedConstants.readByte()
        constType match {
          //Unit
          case 0 => Constant(())
          //Null
          case 1 => Constant(null)
          //True
          case 2 => Constant(true)
          //False
          case 3 => Constant(false)
          //Byte
          case 4 => Constant(serializedConstants.readByte())
          //Short
          case 5 =>
            //TODO - check
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = (((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)).toShort
            Constant(value)
          //Char
          case 6 =>
            val value = readString(serializedConstants)
            //TODO - maybe rewrite
            Constant(value.toCharArray().apply(0))
          //Int
          case 7 =>
            //TODO - check
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            Constant(value)
          //Long
          case 8 =>
            //TODO - check
            val i56 = serializedConstants.readByte()
            val i48 = serializedConstants.readByte()
            val i40 = serializedConstants.readByte()
            val i32 = serializedConstants.readByte()
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = ((i56 & 0xFF) << 56).toLong + ((i48 & 0xFF) << 48).toLong + ((i40 & 0xFF) << 40).toLong + ((i32 & 0xFF) << 32).toLong +
              ((i24 & 0xFF) << 24).toLong + ((i16 & 0xFF) << 16).toLong + ((i8 & 0xFF) << 8).toLong + ((i0 & 0xFF) << 0).toLong
            Constant(value)
          //Float
          case 9 =>
            //TODO - rewrite
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val intBits = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val res = java.lang.Float.intBitsToFloat(intBits)
            Constant(res)
          //Double
          case 10 =>
            //TODO - rewrite
            val i56 = serializedConstants.readByte()
            val i48 = serializedConstants.readByte()
            val i40 = serializedConstants.readByte()
            val i32 = serializedConstants.readByte()
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val longBits = ((i56 & 0xFF) << 56) + ((i48 & 0xFF) << 48) + ((i40 & 0xFF) << 40) + ((i32 & 0xFF) << 32) +
              ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val res = java.lang.Double.longBitsToDouble(longBits)
            Constant(res)
          //String
          case 11 =>
            //TODO - check
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val stringId = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val value = getStringById(stringId)
            //TODO - maybe rewrite
            Constant(value)
        }
      }

      val stringsSectionNameBytes = readString(input)
      val serializedStrings = readPB(input)

      def deserializeConstants: mutable.Map[Int, Constant] = {
        val list = serializedConstants.until(serializedConstants.bytes.size,
          () => {
            val ind = serializedConstants.readIndex
            val const = readConstant
            (ind, const)
          }
        )
        mutable.Map(list: _*)
      }

      //TODO - method to deserialize strings section
      def deserializeStrings: mutable.Map[Int, String] = {
        val list = serializedStrings.until(serializedStrings.bytes.size,
          () => {
            val ind = serializedStrings.readIndex
            val str = readString(serializedStrings)
            (ind, str)
          }
        )
        mutable.Map(list: _*)
      }

      //strings should be initialized before constants
      strings ++= (deserializeStrings)
      constants ++= (deserializeConstants)

//      println(s"constants: $constants")
//      println(s"strings: $strings")

      val treesSectionNameBytes = readString(input)
      val serializedTrees = readPB(input)

      def getStringByIdInTrees(): String = {
        val stringId = serializedTrees.readNat()
        getStringById(stringId)
      }

      def getConstantByIdInTrees(): Constant = {
        val constantId = serializedTrees.readNat()
//        println(s"constantId: $constantId")
        getConstantById(constantId)
      }

      //TODO: trees processing
      def deserializeTrees: Tree = {
        val treeId = serializedTrees.readByte()
//        println(s"treeId: $treeId")

        def deserializeNumOfTrees(num: Int): List[Tree] = {
          val res = (0 until num).toList map { x =>
            deserializeTrees
          }
          res
        }

        def readTreeArgs: List[Tree] = {
          //read number of trees
          val num: Int = serializedTrees.readNat()
//          println(s"num: $num")

          //deserialize args
          deserializeNumOfTrees(num)
        }

        val tree = (treeId) match {
          case 0 => EmptyTree
          //Ident
          case 1 =>
            val symId = serializedTrees.readNat()
            Ident(symId.toString)

          //Select | TypeRef
          case 2 =>
            val symId = serializedTrees.readNat()
            val qual = deserializeTrees
            Select(qual, symId.toString)

          //This
          case 3 =>
            val symId = serializedTrees.readNat()
            This(newTypeName(symId.toString))

          //Super
          case 4 =>
            val symQualId = serializedTrees.readNat()
            val symOwnerId = serializedTrees.readNat()
            Super(Ident(symQualId.toString), newTypeName(symOwnerId.toString))

          //Apply
          case 5 =>
            val argsCount: Int = serializedTrees.readNat()
            val fun = deserializeTrees
            val args = deserializeNumOfTrees(argsCount)
            Apply(fun, args)

          //UnApply
          case 6 => ???

          //case 7 => NamedArg
          //case 8 => SeqLiteral

          //TypeApply
          case 9 =>
            val argsCount: Int = serializedTrees.readNat()
            val fun = deserializeTrees
            val args = deserializeNumOfTrees(argsCount)
            TypeApply(fun, args)

          //Literal
          case 10 =>
            Literal(getConstantByIdInTrees())

          //New
          case 11 =>
            val tree = deserializeTrees
            New(tree)

          //Typed
          case 12 =>
            val expr = deserializeTrees
            val tpt = deserializeTrees
            Typed(expr, tpt)

          //Assign
          case 13 =>
            val lhs = deserializeTrees
            val rhs = deserializeTrees
            Assign(lhs, rhs)

          //Block
          case 14 =>
            val stats = readTreeArgs
            Block(stats: _*)

          //If
          case 15 =>
            val ifp = deserializeTrees
            val thenp = deserializeTrees
            val elsep = deserializeTrees
            If(ifp, thenp, elsep)

          //case 16 => Closure

          //Match
          case 17 => ???

          //CaseDef
          case 18 => ???

          //Return
          case 19 => ???

          //Try
          case 20 => ???

          //Throw
          case 21 => ???

          //Bind
          case 22 => ???

          //Alternative
          case 23 => ???

          //24 is annotation

          //ValDef
          case 25 =>
            val name = getStringByIdInTrees()
//            println(s"name-ValDef: $name")
            val tpt = deserializeTrees
            val rhs = deserializeTrees
            ValDef(NoMods, newTermName(name), tpt, rhs)

          //DefDef
          case 26 =>
            val name = getStringByIdInTrees()
//            println(s"name: $name")
            val tparamsCount: Int = serializedTrees.readNat()
//            println(s"tparamsCount: $tparamsCount")

            val vparamssCount: Int = serializedTrees.readNat()
//            println(s"vparamssCount: $vparamssCount")

            val vparamsArray: Array[Int] = new Array(vparamssCount)
            for (i <- (0 until vparamsArray.size)) {
              vparamsArray(i) = serializedTrees.readNat()
            }
//            println(s"vparamsArray.toList: ${vparamsArray.toList}")

            val tparams = deserializeNumOfTrees(tparamsCount) map (tr => tr.asInstanceOf[TypeDef])
//            println(s"tparams: $tparams")

            val vparamss: List[List[ValDef]] =
              vparamsArray.toList map {
                vparamsCount =>
                deserializeNumOfTrees(vparamsCount) map (tr => tr.asInstanceOf[ValDef])
              }
//            println(s"vparamss: $vparamss")

            val tpt = deserializeTrees
//            println(s"tpt: ${showRaw(tpt)}")

            val rhs = deserializeTrees
//            println(s"rhs: ${showRaw(rhs)}")

            DefDef(NoMods, newTermName(name), tparams,
              vparamss, tpt, rhs)

          //TypeDef
          case 27 =>
            val name = getStringByIdInTrees()
            val tparamsCount: Int = serializedTrees.readNat()
            val tparams = deserializeNumOfTrees(tparamsCount) map (tr => tr.asInstanceOf[TypeDef])
            val rhs: Tree = deserializeTrees
            TypeDef(NoMods, newTypeName(name), tparams, rhs)

          //ModuleDef
          case 28 =>
            val name = getStringByIdInTrees()
            val impl = deserializeTrees.asInstanceOf[Template]
            ModuleDef(NoMods, newTermName(name), impl)

          //Template
          case 29 =>
            val parentsCount: Int = serializedTrees.readNat()
            val bodyCount: Int = serializedTrees.readNat()

            val parents = deserializeNumOfTrees(parentsCount)
//            println(s"=====> parents: ${parents}")

            //TODO - fix - noSelfType is not persisted
            //val self = deserializeTrees
            //println(s"=====> self: ${self}")
            //val selfy = self.asInstanceOf[ValDef]

            val constr = deserializeTrees
//            println(s"=====> constr: ${constr}")

            val body = deserializeNumOfTrees(bodyCount-1) //constr is already deserialized
//            println(s"=====> body: ${body}")

            Template(parents, noSelfType, constr :: body)

//          //Template (working)
//          case 29 =>
//            val parentsCount: Int = serializedTrees.readNat()
//            val bodyCount: Int = serializedTrees.readNat()
//            val constr = deserializeTrees
//            println(s"constr: ${showRaw(constr)}")
//            val parents = deserializeNumOfTrees(parentsCount)
//            println(s"parents: ${parents}")
//            val self = deserializeTrees.asInstanceOf[ValDef]
//            val body = deserializeNumOfTrees(bodyCount)
//            Template(parents, self, constr :: body)

          //PackageDef
          case 30 =>
            val argsSize = serializedTrees.readNat()
            val ident = deserializeTrees.asInstanceOf[RefTree]

            //writeArgsCount(tree.stats.size) // to be replaced by tree size
            val args = deserializeNumOfTrees(argsSize)
//            println(s"args: $args")
            PackageDef(ident, args)

          //Import
          case 31 => //what to do with ImportSelector - they are not trees for us!
            val argsCount = serializedTrees.readNat()
            val expr = deserializeTrees
            //TODO - fix it
            Import(expr, List(ImportSelector(newTermName("test"), argsCount, newTermName("test"), argsCount)))

          //case 32 => Pair

          //AnnotatedType
          case 33 =>
            //TODO - fix
            TypeTree(NoType)

          //SingletonType
          case 34 =>
            //TODO - fix
            TypeTree(NoType)

          //SelectFromTypeTree
          case 35 => ???

          //case 36 => AndType
          //case 37 => OrType

          //RefinedType
          case 38 =>
            //TODO - fix
            TypeTree(NoType)

          //case 39 => ByNameTypeTree
          //case 40 => RepeatedType

          //TypeBounds
          case 41 =>
            //TODO - fix
            TypeTree(NoType)

          //case 42 => ExistentialType
          //case 43 => AppliedType

          //TODO - remove just for test purposes
          //TypeRef | ThisType
          case 44 =>
            //TODO - fix
            TypeTree(NoType)

          case _ => ???
        }
        val r = tree
//        println(s"r: ${showRaw(r)}")
        r
      }
      val res = deserializeTrees

      val symbolsSectionNameBytes = readString(input)
      val serializedSymbols = readPB(input)

      def readRef: RefRep = {
        val refId = serializedSymbols.readByte()
        refId match {
          case NoSymbolVal => NoSymbolRep()
          case RootSymbolVal => RootSymbolRep()
          case LocalPackageSymbolVal =>
            val ownerId = serializedSymbols.readNat()
            val name = readString(serializedSymbols)
            LocalPackageSymbolRep(ownerId, name)
          //Local
          case LocalSymbolVal =>
            val ownerId = serializedSymbols.readNat()
            val treeId = serializedSymbols.readNat()
            LocalSymbolRep(ownerId, treeId)
          //External Type
          case TypeSymbolVal =>
            val ownerId = serializedSymbols.readNat()
            val name = readString(serializedSymbols)
            TypeSymbolRep(ownerId, name)
          //External Term
          case TermSymbolVal =>
            val ownerId = serializedSymbols.readNat()
            val name = readString(serializedSymbols)
            val paramssSize = serializedSymbols.readNat()

            val paramRefIds: List[List[Int]] = for (paramsSizeIndex <- paramssSize) {
              val paramsSize = serializedSymbols.readNat()
              for (paramId <- paramsSize) {
                val param = serializedSymbols.readNat()
              }
            }

            val erasedRetId = serializedSymbols.readNat()
            val tsr = TermSymbolRep(ownerId, name, paramssSize, erasedRetId)
            tsr.erasedParams = paramRefIds
            tsr
        }
      }

      def deserializeSymbols: mutable.Map[Int, RefRep] = {
        val list = serializedSymbols.until(serializedSymbols.bytes.size,
          () => {
            val ind = serializedSymbols.readIndex
            val ref = readRef
            (ind, ref)
          }
        )
        mutable.Map(list: _*)
      }

      val refs = deserializeSymbols

      println("====================")
      println(s"refs: $refs")
//      println
//      println(s"tree: ${showRaw(res)}")
//      println
//      println(s"tree: ${res}")
    }

    def init(unit: Tree): Unit = {
    }

    val serializedSymbols = new PickleBuffer(new Array[Byte](1024), 0, 0)

    def getSymbolById(id: Int): Symbol = {
      ???
    }

//    def deserializeSymbol(nd: ArrayBuffer[Byte]): Symbol = {
//      val id = (nd: @unchecked) match {
//        case NoSymbol => 0
//        case _ if nd.isRoot => 1 //TODO-check isRoot for NoSymbol and RootClass equality (should be false, true)
//        //Local
//        //TODO - rewrite !sym.isLocatable
//        case sym: Symbol if !sym.isLocatable => 2
//        //External Type
//        case sym: Symbol if sym.isType => 3
//        //External Term
//        case sym: Symbol if sym.isTerm => 4
//        case sym: Symbol if sym.hasPackageFlag => 5
//        //case array => 6 //TODO implement for Array
//      }
//      serializedSymbols.writeByte(id)
//    }

    //TODO implement modifiers rewriting
    def readModifiersId(mod: Modifiers, idx: Int): Unit = ???

    //get during deserialization
    val strings: mutable.Map[Int, String] = new mutable.HashMap[Int, String]()

    //TODO - method to get string from strings Map[Offset, String]
    def getStringById(id: Int): String = strings(id)

    //get during deserialization
    val constants: mutable.Map[Int, Constant] = new mutable.HashMap[Int, Constant]()

    //TODO - method to get constant from constants Map[Offset, Constant]
    def getConstantById(id: Int): Constant = {
//      println(s"id: $id")
//      println(s"constants.contains(id): ${constants.contains(id)}")
      val res = constants.apply(id)
//      println(s"res: $res")
      res
    }

    //from trees
    def readBoolean(pb: PickleBuffer): Boolean = {
      val value = pb.readByte()
      if (value > 0) true else false
    }

    //from trees
    def readArgsCount(pb: PickleBuffer): Int = {
      pb.readNat()
    }
  }

}
