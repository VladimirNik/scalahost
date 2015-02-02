package scala.meta.internal.hosts.scalac.convert

import scala.reflect.internal.pickling.PickleBuffer

trait SerializerUtils {
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

    def readRawLong: Long = {
      //TODO rewrite!!!
      val l56 = (pb.readByte() & 0xFF).toLong << 56
      val l48 = (pb.readByte() & 0xFF).toLong << 48
      val l40 = (pb.readByte() & 0xFF).toLong << 40
      val l32 = (pb.readByte() & 0xFF).toLong << 32
      val l24 = (pb.readByte() & 0xFF).toLong << 24
      val l16 = (pb.readByte() & 0xFF).toLong << 16
      val l8 = (pb.readByte() & 0xFF).toLong << 8
      val l0 = (pb.readByte() & 0xFF).toLong << 0
      l56 + l48 + l40 + l32 + l24 + l16 + l8 + l0
    }

    def readRawInt: Int = {
      //TODO rewrite!!!
      val i24 = (pb.readByte() & 0xFF).toInt << 24
      val i16 = (pb.readByte() & 0xFF).toInt << 16
      val i8 = (pb.readByte() & 0xFF).toInt << 8
      val i0 = (pb.readByte() & 0xFF).toInt << 0
      i24 + i16 + i8 + i0
    }

    /** append bytes bts[start..start + len - 1] */
    def writeBytes(bts: Array[Byte], start: Int, len: Int): Unit = {
      if(len > 0) {
        pb.ensureCapacity(len)
        //def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int)
        Array.copy(bts, start, pb.bytes, pb.writeIndex, len)
        pb.writeIndex += len
      }
    }

    def writeBytes(bts: Array[Byte]): Unit = writeBytes(bts, 0, bts.length)

    def readBytes(start: Int, len: Int): Array[Byte] = {
      pb.times(len - start, () => pb.readByte().toByte).toArray
    }

    def readBytes(num: Int): Array[Byte] = {
      readBytes(0, num)
    }
  }
}
