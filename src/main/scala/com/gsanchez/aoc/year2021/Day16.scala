package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day16 {
  case class PktHdr(ver: Int, typ: Int) {}

  abstract class Packet(val hdr: PktHdr) {
    def apply(): BigInt = ???

    def version(): Int = ???
  }

  case class PacketLiteral(override val hdr: PktHdr, data: String) extends Packet(hdr) {
    val literal = {
      val str = data.grouped(5).foldLeft("")((num, d) => num.concat(d.substring(1)))
      BigInt.apply(str, 2)
    }

    override def apply(): BigInt = literal

    override def version(): Int = hdr.ver
  }

  case class PacketOp(override val hdr: PktHdr, pktList: List[Packet]) extends Packet(hdr) {
    override def version(): Int = hdr.ver + pktList.map(_.version()).sum

    override def apply(): BigInt = {
      hdr.typ match {
        case 0 => pktList.foldLeft(BigInt(0))((acc, pkt) => acc + pkt())
        case 1 => pktList.foldLeft(BigInt(1))((acc, pkt) => acc * pkt())
        case 2 => pktList.map(_ ()).min
        case 3 => pktList.map(_ ()).max
        case 5 => if (pktList.head() > pktList.last()) 1 else 0
        case 6 => if (pktList.head() < pktList.last()) 1 else 0
        case 7 => if (pktList.head() == pktList.last()) 1 else 0
        case _ =>
          assert(1 == 0)
          -1
      }
    }
  }

  def pktHeader(hdr: String): PktHdr = {
    val (v, h) = hdr.splitAt(3)
    PktHdr(Integer.parseInt(v, 2), Integer.parseInt(h, 2))
  }

  def extractNextPacket(bin: String): (Packet, String) = {
    val hdr = pktHeader(bin.take(6))
    val nextdata = bin.drop(6)
    val (pkt, sz) = hdr match {
      case PktHdr(_, 4) =>
        val pktData = nextdata.sliding(5, 5).takeWhile(lit => lit.head == '1').toList
        val size = 5 * (pktData.length + 1)
        (PacketLiteral(hdr, nextdata.slice(0, size)), size)
      case PktHdr(_, op) =>
        if (nextdata.length < 12) (PacketOp(hdr, List()), nextdata.length)
        else nextdata.take(1) match {
          case "0" =>
            val size = Integer.parseInt(nextdata.slice(1, 16), 2)
            (PacketOp(hdr, process(nextdata.slice(16, 16 + size))), 16 + size)
          case "1" =>
            val size = Integer.parseInt(nextdata.slice(1, 12), 2)
            val subPkts = (1 to size).foldLeft((nextdata.drop(12), List[Packet]()))((pkl, _) => {
              val nn = extractNextPacket(pkl._1)
              (nn._2, pkl._2 ::: List(nn._1))
            })
            (PacketOp(hdr, subPkts._2), nextdata.length - subPkts._1.length)
        }
    }
    (pkt, nextdata.drop(sz))
  }

  def process(bin: String): List[Packet] = {
    def loop(binstr: String, packets: List[Packet]): List[Packet] = {
      val (packet, nextdata) = extractNextPacket(binstr)
      if (nextdata.length > 6) loop(nextdata, packets ::: List(packet))
      else packets ::: List(packet)
    }

    loop(bin, List())
  }

  def asBinaryString(hexa: String): String = {
    hexa.toList.map(c => Integer.parseInt(c.toString, 16)).foldLeft("")((packet, n) => {
      val asbin = n.toBinaryString
      packet.concat("0" * (4 - asbin.length) + asbin)
    })
  }

  def part1(pkt: Packet): Int = pkt.version()

  def part2(pkt: Packet): BigInt = pkt()

  def main(args: Array[String]): Unit = {
    val hexa = read("day16.txt").head
    val binary = asBinaryString(hexa)
    assert(hexa.length * 4 == binary.length)

    val (decoded, _) = extractNextPacket(binary)
    println(s"Sum versions: ${part1(decoded)}")
    println(s"BITS: ${part2(decoded)}")
  }
}
