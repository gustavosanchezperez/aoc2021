package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day16 {
  case class PktHdr(ver: Int, typ: Int) {}

  case class Packet(hdr: PktHdr, data: String) {}

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
        (Packet(hdr, nextdata.slice(0, size)), size)
      case PktHdr(_, op) =>
        if (nextdata.length < 12) (Packet(hdr, ""), nextdata.length)
        else nextdata.take(1) match {
          case "0" =>
            val size = Integer.parseInt(nextdata.slice(1, 16), 2)
            (Packet(hdr, nextdata.slice(0, 16 + size)), 16)
          case "1" =>
            val size = Integer.parseInt(nextdata.slice(1, 12), 2)
            (Packet(hdr, ""), 12)
        }
    }
    (pkt, nextdata.drop(sz))
  }

  def process(bin: String): List[Packet] = {
    def loop(binstr: String, packets: List[Packet]): List[Packet] = {
      val (packet, nextdata) = extractNextPacket(binstr)
      if (nextdata.length > 6) loop(nextdata, List(packet) ::: packets)
      else List(packet) ::: packets
    }

    loop(bin, List())
  }

  def asBinaryString(hexa: String): String = {
    hexa.toList.map(c => Integer.parseInt(c.toString, 16)).foldLeft("")((packet, n) => {
      val asbin = n.toBinaryString
      packet.concat("0" * (4 - asbin.length) + asbin)
    })
  }

  def part1(pktlist: List[Packet]): Int = pktlist.foldLeft(0)((acc, pkt) => acc + pkt.hdr.ver)

  def main(args: Array[String]): Unit = {
    val hexa = read("day16.txt").head
    val binary = asBinaryString(hexa)
    assert(hexa.length * 4 == binary.length)
    println(binary)

    val decoded = process(binary)
    println(s"Sum versions: ${part1(decoded)}")

  }
}
