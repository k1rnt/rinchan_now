package com.github.k1rnt.rinchan_now

class RinchanNow {
  def run(program: String): Unit = {
    val tape = Array.fill[Byte](30000)(0)
    var pointer = 0

    def next(): Unit = pointer += 1
    def prev(): Unit = pointer -= 1
    def incr(): Unit = tape(pointer) = (tape(pointer) + 1).toByte
    def decr(): Unit = tape(pointer) = (tape(pointer) - 1).toByte
    def putchar(): Unit = print(tape(pointer).toChar)
    def getchar(): Unit = tape(pointer) = Console.in.read().toByte

    var i = 0
    while (i < program.length) {
      program(i) match {
        case 'リ' => next()
        case 'ン' => prev()
        case 'ち' => incr()
        case 'ゃ' => decr()
        case 'ん' => putchar()
        case 'な' => getchar()
        case 'う' =>
          if (tape(pointer) == 0) {
            var loop = 1
            while (loop > 0) {
              i += 1
              if (program(i) == 'う') loop += 1
              if (program(i) == '！') loop -= 1
            }
          }
        case '！' =>
          if (tape(pointer) != 0) {
            var loop = 1
            while (loop > 0) {
              i -= 1
              if (program(i) == 'う') loop -= 1
              if (program(i) == '！') loop += 1
            }
            i -= 1
          }
        case _ =>
      }
      i += 1
    }
  }
}
