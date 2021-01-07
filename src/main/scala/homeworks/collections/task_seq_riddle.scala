package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
//    task"Реализуйте функцию генерирующую след последовательность из текущей"()
    currentLine.foldLeft(List.empty[Int]) {
      (acc, currentInt) => acc match {
        case h :: count :: rest if currentInt == h => currentInt :: (count + 1) :: rest
        case _ => currentInt :: 1 :: acc
      }
    }.reverse

  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = LazyList.cons(hd = List(1), tl = funSeq.map(nextLine))
//    task"Реализуйте ленивый список, который генерирует данную последовательность"()
}