package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
//    task"Реализуйте метод `encrypt`"()
    val numFirstCharacter = 65
    val numLastCharacter = 90
    val countAlphabet = 26
    val off = {
      if (offset.abs > numLastCharacter) offset % countAlphabet else offset
    }

    word.toList
      .map(_.toLong + off)
      .collect {
        case x if x < numFirstCharacter => x + countAlphabet
        case x if x > numLastCharacter => x - countAlphabet
        case x => x
      }
      .map(_.toInt.toChar)
      .mkString
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
//    task"Реализуйте метод `decrypt`"()
    encrypt(cipher, -offset)
  }

}
