import scala.annotation.tailrec

object Task4WithTailRec {
    def containsTail(string: String, keyTail: String, key: String): Boolean =
        (string, keyTail, key) match {
            case (_, "", _) => true
            case ("", _, _) => false
            case _ =>  if (string.head == keyTail.head &&
                containsTail(string.tail, keyTail.tail, key)) true
                else containsTail(string.tail, key, key)
        }

    def contains(string: String, key: String): Boolean =
    containsTail(string, key, key)

    @tailrec
    def containsAny(string: String, keys: List[String]): Boolean =
    (string: String, keys: List[String]) match {
        case (_, Nil) => false
        case(_, hd::tl) => if (contains(string, hd)) true
            else containsAny(string, tl)
    }

    def find(list: List[String], key: String): List[String] =
        reverse(findTail(list, key, Nil))

    @tailrec
    def findTail(list: List[String], key: String, accum: List[String]): List[String] =
        (list, key) match {
            case (Nil, _) => accum
            case(hd::tl, _) =>  
                findTail(tl, key, if (contains(hd, key)) hd::accum else accum)
        }

    def find(list: List[String], keys: List[String]): List[String] =
        reverse(findTail(list, keys, Nil))

    @tailrec
    def findTail(list: List[String], keys: List[String], accum: List[String]): List[String] =
        (list, keys) match {
            case (Nil, _) => accum
            case(hd::tl, _) =>
                findTail(tl, keys, if (containsAny(hd, keys)) hd::accum else accum)
        }
    
    def reverse[A](list: List[A]): List[A] = {
        @tailrec
        def reverseTail[A](list: List[A], reversed: List[A]): List[A] =
            list match {
                case Nil => reversed
                case hd::tl => reverseTail(tl, hd::reversed)
            }
        reverseTail(list, Nil)
    }

}