import scala.annotation.tailrec
import Utils.reverse

object Task4WithTailRec {
    def contains(string: String, key: String): Boolean =
        (string, key) match {
            case (_, "") => true
            case ("", _) => false
            case _ =>  if (string.head == key.head &&
                contains(string.tail, key.tail)) true
                else contains(string.tail, key)
        }

    @tailrec
    def containsAny(string: String, keys: List[String]): Boolean =
        (string: String, keys: List[String]) match {
            case (_, Nil) => false
            case(_, hd::tl) => if (contains(string, hd)) true
                else containsAny(string, tl)
        }

    def findWithTailRec(list: List[String], key: String): List[String] = {
        @tailrec
        def findTail(list: List[String], key: String, accum: List[String]): List[String] =
            (list, key) match {
                case (Nil, _) => accum
                case(hd::tl, _) =>  
                    findTail(tl, key, if (contains(hd, key)) hd::accum else accum)
            }
        reverse(findTail(list, key, Nil))
    }

    def findWithTailRec(list: List[String], keys: List[String]): List[String] = {
        @tailrec
        def findTail(list: List[String], keys: List[String], accum: List[String]): List[String] =
            (list, keys) match {
                case (Nil, _) => accum
                case(hd::tl, _) =>
                    findTail(tl, keys, if (containsAny(hd, keys)) hd::accum else accum)
            }
        reverse(findTail(list, keys, Nil))
    }

    def findWithoutTailRec(list: List[String], key: String): List[String] =
        (list, key) match {
            case (Nil, _) => Nil
            case(hd::tl, _) => if (contains(hd, key)) 
                hd::find(tl, key)
                else find(tl, key)
        }

    def findWithoutTailRec(list: List[String], keys: List[String]): List[String] =
        (list, keys) match {
            case (Nil, _) => Nil
            case(hd::tl, _) => if (containsAny(hd, keys)) 
                hd::find(tl, keys)
                else find(tl, keys)
        }
}
