import scala.annotation.tailrec

object Task4 {
    def contains(string: String, key: String): Boolean = {
        def innerContains(string: String, key: String, tailOfKey: Boolean): Boolean =
            (string, key) match {
                case (_, "") => true
                case ("", _) => false
                case _ =>  if (string.head == key.head &&
                    innerContains(string.tail, key.tail, true)) true
                    else if (tailOfKey) false 
                    else innerContains(string.tail, key, false)
            }
        innerContains(string, key, false)
    }    

    @tailrec
    def containsAny(string: String, keys: List[String]): Boolean =
        keys match {
            case Nil => false
            case hd::tl => if (contains(string, hd)) true
                else containsAny(string, tl)
        }

    def findWithTailRec(list: List[String], key: String): List[String] = {
        @tailrec
        def findTail(list: List[String], key: String, accum: List[String]): List[String] =
            list match {
                case Nil => accum
                case hd::tl =>  
                    findTail(tl, key, if (contains(hd, key)) hd::accum else accum)
            }
        Utils.reverse(findTail(list, key, Nil))
    }

    def findWithTailRec(list: List[String], keys: List[String]): List[String] = {
        @tailrec
        def findTail(list: List[String], keys: List[String], accum: List[String]): List[String] =
            list match {
                case Nil => accum
                case hd::tl =>
                    findTail(tl, keys, if (containsAny(hd, keys)) hd::accum else accum)
            }
        Utils.reverse(findTail(list, keys, Nil))
    }

    def findWithoutTailRec(list: List[String], key: String): List[String] =
        list match {
            case Nil => Nil
            case hd::tl => if (contains(hd, key)) 
                hd::findWithoutTailRec(tl, key)
                else findWithoutTailRec(tl, key)
        }

    def findWithoutTailRec(list: List[String], keys: List[String]): List[String] =
        list match {
            case Nil => Nil
            case hd::tl => if (containsAny(hd, keys)) 
                hd::findWithoutTailRec(tl, keys)
                else findWithoutTailRec(tl, keys)
        }
}
