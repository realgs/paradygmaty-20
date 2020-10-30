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
        (list, key) match {
            case (Nil, _) => Nil
            case(hd::tl, _) => if (contains(hd, key)) 
                hd::find(tl, key)
                else find(tl, key)
        }

    def find(list: List[String], keys: List[String]): List[String] =
        (list, keys) match {
            case (Nil, _) => Nil
            case(hd::tl, _) => if (containsAny(hd, keys)) 
                hd::find(tl, keys)
                else find(tl, keys)
        }
}