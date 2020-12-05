import scala.collection.mutable

class DuplicateCollection {

  /*1) Napisz funkcję powielającą elementy w kolekcji na podstawie drugiej kolekcji
   określającej ile razy elementy mają być powielone.

   przykład: duplicate [1;2;3] oraz [0;3;1;4] daje wynik [2;2;2;3]
   Punkty: 2.5*/

   def duplicate [A](queue: mutable.Queue[A], countQueue: mutable.Queue[Int]): mutable.Queue[A] = {
      val result = mutable.Queue[A]()
      @scala.annotation.tailrec
      def helper (q: mutable.Queue[A], counter: mutable.Queue[Int], res: mutable.Queue[A]): mutable.Queue[A] = {
         (q.isEmpty, counter.isEmpty) match {
            case (false, false) =>
               var count = counter.dequeue()
               count match {
                  case 0 =>
                     q.dequeue()
                     helper(q, counter, res)
                  case _ =>
                     while (count != 0) {
                        res.enqueue(q.front)
                        count = count - 1
                     }
                     q.dequeue()
                     helper(q, counter, res)
               }
            case (_, _) => res
         }
      }
      helper(queue, countQueue, result)
   }



  /*2) Przepisz funkcję z zadania pierwszego tak, by pierwszy zbiór wejściowy nie mógł przyjmować duplikatów.
     Punkty: 2.5*/

   def duplicateSet [A](set: Set[A], countQueue: mutable.Queue[Int]): mutable.Queue[A] = {
      val result = mutable.Queue[A]()
      @scala.annotation.tailrec
      def helper (s: Set[A], counter: mutable.Queue[Int], res: mutable.Queue[A]): mutable.Queue[A] = {
         (s.isEmpty, counter.isEmpty) match {
            case (false, false) =>
               var count = counter.dequeue()
               count match {
                  case 0 =>
                     helper(s.tail, counter, res)
                  case _ =>
                     while (count != 0) {
                        res.enqueue(s.head)
                        count = count - 1
                     }
                     helper(s.tail, counter, res)
               }
            case (_, _) => res
         }
      }
      helper(set, countQueue, result)
   }


}
