import scala.util.Random

object l6_TreeFull_TimeTests extends App {
  println("Depth: 3")
  val tree_1 = L6_tree.generateTree(3)(0)(10)
  l6.compareNanoTime{L6_tree.isTreeFull(tree_1)}{L6_tree.isTreeFullPar(tree_1)}

  println("Depth: 10")
  val tree_2 = L6_tree.generateTree(10)(0)(1000)
  l6.compareNanoTime{L6_tree.isTreeFull(tree_2)}{L6_tree.isTreeFullPar(tree_2)}

  println("Depth: 20")
  val tree_3 = L6_tree.generateTree(20)(0)(1000)
  l6.compareNanoTime{L6_tree.isTreeFull(tree_3)}{L6_tree.isTreeFullPar(tree_3)}
}
/*Depth: 3
Time NotParallel: 44800 nanoSeconds
Time Parallel:    107849100 nanoSeconds
Ratio: 0.0
Differ: -107804300 nanoSeconds
Depth: 10
Time NotParallel: 2266700 nanoSeconds
Time Parallel:    32839300 nanoSeconds
Ratio: 0.069
Differ: -30572600 nanoSeconds
Depth: 20
Time NotParallel: 110388100 nanoSeconds
Time Parallel:    1352257500 nanoSeconds
Ratio: 0.081
Differ: -1241869400 nanoSeconds*/


object l6_TreeDepth_TimeTests extends App {
  println("Depth: 3")
  val tree_1 = L6_tree.generateTree(3)(0)(10)
  l6.compareNanoTime{L6_tree.treeDepth(tree_1)}{L6_tree.treeDepthPar(tree_1)}

  println("Depth: 10")
  val tree_2 = L6_tree.generateTree(10)(0)(1000)
  l6.compareNanoTime{L6_tree.treeDepth(tree_2)}{L6_tree.treeDepthPar(tree_2)}

  println("Depth: 20")
  val tree_3 = L6_tree.generateTree(20)(0)(1000)
  l6.compareNanoTime{L6_tree.treeDepth(tree_3)}{L6_tree.treeDepthPar(tree_3)}
}
/*Depth: 3
Time NotParallel: 42200 nanoSeconds
Time Parallel:    112234200 nanoSeconds
Ratio: 0.0
Differ: -112192000 nanoSeconds
Depth: 10
Time NotParallel: 271000 nanoSeconds
Time Parallel:    41348000 nanoSeconds
Ratio: 0.006
Differ: -41077000 nanoSeconds
Depth: 20
Time NotParallel: 8951000 nanoSeconds
Time Parallel:    1972502000 nanoSeconds
Ratio: 0.004
Differ: -1963551000 nanoSeconds*/

/*Analiza:
Dużą liczba podziałów (rozgałęzień) pociąga ze soba stałe koszty,
których nie udaje się zrównoważyć poprzez zrównoleglenie obliczeń
*/

object l6_quicksort_TimeTests extends App {
  val r = new Random
  val array_1 = Array.fill(1000)(r.nextInt(10000))
  val array_2 = Array.fill(100000)(r.nextInt(1000000))
  val array_3 = Array.fill(10000000)(r.nextInt(100000000))

  l6.compareNanoTime{L6_quicksort.quicksort(array_1.clone())}{L6_quicksort.quicksortPar(array_1.clone())}

  l6.compareNanoTime{L6_quicksort.quicksort(array_2.clone())}{L6_quicksort.quicksortPar(array_2.clone())}

  l6.compareNanoTime{L6_quicksort.quicksort(array_3.clone())}{L6_quicksort.quicksortPar(array_3.clone())}
}

/*Time NotParallel: 3031900 nanoSeconds
Time Parallel:    65429600 nanoSeconds
Ratio: 0.046
Differ: -62397700 nanoSeconds
Time NotParallel: 30088400 nanoSeconds
Time Parallel:    1293500 nanoSeconds
Ratio: 23.261
Differ: 28794900 nanoSeconds
Time NotParallel: 1810987800 nanoSeconds
Time Parallel:    56435200 nanoSeconds
Ratio: 32.089
Differ: 1754552600 nanoSeconds*/

/*Analiza:
Wraz ze zwiększaniem ilości sortowanych elementów zrównoleglenie daje coraz lepsze rezultaty,
*/

object l6_findpattern_TimeTests extends App {
  val list_0 = List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")
  l6.compareNanoTime{ L6_pattern.find(List("index0168", "index0169224"), list_0) }{ L6_pattern.findPar(List("index0168", "index0169224"), list_0) }

  val r = new Random
  val list_1 = Array.fill(1000)(r.nextInt(50)).toList
  l6.compareNanoTime{ L6_pattern.find(List(1, 2, 3), list_1) }{ L6_pattern.findPar(List(1, 2, 3), list_1) }

  val list_2 = Array.fill(10000)(r.nextInt(50)).toList
  l6.compareNanoTime{ L6_pattern.find(List(11, 29, 47), list_2) }{ L6_pattern.findPar(List(11, 29, 47), list_2) }
}

/*Time NotParallel: 100113000 nanoSeconds
Time Parallel:    104744300 nanoSeconds
Ratio: 0.955
Differ: -4631300 nanoSeconds
Time NotParallel: 15951200 nanoSeconds
Time Parallel:    10971100 nanoSeconds
Ratio: 1.453
Differ: 4980100 nanoSeconds
Time NotParallel: 47797300 nanoSeconds
Time Parallel:    27416600 nanoSeconds
Ratio: 1.743
Differ: 20380700 nanoSeconds*/

/*Analiza:
Wyszukiwanie wzorca odbywa się równolegle na dwóch połowach listy w której szukamy,
więc zrównoleglenie zapewni przyspieszenie do dwóch razy (przyspieszenie poprzez zrównoleglenie - czas rozgałęziania)
*/