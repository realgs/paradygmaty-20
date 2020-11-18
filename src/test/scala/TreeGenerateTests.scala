import org.scalatest.FunSuite

class TreeGenerateTests extends FunSuite {
  var bt1 = BinaryTreesTasks.tree_generate(3, 2, 10)
  var bt2 = BinaryTreesTasks.tree_generate(5, 4, 20)
  var bt3 = BinaryTreesTasks.tree_generate(10, 10, 100)
  var bt4 = BinaryTreesTasks.tree_generate(0, 2, 29)
  var bt5 = BinaryTreesTasks.tree_generate(3, 2, 10)
  var bt6 = BinaryTreesTasks.tree_generate(5, 5, 14)
  var bt7 = BinaryTreesTasks.tree_generate(0, 7, 23)

  // tests for first task
  test("case of Exeption during generating binary tree"){
    assertThrows[Exception](BinaryTreesTasks.tree_generate(-6 ,9, 21))
  }

  test("case of creating Empty tree if deep is equal 0 "){
    assert(BinaryTreesTasks.tree_generate(0, 7, 20) == Empty)
  }

  test("check the deep of tree"){
    assert(BinaryTreesTasks.checkDeep(bt1) == 3)
    assert(BinaryTreesTasks.checkDeep(bt2) == 5)
    assert(BinaryTreesTasks.checkDeep(bt4) == 0)
  }

  test("check if tree is full"){
    assert(BinaryTreesTasks.isFullTree(bt1))
    assert(BinaryTreesTasks.isFullTree(bt2))
    assert(BinaryTreesTasks.isFullTree(bt3))
    assert(BinaryTreesTasks.isFullTree(bt4))
  }

   //tests for second task
  test("checking function of tree substraction"){
    println("1 tree: " + bt1)
    println("2 tree: " + bt5)
    println("result: " + BinaryTreesTasks.subtractTrees(bt1, bt5))
    println("-----------------------")
    println("1 tree: " + bt2)
    println("2 tree: " + bt6)
    println("result: " + BinaryTreesTasks.subtractTrees(bt2, bt6))

    assert(BinaryTreesTasks.subtractTrees(bt4, bt7) == Empty)
  }
}
