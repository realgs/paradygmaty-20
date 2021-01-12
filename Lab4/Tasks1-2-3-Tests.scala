object Tests123 {
    def main(): Unit = {
        val tree1 = Tasks123.generateNDepthTree(4, 0, 5)
        println(Tasks123.count(tree1) == 15)
        val list1 = Tasks123.breadth(tree1)
        println(list1.filter(e => e < 0) == List())
        println(list1.filter(e => e > 5) == List())
        try {Tasks123.generateNDepthTree(7, 4, 2)} 
        catch {case e : Throwable => println(e.getMessage() == "min cannot be greater than max")}

        try {Tasks123.generateNDepthTree(-7, 4, 2)} 
        catch {case e : Throwable => println(e.getMessage() == "N cannot be negative")}

        val tree2 = Tasks123.generateNDepthTree(3,1,2)
        println(Tasks123.breadth(Tasks123.subtractElements(tree2, tree2)) == List(0,0,0,0,0,0,0))
        try {Tasks123.subtractElements(tree1, tree2)} 
        catch {case e : Throwable => println(e.getMessage() == "trees must match to themselves")}
    }
}
