

object TestTree {
     def main(args: Array[String]) : Unit = {
     
     
     println("size")
     println(Tree.size(Leaf(1)))
     println(Tree.size(Branch(Leaf(1), Leaf(2))))
     
     println("maximum")
     println(Tree.maximum(Leaf(1)))
     println(Tree.maximum(Branch(Leaf(1), Leaf(2))))
     
     println("depth")
     println(Tree.depth(Leaf(1)))
     println(Tree.depth(Branch(Leaf(1), Leaf(2))))
     println(Tree.depth(Branch(Leaf(1), Branch(Leaf(10),Leaf(2)))))
     
     
     
     println("sizeF")
     println(Tree.sizeF(Leaf(1)))
     println(Tree.sizeF(Branch(Leaf(1), Leaf(2))))
     
     println("maximumF")
     println(Tree.maximumF(Leaf(1)))
     println(Tree.maximumF(Branch(Leaf(1), Leaf(2))))
     
     println("depthF")
     println(Tree.depthF(Leaf(1)))
     println(Tree.depthF(Branch(Leaf(1), Leaf(2))))
     println(Tree.depthF(Branch(Leaf(1), Branch(Leaf(10),Leaf(2)))))
   }
}