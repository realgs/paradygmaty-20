
object BinarySearchParallel {

  def containsParallel(arr: Array[Int], elem: Int): Boolean={
    if(arr.size==0) false
    else searchParallel(arr, 0, arr.size-1, elem)
  }

  def searchParallel(arr: Array[Int], begin: Int, end: Int, elem: Int): Boolean={
    if(end-begin==0){
      if(arr(begin)==elem) true
      else false
    }else{
      val mid=(begin+end)/2;
      val (left, right)=ParallelUtil.parallel(searchParallel(arr, begin, mid, elem), searchParallel(arr, mid+1, end, elem))
      left || right
    }
  }

}
