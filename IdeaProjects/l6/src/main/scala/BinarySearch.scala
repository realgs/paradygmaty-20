object BinarySearch {

  def contains(arr: Array[Int], elem: Int): Boolean={
    if(arr.length==0) false
    else search(arr, 0, arr.length-1, elem)
  }

  def search(arr: Array[Int], begin: Int, end: Int, elem: Int): Boolean={
    if(end-begin==0){
      if(arr(begin)==elem) true
      else false
    }else{
      val mid=(begin+end)/2;
      val left=search(arr, begin, mid, elem)
      val right=search(arr, mid+1, end, elem)
      left || right
    }
  }

  def containsParallel(arr: Array[Int], elem: Int): Boolean={
    if(arr.length==0) false
    else searchParallel(arr, 0, arr.length-1, elem)
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
