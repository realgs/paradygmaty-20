object BinarySearch {

  def contains(arr: Array[Int], elem: Int): Boolean={
    if(arr.size==0) false
    else search(arr, 0, arr.size-1, elem)
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

}
