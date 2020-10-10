object SortingMethods {

  def main(args:Array[String]): Unit ={
    BubbleSort(Array(6,5,34,2,1));
    SelectionSort(Array(6,5,34,2,1))
  }

  def BubbleSort(array:Array[Int]):Array[Int]={
    for(i<- 0 to array.length-2){
      for(j<- 0 to array.length-2){
        if(array(j)>array(j+1)){
          val temp = array(j);
          array(j)=array(j+1);
          array(j+1)=temp;
        }
      }
    }
    array;
  }

  def SelectionSort(array:Array[Int]):Array[Int]={
    for(i<-0 to array.length-1){
      var smallest_index=i;
      for(j<-i to array.length-1){
        if(array(smallest_index)>array(j)){
          smallest_index=j;
        }
      }
      val temp = array(i);
      array(i)=array(smallest_index);
      array(smallest_index)=temp;
    }
    array;
  }
}
