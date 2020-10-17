
public class L1 {

	public static void main(String[] args) {
		int []array = {1,8,10,1,5,14,47,1,9,4,3,8,7,1};
	
		for (int i =0;i<insertionSort(array).length;i++) {
			
			System.out.print(insertionSort(array)[i]+ "  ");
		}
		}
	

	
	
	public   int[] bubbleSort(int[]array) {
		
		for (int i=0;i<array.length;i++) {
			
			
			for (int j=1;j<(array.length-i);j++) {
				
				if (array[j-1]>array[j]) {
					
					int valueSaver = array[j-1];
					
					array[j-1]=array[j];
					array[j]= valueSaver ;
				}
				
			}
		}
		return array;
		
	}
	
	 public static int[] insertionSort(int array[]) 
		{  
	        int n = array.length;  
	        
	        for (int i = 1; i < n; i++)
	        {   
	            int key = array[i];  
	            int j = i-1;  
	            
	            while ( (j > -1) && ( array [j] > key ) ) {
	            
	             
	                array [j+1] = array [j];  
	                j--;  
	            }  
	            array[j+1] = key; 
	       
	        }  
	        return array;
	    }
	
	

}