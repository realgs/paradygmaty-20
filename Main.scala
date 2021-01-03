import lab6.SpeedTests._
object Main extends App {
  runSpeedTestsQuickSort()
  runSpeedTestsMatrix()
  runSpeedTestsPattern()
  /*
    QuickSort Speed Tests
 Quicksort Array Size 100
   seqTime 1.0
   parTime 106.0
   differ -105.0 ratio 0.009433962264150943
 Quicksort Array Size 100000
   seqTime 41.0
   parTime 4.0
   differ 37.0 ratio 10.25
 Quicksort Array Size 10000000
   seqTime 2139.0
   parTime 64.0
   differ 2075.0 ratio 33.421875

Matrix Speed Tests
 Matrix size 100x100
   seqTime 51.0
   parTime 237.0
   differ -186.0 ratio 0.21518987341772153
 Matrix size 500x500
   seqTime 484.0
   parTime 244.0
   differ 240.0 ratio 1.9836065573770492
 Matrix size 1000x1000
   seqTime 5349.0
   parTime 4263.0
   differ 1086.0 ratio 1.254750175932442

Find Pattern Speed Tests
 Array size 10000 Number of patterns 100
   seqTime 204.0
   parTime 134.0
   differ 70.0 ratio 1.5223880597014925
 Array size 1000000 Number of patterns 100
   seqTime 7686.0
   parTime 1493.0
   differ 6193.0 ratio 5.148024112525118
 Array size 10000 Number of patterns 300
   seqTime 207.0
   parTime 46.0
   differ 161.0 ratio 4.5
 Array size 1000000 Number of patterns 300
   seqTime 15712.0
   parTime 3401.0
   differ 12311.0 ratio 4.619817700676272
   */
}
