import Tests.{longestCommonPrefixTest, matrixProductTest, mergeSortTest}
object Main extends App {
  /* Przetwarzanie równoległe pozwala osiągnąć spory zysk czasowy,ale tylko w sytuacji gdy zadanie do wykonania równoległego jest odpowiedniej wielkości.
  Dla niewielkiej ilości danych przetwarzanie równoległe może spowolnić obliczenia. */
  mergeSortTest()
  matrixProductTest()
  longestCommonPrefixTest()
}
