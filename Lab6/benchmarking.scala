import org.scalameter._

object Benchmarking {
  type Series = List[Double]
  type AlghoritmStats = List[Series]

  def compareBenchmark[A,B](alghoritms: List[A => Unit])(generator: B => A)(params: B)(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats]= {
    (0 until numberOfTestSets).toList.map ( _ => {
      val testSet = generator(params)
      alghoritms.map(alghoritm => trueMeasurement(alghoritm)(repeatsPertestSet)(testSet))
    })
  }
  def benchmark[A,B](alghoritm: A => Unit)(generator: B => A)(params: B)(numberOfTestSets: Int)(repeatsPertestSet: Int): AlghoritmStats = {
    (0 until numberOfTestSets).toList.map ( _ => {
      trueMeasurement(alghoritm)(repeatsPertestSet)(generator(params))
    })
  }

  def compareBenchmarkArray[A, B](alghoritms: List[Array[A] => Unit])(generator: B => Array[A])(params: B)(numberOfTestSets: Int)(repeatsPertestSet: Int): List[AlghoritmStats]= {
    (0 until numberOfTestSets).toList.map ( _ => {
      val testSet = generator(params)
      alghoritms.map(alghoritm => trueMeasurementArray(alghoritm)(repeatsPertestSet)(testSet))
    })
  }

  def benchmarkArray[A, B](alghoritm: Array[A] => Unit)(generator: B => Array[A])(params: B)(numberOfTestSets: Int)(repeatsPertestSet: Int): AlghoritmStats= {
    (0 until numberOfTestSets).toList.map ( _ => {
      trueMeasurementArray(alghoritm)(repeatsPertestSet)(generator(params))
    })
  }

  def trueMeasurementArray[A](alghoritm: Array[A] => Unit)(repeatsPertestSet: Int)(testsSet: Array[A]): List[Double] = {
    trueMeasurement((a: Array[A]) => {
      val copy = a.clone()
      alghoritm(copy)
    })(repeatsPertestSet)(testsSet)
  }

  def trueMeasurement[A](alghoritm: A => Unit)(repeatsPertestSet: Int)(testsSet: A): List[Double] = {
    (0 until repeatsPertestSet).toList.map(_ => (
        config(
          Key.exec.minWarmupRuns -> 20,
          Key.verbose -> false
        )
        withWarmer new Warmer.Default
        withMeasurer new Measurer.IgnoringGC
        measure alghoritm(testsSet)
      ).value
    )
  }

  implicit class RichListDouble(list: List[Double]) {
    def sum: Double = list.foldLeft(0.0)((sum: Double, el: Double) => {
      sum + el
    })

    def mean: Double = sum / list.length

    def variance: Double = list.map((el: Double) => el * el).mean - list.mean * list.mean
  }
}
