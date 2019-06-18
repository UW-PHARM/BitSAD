// import bitstream.compiler.NodeHandlers._

object MainSpec {
  val TEST_CODE = 7
  def main(args: Array[String]) {

    // TEST_CODE match {
    //   case 0 => {
    //     // Test AddHandler
    //     var addHandler = new AddHandler()

    //     var (netList, outputString1) =
    //       addHandler.create(
    //         List("a", "b"),
    //         "c",
    //         List((2, 2), (2, 2)),
    //         Map())
    //     var outputString2 = ""
    //     addHandler.create(
    //       List("a", "b"),
    //       "d",
    //       List((2, 2), (2, 2), (2, 2)),
    //       netList) match {
    //         case (map, str) => {
    //           netList = map
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 1 => {
    //     // Test SubHandler
    //     var subHandler = new SubHandler()

    //     var (netList, outputString1) =
    //       subHandler.create(
    //         List("a", "b"),
    //         "c",
    //         List((2, 2), (2, 2)),
    //         Map())
    //     var outputString2 = ""
    //     subHandler.create(
    //       List("a", "b"),
    //       "d",
    //       List((2, 2), (2, 2)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 2 => {
    //     // Test MatrixMultiplyHandler
    //     var multHandler = new MatrixMultiplyHandler()

    //     var (netList, outputString1) =
    //       multHandler.create(
    //         List("a", "b"),
    //         "c",
    //         List((2, 3), (3, 2)),
    //         Map())
    //     var outputString2 = ""
    //     multHandler.create(
    //       List("a", "b"),
    //       "d",
    //       List((2, 3), (3, 2)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 3 => {
    //     // Test DivHandler
    //     var divHandler = new DivHandler()

    //     var (netList, outputString1) =
    //       divHandler.create(
    //         List("a", "b"),
    //         "c",
    //         List((1, 1), (1, 1)),
    //         Map())
    //     var outputString2 = ""
    //     divHandler.create(
    //       List("d", "b"),
    //       "e",
    //       List((2, 2), (1, 1)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }
    //     var outputString3 = ""
    //     divHandler.create(
    //       List("e", "f"),
    //       "g",
    //       List((2, 2), (2, 2)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString3 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //     println("")
    //     println(outputString3)
    //   }
    //   case 4 => {
    //     // Test FixedGainDivHandler
    //     var divHandler = new FixedGainDivHandler()

    //     var (netList, outputString1) =
    //       divHandler.create(
    //         List("a", "2"),
    //         "b",
    //         List((2, 2), (1, 1)),
    //         Map())
    //     var outputString2 = ""
    //     divHandler.create(
    //       List("a", "3"),
    //       "d",
    //       List((2, 2), (1, 1)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 5 => {
    //     // Test L2NormHandler
    //     var normHandler = new L2NormHandler()

    //     var (netList, outputString1) =
    //       normHandler.create(
    //         List("a"),
    //         "b",
    //         List((2, 1)),
    //         Map())
    //     var outputString2 = ""
    //     normHandler.create(
    //       List("a"),
    //       "c",
    //       List((2, 1)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 6 => {
    //     // Test SqrtHandler
    //     var sqrtHandler = new SqrtHandler()

    //     var (netList, outputString1) =
    //       sqrtHandler.create(
    //         List("a"),
    //         "b",
    //         List((1, 1)),
    //         Map())
    //     var outputString2 = ""
    //     sqrtHandler.create(
    //       List("a"),
    //       "c",
    //       List((1, 1)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case 7 => {
    //     // Test TransposeHandler
    //     var transposeHandler = new TransposeHandler()

    //     var (netList, outputString1) =
    //       transposeHandler.create(
    //         List("a"),
    //         "a_t",
    //         List((2, 3)),
    //         Map())
    //     var outputString2 = ""
    //     transposeHandler.create(
    //       List("a"),
    //       "b",
    //       List((2, 3)),
    //       netList) match {
    //         case (list, str) => {
    //           netList = list
    //           outputString2 = str
    //         }
    //       }

    //     for ((name, (style, rows, cols)) <- netList) {
    //       println(s"($name, $style, $rows, $cols)")
    //     }
    //     println("")
    //     println(outputString1)
    //     println("")
    //     println(outputString2)
    //   }
    //   case _ => println("Unrecognized TEST_CODE")
    // }

  }
}