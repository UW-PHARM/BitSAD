package bitstream.compiler

case class ImportRecord(filename: String) {

  private var moduleMap = Map[String, Tuple2[String, List[String]]]()
  private var moduleRenameMap = Map[String, String]()
  private var invRenameMap = Map[String, String]()
  private var defaultParams = Map[String, Double]()
  private var outputList = List[String]()
  private var outputSizes = List[Tuple2[Int, Int]]()

  def checkRenameRule(original: String): String = moduleRenameMap(original)

  def addRenameRule(original: String, rename: String): Unit =
    moduleRenameMap += (original -> rename)

  def mkInvRenameMap(): Unit = invRenameMap = moduleRenameMap.map(_.swap)

  def checkInvRenameRule(name: String): String = invRenameMap(name)

  def isModule(name: String): Boolean = invRenameMap contains name

  def params: Map[String, Double] = defaultParams

  def setDefaultParams(map: Map[String, Double]): Unit =
    defaultParams = map

  def getModule(name: String): Tuple2[String, List[String]] = moduleMap(name)

  def checkModule(name: String): Boolean = moduleMap contains name

  def addModuleRule(name: String, toplevelName: String, paramList: List[String]): Unit =
    moduleMap += (name -> (toplevelName, paramList))

  def printModuleMap() = println(moduleMap)

  def addOutput(name: String): Unit =
    outputList = outputList ++ List(name)

  def addOutputSize(size: Tuple2[Int, Int]): Unit =
    outputSizes = outputSizes ++ List(size)

  def getOutList(): List[String] = outputList

  def getOutSizeList(): List[Tuple2[Int, Int]] = outputSizes

  def printOutputs(): Unit = for ((output, size) <- outputList.zip(outputSizes)) {
    println(s"$output (${size._1}, ${size._2})")
  }

}