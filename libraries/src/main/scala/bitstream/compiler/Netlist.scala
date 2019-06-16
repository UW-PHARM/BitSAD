package bitstream.compiler

case class Net (isSigned: Boolean, isReg:Boolean, width:Int, depth:Int)

  // def isReg: Boolean = _isReg

  // def isReg_=(isReg:Boolean) = {
  //   _isReg = isReg
  // }

  // def width: Int = _width

  // def width_=(width:Int) = {
  //   _width = width
  // }

  // def depth: Int = _depth

  // def depth_=(depth:Int) = {
  //   _depth = depth
  // }

case class Netlist() {

  // Internal maps for storing the netlist
  private var inputMap: Map[String, Net] = Map()
  private var outputMap: Map[String, Net] = Map()
  private var wireMap: Map[String, Net] = Map()

  def inputs: Map[String, Net] = inputMap

  def outputs: Map[String, Net] = outputMap

  def wires: Map[String, Net] = wireMap

  def contains(name: String): Boolean =
    (inputMap contains name) || (outputMap contains name) || (wireMap contains name)

  def getSize(name: String): (Int, Int) = {
    if (inputMap contains name) (inputMap(name).width, inputMap(name).depth)
    else if (outputMap contains name) (outputMap(name).width, outputMap(name).depth)
    else if (wireMap contains name) (wireMap(name).width, wireMap(name).depth)
    else throw new IllegalArgumentException("Net name passed to Netlist.getSize does not exist")
  }

  def addInputs(inputs: List[String], sizes: List[Tuple2[Int, Int]],
  				isSigned: List[Boolean]): Unit = {
    for ((name, size, sign) <- (inputs, sizes, isSigned).zipped.toList) {
      if (!(inputMap contains name)) inputMap += (name -> Net(sign, false, size._1, size._2))
    }
  }

  def setOutputs(outputs: List[String]): Unit = {
    // Update netlist
    for (output <- outputs) {
      if (wireMap contains output) {
        outputMap += (output -> wireMap(output))
        wireMap -= output
      } else throw new IllegalArgumentException(
        "Name passed into Netlist.setOutputs is not an already existing internal net: " + output)
    }
  }

  def addWire(name: String, size: Tuple2[Int, Int], isSigned: Boolean, isReg: Boolean): Unit = {
    if (!(this contains name)) wireMap += (name -> Net(isSigned, isReg, size._1, size._2))
  }

}