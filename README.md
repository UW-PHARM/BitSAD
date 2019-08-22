# BitSAD
[![Build Status](https://travis-ci.com/UW-PHARM/BitSAD.svg?branch=master)](https://travis-ci.com/UW-PHARM/BitSAD)

A domain-specific language for bitstream computing.

### Citation

If you use BitSAD in your work, please cite this paper:
```
@INPROCEEDINGS {BitSAD2019,
	author = {Kyle Daruwalla and Heng Zhuo and Mikko Lipasti},
	title = {{BitSAD}: A Domain-Specific Language for Bitstream Computing},
	booktitle = {Unary Computing Workshop},
	year = {2019},
	month = {June}
}
```

## Summary

BitSAD is a domain-specific language for bitstream computing delivered in two parts: a library API and a compiler plugin. The library API provides the `SBitstream` and `DBitstream` classes, `Matrix[A]` class, and corresponding useful functions and classes. The compiler plugin is developed alongside the library API to deliver automated hardware (Verilog) generation for BitSAD programs.

## Installation

The library can be installed by adding the following line to your `build.sbt`:
```scala
libraryDependencies += "com.github.uw-pharm" % "bitsad-libraries_2.12" % "0.5.0",
```

To install the plugin, first add the library. Then, add the following to your `build.sbt`:
```scala
libraryDependencies += "com.github.uw-pharm" % "bitsad-plugin_2.12" % "v0.5.0",
libraryDependencies += compilerPlugin("com.github.uw-pharm" % "bitsad-plugin_2.12" % "v0.5.0"),
scalacOptions += s"-Xplugin:bitsad-plugin_2.12.jar:bitsad-libraries_2.12.jar",
scalacOptions += s"-P:bitsad-plugin:top:<your top level file>.scala"
```

If you want working examples of installation, please refer to `BitBench/build.sbt`.

### Verilog Submodules

The generated Verilog requires submodules that implement each operator. These modules are packaged separately in `verilog-lib.tar.gz`. Download the compressed file, then uncompress it into the directory containing your Verilog project source code.

## A Sample BitSAD Program

Below is a sample BitSAD module. The names of traits, objects, classes, and functions must be as specified (e.g. `Parameters`, `Module`, `loop`, etc.). Please refer to `BitBench` for working example programs.

```scala
package <your module name>

// Import libraries as you need them
import bitstream.types._
import bitstream.simulator.units._
import math._

trait Parameters {
  // Declare values that parameterize your module
  // (e.g. constants, matrix sizes, etc.)
}

object DefaultParams extends Parameters {
  // Set values for each of the parameters listed above
}

// The class that defines your module
// Must accept Parameters argument as input to constructor
case class Module (params: Parameters) {

  // Define outputs
  // A List() of 3-tuples
  // Each tuple is (<string with name of var>, <num of rows>, <num of cols>)
  val outputList = List(("v", 2, 2), ("u", 2, 2), ("sigma", 1, 1))

  // Required function that describes the compute done in
  //   one clock cycle of your module
  // Arguments/return can be anything
  // A variable that is an output but also an input must be assigned as
  //   _<var name>
  def loop(A: Matrix[SBitstream], v: Matrix[SBitstream]):
      (Matrix[SBitstream], Matrix[SBitstream], SBitstream) = {
    // Update right singular vector
    var w = A * v

    // ...

    (u, _v, sigma)
  }

}

// BELOW IS NOT REQUIRED

object IterativeSVD {

  def main(args: Array[String]) {

    // Useful to instantiate your module and test it
    // This will not affect hardware generation in any way

  }
}
```

### Note about BitBench

The BitBench folder is a Git submodule. When you clone this repo, add the `--recursive` flag to your Git command to also clone the BitBench repo.

## Contributing

We welcome all kinds of contributions including PRs, sample code, and issues.

### Issues

If you have a feature request or an issue with BitSAD, please file an issue on GitHub. Please file issues relating to BitBench with the BitBench repository.

### Pull Requests

If you would like to add to BitSAD, we welcome PRs. Submit a PR to the `staging` branch. This allows us to move around files/reorganize the repo before bringing your code into the `master` branch. *All commits to the master branch create versions on Sonatype*.

If you want to add a new operator, etc. to the project, then please email us or file an issue, and we can direct you on how to proceed. Later versions of BitSAD will likely modularize this process to make it very simple and straightforward.