object Utils {
	/**
	* * Read lines from the given input file.
	* */
	def readFrom(filename: String):Iterator[String] = {
		scala.io.Source.fromFile(s"src/main/resources/$filename").getLines()
	}

}