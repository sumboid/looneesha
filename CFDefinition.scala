package looneesha

trait CFDefinition {
	val mapping: Map[String, List[DF] => List[Double]]
	def apply = mapping

	implicit def DFtoValue(df: DF) = df.value
}