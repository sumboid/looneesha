package looneesha

trait CFDefinition {
	def mapping: Map[String, List[DF] => List[Double]]
	def apply = mapping

	implicit def DFtoValue(df: DF) = df.value
}
