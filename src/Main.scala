package looneesha

object KinematicsDef extends CFDefinition {
  override def mapping = Map("distance0" -> ((in: List[DF]) => (in(0) + in(1) * in(3) + in(2) * in(3) * in(3) / 2) :: Nil),
    "distance1" -> ((in: List[DF]) => (in(0) - in(1) * in(3) - in(2) * in(3) * in(3) / 2) :: Nil),
    "speed00" -> ((in: List[DF]) => (in(0) + in(1) * in(2)) :: Nil),
    "speed10" -> ((in: List[DF]) => (in(0) - in(1) * in(2)) :: Nil),
    "speed01" -> ((in: List[DF]) => ((in(1) - in(0)) / in(3) - in(2) * in(3) / 2) :: Nil),
    "deltaT0" -> ((in: List[DF]) => ((in(1) - in(0)) / in(2)) :: Nil),
    "accelerate0" -> ((in: List[DF]) => ((in(1) - in(0)) / in(2)) :: Nil),
    "accelerate1" -> ((in: List[DF]) => ((in(1) - in(0)) / in(3) / in(3) - in(2) / in(3)) :: Nil))
}

object MechanicsDef extends CFDefinition {
  override def mapping = Map("frictionF" -> ((in: List[DF]) => (in(0) * in(1)) :: Nil),
    "reactionF" -> ((in: List[DF]) => -in(0) :: Nil),
    "gravitationFY" -> ((in: List[DF]) => (in(1) * 9.8 * math.cos(in(0))) :: Nil),
    "gravitationFX" -> ((in: List[DF]) => (in(1) * 9.8 * math.sin(in(0))) :: Nil),
    "overallF" -> ((in: List[DF]) => (in(0) + in(1)) :: Nil),
    "accelerate" -> ((in: List[DF]) => (in(0) / in(1)) :: Nil),
    "overallF0" -> ((in: List[DF]) => (in(0) * in(1)) :: Nil),
    "reactionF0" -> ((in: List[DF]) => (in(1) / in(0)) :: Nil),
    "coeffriction" -> ((in: List[DF]) => (in(1) / in(0)) :: Nil),
    "gravitationFY0" -> ((in: List[DF]) => (in(0)) :: Nil),
    "mass0" -> ((in: List[DF]) => (in(0) / in(1)) :: Nil),
    "frictionF0" -> ((in: List[DF]) => (in(0) - in(1)) :: Nil),
    "gravitaionFX0" -> ((in: List[DF]) => (in(0) - in(1)) :: Nil)) 
}

object Kinematics extends GraphBuilder(KinematicsDef.mapping, "Kinematics") {
  FOR (*i, *a -> *b) {
    defn distance0 in (%s(i), %v(i), %a, %dt) -> out (%s(i + 1))
    defn speed00 in (%v(i), %a, %dt) -> out (%v(i + 1))
    defn accelerate0 in (%v(i), %v(i + 1), %dt) -> out(%a)
    defn accelerate1 in (%s(i), %s(i + 1), %v(i), %dt) -> out(%a)
  }
}

object Mechanics extends GraphBuilder(MechanicsDef.mapping, "Mechanics") {
  defn frictionF in (%cf, %fr) -> out (%ff)
  defn reactionF in (%fgy) -> out (%fr)

  defn gravitationFY in (%alpha, %m) -> out (%fgy)
  defn gravitationFX in (%alpha, %m) -> out (%fgx)

  defn overallF in (%fgx, %ff) -> out (%foa)
  defn accelerate in (%foa, %m) -> out (%Kinematics_a)

  defn overallF0 in (%Kinematics_a, %m) -> out (%foa)
  defn reactionF0 in (%cff, %ff) -> out (%fr)
  defn coeffriction in (%fr, %ff) -> out (%cf)
  defn gravitationFY0 in (%fr) -> out (%fgy)
  defn mass0 in (%foa, %Kinematics_a) -> out(%m)
  defn frictionF0 in (%foa, %fgx) -> out (%ff)
  defn gravitaionFX0 in (%foa, %ff) -> out (%fgx)
}

object MechanicsProblem extends ProblemBuilder {
  defn %Kinematics_v(0) 10
  defn %Kinematics_dt 1
  defn %Kinematics_s(0) 0
  defn *a 0
  defn *b 10

  defn %Mechanics_m 10
  defn %Mechanics_cf 0.02
  defn %Mechanics_alpha 0

  quest %Kinematics_s1

  quest %Kinematics_dt
}

object Main {
  def main(args: Array[String]): Unit = {
    val runtime = Runtime(Kinematics + Mechanics, MechanicsProblem)
    runtime.writeSolution
    runtime.init
    runtime.start
    runtime.visualize
    //GV create (Mechanics + Kinematics get) draw
  }
}
