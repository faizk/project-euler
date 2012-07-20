object P67 extends P18 {

  override
  val triangle = io.Source.fromFile("triangle.txt").getLines.
    map (_ split " " map (_.toInt) toIndexedSeq)  toIndexedSeq

}

// vim: set ts=2 sw=2 et:
