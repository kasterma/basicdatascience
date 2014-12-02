namespace java net.kasterma.thriftex1

struct LabeledNumber {
  1 : string label
  2 : i16 number
}

struct LabeledState {
  1 : string label
  2 : bool state
}

union DataUnit {
  1 : LabeledNumber num
  2 : LabeledState state
}
