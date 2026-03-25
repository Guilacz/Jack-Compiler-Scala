sealed trait CommandType
case object C_ARITHMETIC extends CommandType
case object C_PUSH extends CommandType
case object C_POP extends CommandType