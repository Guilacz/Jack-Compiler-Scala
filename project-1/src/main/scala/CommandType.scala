sealed trait CommandType

//STAGE 1

case object C_ARITHMETIC extends CommandType
case object C_PUSH extends CommandType
case object C_POP extends CommandType

// STAGE 2
case object C_LABEL extends CommandType
case object C_GOTO extends CommandType
case object C_IF extends CommandType
case object C_FUNCTION extends CommandType
case object C_RETURN extends CommandType
case object C_CALL extends CommandType


