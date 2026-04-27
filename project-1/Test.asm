// push constant 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// push constant 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
// eq#
@SP
AM=M-1
D=M
A=A-1
D=M-D
@TRUE_0
D;JEQ
@SP
A=M-1
M=-1
@END_0
0;JMP
(TRUE_0)
@SP
A=M-1
M=0
(END_0)
