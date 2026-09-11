// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// The algorithm is based on repetitive addition.

//// Replace this comment with your code.

// R2 = R0 * R1

// R2 = 0
@0
D=A
@R2
M=D

// i=R1
@R1
D=M
@i
M=D

(LOOP)
// i--
@i
DM=M-1
// if i<0 break
@ELOOP
D;JLT
// R2 += R0
@R0
D=M
@R2
M=D+M

@LOOP
0;JMP

(ELOOP)
@ELOOP
0;JMP