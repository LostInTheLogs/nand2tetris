// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.

// 256 rows of 512 pixels 
// 32 words ber row
// 32 * 256 = 8192

@8192
D=A
@SCREEN
D=D+A
@end
M=D

(START)
@SCREEN
D=A
@ptr
M=D

(LOOP)
@KBD
D=M

@JMPOVER
D;JEQ
D=-1
(JMPOVER)

(FILL)
@ptr
A=M
M=D

@ptr
MD=M+1
@end
D=M-D
@START
D;JLE
@LOOP
0;JMP

