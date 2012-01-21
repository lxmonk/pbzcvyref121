/* compiler.c
 * Mock-assembly programming for a CISC-like architecture
 *
 * @author: The code generator written as part of compilers 121
 * BGU, IL.
 */

#include <stdio.h>
#include <stdlib.h>

#include "cisc.h"

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 0

/* for debugging only, use SHOW("<some message>, <arg> */
/* #if DO_SHOW */
/* #define SHOW(msg, x) { printf("%s %s = %ld\n", (msg), (#x), (x)); } */
/* #else */
/* #define SHOW(msg, x) {} */
/* #endif */

int main()
{
    START_MACHINE;
    JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"
#include "scheme.lib"

#define SOB_BOOL_TRUE (ADDR(1));
#define SOB_BOOL_FALSE (ADDR(3));
#define SOB_NIL (M(mem)[5]);
#define SOB_VOID (ADDR(6));
#define IS_TRUE(reg)

 CONTINUE:
    /* initialize the 4 singletons */
    PUSH(IMM(1));
    CALL(MAKE_SOB_BOOL);         /* define SOB_BOOL_TRUE */
    DROP(1);
    PUSH(IMM(0));
    CALL(MAKE_SOB_BOOL);         /* define SOB_BOOL_FALSE */
    DROP(1);
    CALL(MAKE_SOB_NIL);          /* define nil */
    CALL(MAKE_SOB_VOID);

  /* start of code */
    /*
        code for the expression:
        (begin #t #t #f () #t)
    */
    /*
        code for the expression:
        #t
    */
 MOV(R0, IMM(1));    /* assigning true to R0 */
    /*
        code for the expression:
        #t
    */
 MOV(R0, IMM(1));    /* assigning true to R0 */
    /*
        code for the expression:
        #f
    */
 MOV(R0, IMM(3));    /* assigning false to R0 */
    /*
        code for the expression:
        ()
    */
 MOV(R0, IMM(5));    /* assign nil to R0 */
    /*
        code for the expression:
        #t
    */
 MOV(R0, IMM(1));    /* assigning true to R0 */


/* end of generated code. CONCLUSION: */
 MOV(R1, R0);    /* save the result in R1 */
 PUSH(R1);    /* pushing R1 for void-check */
 CALL(IS_SOB_VOID);    /* if R0 is #Void, don't print */
 DROP(1);
 CMP(R0, IMM(1));    /* 1 means R0 was void, 0 means it wasn't */
 JUMP_EQ(END_OF_THE_WORLD);    /* do not print */
 PUSH(R1);    /* push R1 for print */
 CALL(WRITE_SOB);    /* print the result before quitting */
 DROP(1);
/* that was useless.. */
END_OF_THE_WORLD:    /* (only as we know it.) */
  /* newline and stop machine */
  PUSH(IMM('\n'));
  CALL(PUTCHAR);
  STOP_MACHINE;

  return 0;
}
