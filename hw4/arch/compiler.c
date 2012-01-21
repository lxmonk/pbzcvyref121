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

 CONTINUE:
    /* initialize the 4 singletons */
    PUSH(IMM(1));
    CALL(MAKE_SOB_BOOL);         /* define SOB_BOOL_TRUE in mem[1]*/
    DROP(1);
    PUSH(IMM(0));
    CALL(MAKE_SOB_BOOL);         /* define SOB_BOOL_FALSE in mem[3]*/
    DROP(1);
    CALL(MAKE_SOB_NIL);          /* define nil in mem[5] */
    CALL(MAKE_SOB_VOID);         /* define #Void in mem[6] */

  /* start of code */
  /* CALL(MAKE_SOB_NIL); */
    /* MOV(R0, IND(IMM(4))); */
    MOV(R0, IMM(5));
  PUSH(R0);
  CALL(IS_SOB_TRUE);
  CMP(R0, IMM(1));              /* 1 means R0 was true, 0 means it
                                   was #f */
  JUMP_EQ(Lelse1);
  PUSH(IMM(1));
  CALL(MAKE_SOB_BOOL);
  JUMP(Lexit1);
 Lelse1:
  PUSH(IMM(0));
  CALL(MAKE_SOB_BOOL);
 Lexit1:

  PUSH(R0);
  CALL(WRITE_SOB);
  /* newline and stop machine */
  PUSH(IMM('\n'));
  CALL(PUTCHAR);
  STOP_MACHINE;

  return 0;
}
