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
    int _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t;
    int _u, _v, _w, _x, _y, _z; /* some vars.. */
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
#define SOB_NIL (ADDR(5));
#define SOB_VOID (ADDR(6));

 DEBUGGG:
    RETURN;

 CONTINUE:

 START_OF_GENERATED_CODE:

    /* code for auto generated constants */
