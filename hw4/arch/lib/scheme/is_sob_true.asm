/* scheme/is_sob_bool.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is Boolean.
 *
 * Programmer: Mayer Goldberg, 2010
 */

 IS_SOB_TRUE:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_BOOL);
  JUMP_EQ(L_IS_SOB_TRUE_IS_BOOL);
  MOV(R0, IMM(1));   /* return 1 for true, (only #f is false) */
  JUMP(L_IS_SOB_TRUE_EXIT);
 L_IS_SOB_TRUE_IS_BOOL:
  MOV(R0, INDD(R0, 1));
  CMP(R0, IMM(0));
  JUMP_EQ(L_IS_SOB_TRUE_IS_FALSE);
  MOV(R0, IMM(1));                /* return 1 for true */
  JUMP(L_IS_SOB_TRUE_EXIT);
 L_IS_SOB_TRUE_IS_FALSE:
  MOV(R0, IMM(0));              /* return 0 for false */
 L_IS_SOB_TRUE_EXIT:
  POP(FP);
  RETURN;
