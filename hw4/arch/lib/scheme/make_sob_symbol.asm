/* scheme/make_sob_symbol.asm
 * Takes address of string name in R0. Places in R0 the address
 * of a newly-allocated pointer to a Scheme SYMBOL
 *
 * Programmer: Aviad Reich, 2012
 */
MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  MOV(R1, R0);                    /* save address of string-name */
  PUSH(IMM(5));
  CALL(MALLOC);
  DROP(1);
  CMP(R16,IMM(0));
  JUMP_EQ(L_FIRST_SYMBOL);
  MOV(INDD(R16, 4), R0);          /* place a ptr to this symbol in the
                                   * previous one. */
 L_FIRST_SYMBOL:
  MOV(R16, R0);                 /* this is now the "previous" symbol */
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0, 1), R1);               /* place string ptr in sym[1] */
  MOV(INDD(R0, 2), IMM(0));            /* symbol NOT initialized */
  MOV(INDD(R0, 3), IMM(0xFFFFFFFF));   /* "initial" value ptr */
  MOV(INDD(R0, 4), IMM(0xEEEEEEEE));
  POP(R1);
  POP(FP);
  RETURN;
