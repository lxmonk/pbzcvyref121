/* scheme/make_sob_symbol.asm
 * Takes address of string name in R0. Places in R0 the address
 * of a newly-allocated pointer to a Scheme SYMBOL
 *
 * Programmer: Aviad Reich, 2012
 */
MAKE_SOB_GENSYM:
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_GENSYM);
  MOV(INDD(R0, 1), GENSYM);
  ADD(GENSYM, IMM(1));
  RETURN;
