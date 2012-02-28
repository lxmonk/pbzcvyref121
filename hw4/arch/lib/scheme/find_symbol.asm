/* scheme/find_symbol.asm
 * takes a pointer to a string in R0
 * and return a pointer to the symbol in R0
 * with the same name. If it does not exist,
 * returns 0 in R0
 */

FIND_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  MOV(R2, R0);
  CMP(SYM_PTR, 0);
  JUMP_EQ(L_FIND_SYMBOL_RETURN_ZERO);
  MOV(R1, SYM_PTR);
 L_FIND_SYMBOL_LOOP:
  CMP(INDD(R1, 1),R2);
  JUMP_EQ(L_FIND_SYMBOL_RETURN_R1);

  MOV(R1, INDD(R1, 4));
  CMP(R1, T_NULL_PTR);
  JUMP_EQ(L_FIND_SYMBOL_RETURN_ZERO);
  /* MOV(R1, IND(R1)); */
  JUMP(L_FIND_SYMBOL_LOOP);

 L_FIND_SYMBOL_RETURN_R1:
  MOV(R0, R1);
  JUMP(L_FIND_SYMBOL_END);

 L_FIND_SYMBOL_RETURN_ZERO:
  MOV(R0, IMM(0));

 L_FIND_SYMBOL_END:
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;
