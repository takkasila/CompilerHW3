#ifndef AST
#define AST

#include <stdio.h>
#include "css.h"
#include "csg.h"
// #include "cfg.h"
typedef struct A_exp_ *A_exp;
typedef struct A_expList_ *A_expList;
typedef unsigned int A_pos;

typedef char *A_var;
typedef char *S_symbol;


struct A_exp_
{
	enum { A_varExp, A_intExp, A_callExp, A_opExp,A_op1Exp, A_assignExp, A_ifExp, A_whileExp, A_arrayExp, A_structExp } kind;
	A_pos pos;
	union {
		A_var var;
		/* nil; - needs only the pos */
		int intt;
		struct { S_symbol func; A_expList args; } call;
		struct { A_oper oper; A_exp left; A_exp right; } op;
		struct { A_exp var; A_exp exp; } assign;
		struct { A_exp test; A_expList then, elsee; } iff; /* elsee is optional */
		struct { A_exp test; A_expList body; } whilee;
		struct { A_exp var, exp; } array;
		struct { A_exp var, exp; } structt;
		struct { A_oper oper;A_exp left; } op1;
		A_expList arr;
		
	} u;
	A_exp next;
	A_exp desig_next;
};

struct A_expList_
{
	A_exp exp;
	A_expList next;
};




extern A_exp A_OpExp(A_pos pos, A_oper oper, A_exp left, A_exp right);
extern A_exp A_AssignExp(A_pos pos, A_exp var, A_exp exp);
extern A_exp A_IfExp(A_pos pos, A_exp test, A_expList then, A_expList elsee);
extern A_exp A_WhileExp(A_pos pos, A_exp test, A_expList body);
extern A_exp A_Call(A_pos pos, S_symbol func, A_expList args);
extern A_exp A_ArrayExp(A_pos pos, A_exp var, A_exp exp);
extern A_exp A_StructExp(A_pos pos, A_exp var, A_exp exp);
extern void push(A_expList *explist, A_exp exp);
extern A_exp A_Op1Exp(A_pos pos, A_exp exp, A_oper oper);
extern A_exp A_VarExp(A_pos pos, A_var var);
#endif
