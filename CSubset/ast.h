#ifndef AST
#define AST

typedef struct A_exp_ *A_exp;
typedef struct A_expList_ *A_expList;
typedef struct A_pos_ *A_pos;
typedef struct A_oper_ *A_oper;
typedef struct A_var_ *A_var;
typedef struct S_symbol_ *S_symbol;

struct A_exp_
{
	enum { A_varExp, A_intExp, A_callExp, A_opExp, A_assignExp, A_ifExp, A_whileExp, A_arrayExp } kind;
	A_pos pos;
	union {
		A_var var;
		/* nil; - needs only the pos */
		int intt;
		struct { S_symbol func; A_expList args; } call;
		struct { A_oper oper; A_exp left; A_exp right; } op;
		struct { A_exp var; A_exp exp; } assign;
		struct { A_exp test, then, elsee; } iff; /* elsee is optional */
		struct { A_exp test, body; } whilee;
		A_expList arr;
	} u;
	A_exp next;
	A_exp desig_next;
};

struct A_expList_
{
	A_pos pos;
	A_exp start, end;
};

struct A_pos_
{
	int number;
};

struct A_oper_
{
	A_pos pos;
	S_symbol op;

};

struct A_var_
{
	A_pos pos;
	S_symbol name;
};

struct S_symbol_
{
	A_pos pos;
	char *name;
};


extern A_exp A_OpExp(A_pos pos, A_oper oper, A_exp left, A_exp right);
extern A_exp A_AssignExp(A_pos pos, A_var var, A_exp exp);
extern A_exp A_IfExp(A_pos pos, A_exp test, A_exp then, A_exp elsee);
extern A_exp A_WhileExp(A_pos pos, A_exp test, A_exp body);

#endif
