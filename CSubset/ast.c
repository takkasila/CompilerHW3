#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ast.h"

A_exp A_OpExp(A_pos pos, A_oper oper, A_exp left, A_exp right) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_opExp;
	p->pos = pos;
	p->u.op.oper = oper;
	p->u.op.left = left;
	p->u.op.right = right;
	return p;
}

A_exp A_AssignExp(A_pos pos, A_var var, A_exp exp) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_assignExp;
	p->pos = pos;
	p->u.assign.var = var;
	p->u.assign.exp = exp;
	return p;
}

A_exp A_IfExp(A_pos pos, A_exp test, A_exp then, A_exp elsee) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_ifExp;
	p->pos = pos;
	p->u.iff.test = test;
	p->u.iff.then = then;
	p->u.iff.elsee = elsee;
	return p;
}

A_exp A_WhileExp(A_pos pos, A_exp test, A_exp body) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_whileExp;
	p->pos = pos;
	p->u.whilee.test = test;
	p->u.whilee.body = body;
	return p;
}