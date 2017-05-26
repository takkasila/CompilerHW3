#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include "ast.h"
#include "csg.h"
// #include "cfg.h"

A_exp A_OpExp(A_pos pos, A_oper oper, A_exp left, A_exp right) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_opExp;
	p->pos = pos;
	p->u.op.oper = oper;
	p->u.op.left = left;
	p->u.op.right = right;
	return p;
}

A_exp A_AssignExp(A_pos pos, A_exp var, A_exp exp) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_assignExp;
	p->pos = pos;
	p->u.assign.var = var;
	p->u.assign.exp = exp;
	return p;
}

A_exp A_IfExp(A_pos pos, A_exp test, A_expList then, A_expList elsee) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_ifExp;
	p->pos = pos;
	p->u.iff.test = test;
	p->u.iff.then = then;
	p->u.iff.elsee = elsee;
	return p;
}

A_exp A_WhileExp(A_pos pos, A_exp test, A_expList body) {
	A_exp p = malloc(sizeof(*p));
	p->kind = A_whileExp;
	p->pos = pos;
	p->u.whilee.test = test;
	p->u.whilee.body = body;
	return p;
}

A_exp A_Call(A_pos pos, S_symbol func, A_expList args){
	A_exp p = malloc(sizeof(*p));
	p->pos = pos;
	p->kind = A_callExp;
	p->u.call.func = func;
	p->u.call.args = args;
	return p;
}

A_exp A_ArrayExp(A_pos pos, A_exp var, A_exp exp){
	A_exp p = malloc(sizeof(*p));
	p->kind = A_arrayExp;
	p->pos = pos;
	p->u.array.var = var;
	p->u.array.exp = exp;
	return p;
}
A_exp A_StructExp(A_pos pos, A_exp var, A_exp exp){
	A_exp p = malloc(sizeof(*p));
	p->kind = A_structExp;
	p->pos = pos;
	p->u.structt.var = var;
	p->u.structt.exp = exp;
	return p;

}
void push(A_expList *explist, A_exp exp) {
    if (!explist) return ;
    if (!*explist) {
        (*explist) = malloc(sizeof(struct A_expList_));
        (*explist)->exp = exp;
        (*explist)->next = NULL;
    } else {
        A_expList it = *explist;
        while(it->next) it = it->next;
        it->next = malloc(sizeof(struct A_expList_));
        it = it->next;
        it->exp = exp;
        it->next = NULL;
    }
}
A_exp A_VarExp(A_pos pos, A_var var){
    A_exp p = malloc(sizeof(*p));
    p->kind = A_varExp;
    p->u.var = var;
	return p;
	
}
A_exp A_Op1Exp(A_pos pos, A_exp exp, A_oper oper){
    A_exp p = malloc(sizeof(*p));
    p->kind = A_op1Exp;
    p->pos = pos;
    p->u.op1.left = exp;
    p->u.op1.oper = oper;
    return p;
}