#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct A_exp_ *A_exp;
typedef struct A_expList_ *A_expList;
typedef struct A_pos_ *A_pos;
typedef struct A_oper_ *A_oper;
typedef struct A_var_ *A_var;
typedef struct S_symbol_ *S_symbol;

struct A_exp_
{
    enum {
        A_varExp
        , A_intExp
        , A_callExp
        , A_opExp
        , A_assignExp
        , A_ifExp
        , A_whileExp
        , A_arrayExp} kind;
    A_pos pos;
    union {A_var var;
        /* nil; - needs only the pos */
        int intt;
        struct {S_symbol func; A_expList args;} call;
        struct {A_oper oper; A_exp left; A_exp right;} op;
        struct {A_var var; A_exp exp;} assign;
        struct {A_exp test, then, elsee;} iff; /* elsee is optional */
        struct {A_exp test, body;} whilee;
    } u;
};

struct A_expList_
{

};

struct A_pos_
{

};

struct A_oper_
{

};

struct A_var_
{

};

struct S_symbol_
{

};

A_exp A_OpExp(A_pos pos, A_oper oper, A_exp left, A_exp right) {
    A_exp p = malloc(sizeof(*p));
    p->kind=A_opExp;
    p->pos=pos;
    p->u.op.oper=oper;
    p->u.op.left=left;
    p->u.op.right=right;
    return p;
}

A_exp A_AssignExp(A_pos pos, A_var var, A_exp exp) {
    A_exp p = malloc(sizeof(*p));
    p->kind=A_assignExp;
    p->pos=pos;
    p->u.assign.var=var;
    p->u.assign.exp=exp;
    return p;
}

A_exp A_IfExp(A_pos pos, A_exp test, A_exp then, A_exp elsee) {
    A_exp p = malloc(sizeof(*p));
    p->kind=A_ifExp;
    p->pos=pos;
    p->u.iff.test=test;
    p->u.iff.then=then;
    p->u.iff.elsee=elsee;
    return p;
}

A_exp A_WhileExp(A_pos pos, A_exp test, A_exp body) {
    A_exp p = malloc(sizeof(*p));
    p->kind=A_whileExp;
    p->pos=pos;
    p->u.whilee.test=test;
    p->u.whilee.body=body;
    return p;
}
