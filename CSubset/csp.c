/*  C Subset Parser  9-15-04  Martin Burtscher  */


#include <stdlib.h>
#include <string.h>
#include <assert.h>

// #include "csp.h"
#include "css.h"
#include "csg.h"
// #include "cfg.h"
#include "ast.h"
#include "ic.h"

static int sym;
static int instruct;
static int tos;
static CSGNode globscope;
CSGNode my_code;
A_expList exps;


static CSGNode FindObj(CSGNode *root, CSSIdent *id)
{
  register int maxlev;
  register CSGNode curr;
  register CSGNode obj;

  maxlev = -1;
  curr = *root;
  obj = NULL;
  while (curr != NULL) {
    while ((curr != NULL) && ((strcmp(curr->name, *id) != 0) || (curr->lev <= maxlev))) {
      curr = curr->next;
    }
    if (curr != NULL) {
      obj = curr;
      maxlev = curr->lev;
      curr = curr->next;
    }
  }
  if (obj != NULL) {
    if (((obj->class == CSGVar) || (obj->class == CSGFld)) && ((obj->lev != 0) && (obj->lev != CSGcurlev))) {
      CSSError("object cannot be accessed");
    }
  }
  return obj;
}


static CSGNode AddToList(CSGNode *root, CSSIdent *id)
{
  register CSGNode curr;

  curr = NULL;
  if (*root == NULL) {
    curr = malloc(sizeof(CSGNodeDesc));
    assert(curr != NULL);
    *root = curr;
    if (curr == NULL) CSSError("out of memory");
    curr->class = -1;
    curr->lev = CSGcurlev;
    curr->next = NULL;
    curr->dsc = NULL;
    curr->type = NULL;
    strcpy(curr->name, *id);
    curr->val = 0;
  } else {
    curr = *root;
    while (((curr->lev != CSGcurlev) || (strcmp(curr->name, *id) != 0)) && (curr->next != NULL)) {
      curr = curr->next;
    }
    if ((strcmp(curr->name, *id) == 0) && (curr->lev == CSGcurlev)) {
      CSSError("duplicate identifier");
    } else {
      curr->next = malloc(sizeof(CSGNodeDesc));
      assert(curr->next != NULL);
      curr = curr->next;
      if (curr == NULL) CSSError("out of memory");
      curr->class = -1;
      curr->lev = CSGcurlev;
      curr->next = NULL;
      curr->dsc = NULL;
      curr->type = NULL;
      strcpy(curr->name, *id);
      curr->val = 0;
    }
  }
  return curr;
}


static void InitObj(CSGNode obj, signed char class, CSGNode dsc, CSGType type, long long val)
{
  obj->class = class;
  obj->next = NULL;
  obj->dsc = dsc;
  obj->type = type;
  obj->val = val;
}


static void InitProcObj(CSGNode obj, signed char class, CSGNode dsc, CSGType type, CSGNode entrypt)
{
  obj->class = class;
  obj->next = NULL;
  obj->dsc = dsc;
  obj->type = type;
  obj->true = entrypt;
}


/*************************************************************************/


static A_exp Expression(CSGNode *x);
static A_exp DesignatorM(CSGNode *x);


static A_exp Factor(CSGNode *x)
{
  register CSGNode obj;
  A_exp p;

  switch (sym) {
    case CSSident:
      obj = FindObj(&globscope, &CSSid);
      if (obj == NULL) CSSError("unknown identifier");
      CSGMakeNodeDesc(x, obj);
      sym = CSSGet();  // consume ident before calling Designator
      p = DesignatorM(x);
      break;
    case CSSnumber:
      CSGMakeConstNodeDesc(x, CSGlongType, CSSval);
      p = malloc(sizeof(*p));
      p->kind = A_intExp;
      p->u.intt = CSSval;
      sym = CSSGet();
      break;
    case CSSlparen:
      sym = CSSGet();
      p = Expression(x);
      if (sym != CSSrparen) CSSError("')' expected");
      sym = CSSGet();
      break;
    default: CSSError("factor expected"); break;
  }
  return p;
}


static A_exp Term(CSGNode *x)
{
  register int op;
  CSGNode y;
  A_exp l, r;

  l = Factor(x);
  while ((sym == CSStimes) || (sym == CSSdiv) || (sym == CSSmod)) {
    op = sym; 
    sym = CSSGet();
    y = malloc(sizeof(CSGNodeDesc));
    assert(y != NULL);
    r = Factor(&y);
    CSGOp2(op, x, y);
    l = A_OpExp(0, op, l, r);
  }
  return l;
}


static A_exp SimpleExpression(CSGNode *x)
{
  register int op;
  CSGNode y;
  A_exp l, r, root;

  if ((sym == CSSplus) || (sym == CSSminus)) {
    op = sym; 
    sym = CSSGet();
    l = A_Op1Exp(0, Term(x), op);
    CSGOp1(op, x);
  } else {
    l = Term(x);
  }
  while ((sym == CSSplus) || (sym == CSSminus)) {
    op = sym; 
    sym = CSSGet();
    y = malloc(sizeof(CSGNodeDesc));
    assert(y != NULL);
    r = Term(&y);
    CSGOp2(op, x, y);
    l = A_OpExp(0, op, l, r);
  }
  return l;
}


static A_exp EqualityExpr(CSGNode *x)
{
  register int op;
  A_exp left, right, p;
  CSGNode y;

  left = SimpleExpression(x);
  if ((sym == CSSlss) || (sym == CSSleq) || (sym == CSSgtr) || (sym == CSSgeq)) {
    y = malloc(sizeof(CSGNodeDesc));
    assert(y != NULL);
    op = sym; 
    sym = CSSGet();
    right = SimpleExpression(&y);
    CSGRelation(op, x, y);
    return A_OpExp(0, op, left, right);
  }
  return left;
}


static A_exp Expression(CSGNode *x)
{
  register int op;
  CSGNode y;
  A_exp l;

  l = EqualityExpr(x);
  if ((sym == CSSeql) || (sym == CSSneq)) {
    op = sym; 
    sym = CSSGet();
    y = malloc(sizeof(CSGNodeDesc));
    assert(y != NULL);
    l = A_OpExp(0, op, l, EqualityExpr(&y));
    CSGRelation(op, x, y);
  }
  return l;
}


static int ConstExpression(CSGNode *expr)
{
  Expression(expr);
  if ((*expr)->class != CSGConst) CSSError("constant expression expected");
}


/*************************************************************************/


static A_var VariableDeclaration(CSGNode *root);


static void FieldList(CSGType type)
{
  register CSGNode curr;

  VariableDeclaration(&(type->fields));
  while (sym != CSSrbrace) {
    VariableDeclaration(&(type->fields));
  }
  curr = type->fields;
  if (curr == NULL) CSSError("empty structs are not allowed");
  while (curr != NULL) {
    curr->class = CSGFld;
    curr->val = type->size;
    type->size += curr->type->size;
    if (type->size > 0x7fffffff) CSSError("struct too large");
    curr = curr->next;
  }
}


static void StructType(CSGType *type)
{
  register CSGNode obj;
  register int oldinstruct;
  CSSIdent id;

  assert(sym == CSSstruct);
  sym = CSSGet();
  if (sym != CSSident) CSSError("identifier expected");
  strcpy(id, CSSid);
  sym = CSSGet();
  if (sym != CSSlbrace) {
    obj = FindObj(&globscope, &id);
    if (obj == NULL) CSSError("unknown struct type");
    if ((obj->class != CSGTyp) || (obj->type->form != CSGStruct)) CSSError("struct type expected");
    *type = obj->type;
  } else {
    sym = CSSGet();
    *type = malloc(sizeof(CSGTypeDesc));
    if ((*type) == NULL) CSSError("out of memory");
    (*type)->form = CSGStruct;
    (*type)->fields = NULL;
    (*type)->size = 0;
    oldinstruct = instruct;
    instruct = 1;
    FieldList(*type);
    instruct = oldinstruct;
    if (sym != CSSrbrace) CSSError("'}' expected");
    sym = CSSGet();
    obj = AddToList(&globscope, &id);
    InitObj(obj, CSGTyp, NULL, *type, (*type)->size);
  }
}


static void Type(CSGType *type)
{
  register CSGNode obj;

  if (sym == CSSstruct) {
    StructType(type);
  } else {
    if (sym != CSSident) CSSError("identifier expected");
    obj = FindObj(&globscope, &CSSid);
    sym = CSSGet();
    if (obj == NULL) CSSError("unknown type");
    if (obj->class != CSGTyp) CSSError("type expected");
    *type = obj->type;
  }
}


static void RecurseArray(CSGType *type)
{
  register CSGType typ;
  CSGNode expr;

  expr = malloc(sizeof(CSGNodeDesc));
  assert(expr != NULL);
  assert(sym == CSSlbrak);
  sym = CSSGet();
  ConstExpression(&expr);
  if (expr->type != CSGlongType) CSSError("constant long expression required");
  if (sym != CSSrbrak) CSSError("']' expected");
  sym = CSSGet();
  if (sym == CSSlbrak) {
    RecurseArray(type);
  }
  typ = malloc(sizeof(CSGTypeDesc));
  if (typ == NULL) CSSError("out of memory");
  typ->form = CSGArray;
  typ->len = expr->val;
  typ->base = *type;
  if (0x7fffffff / typ->len < typ->base->size) {
    CSSError("array size too large");
  }
  typ->size = typ->len * typ->base->size;
  *type = typ;
}


static void IdentArray(CSGNode *root, CSGType type)
{
  register CSGNode obj;

  if (sym != CSSident) CSSError("identifier expected");
  obj = AddToList(root, &CSSid);
  sym = CSSGet();
  if (sym == CSSlbrak) {
    RecurseArray(&type);
  }
  if (instruct == 0) tos -= type->size;
  InitObj(obj, CSGVar, NULL, type, tos);
}


static void IdentList(CSGNode *root, CSGType type)
{
  IdentArray(root, type);
  while (sym == CSScomma) {
    sym = CSSGet();
    IdentArray(root, type);
  }
}


static A_var VariableDeclaration(CSGNode *root)
{
  CSGType type;

  Type(&type);
  IdentList(root, type);
  if (sym != CSSsemicolon) CSSError("';' expected");
  sym = CSSGet();
}


static int ConstantDeclaration(CSGNode *root)
{
  register CSGNode obj;
  CSGType type;
  CSGNode expr;
  CSSIdent id;

  expr = malloc(sizeof(CSGNodeDesc));
  assert(expr != NULL);
  assert(sym == CSSconst);
  sym = CSSGet();
  Type(&type);
  if (type != CSGlongType) CSSError("only long supported");
  if (sym != CSSident) CSSError("identifier expected");
  strcpy(id, CSSid);
  sym = CSSGet();
  if (sym != CSSbecomes) CSSError("'=' expected");
  sym = CSSGet();
  ConstExpression(&expr);
  if (expr->type != CSGlongType) CSSError("constant long expression required");
  obj = AddToList(root, &id);
  InitObj(obj, CSGConst, NULL, type, expr->val);
  if (sym != CSSsemicolon) CSSError("';' expected");
  sym = CSSGet();
}


/*************************************************************************/


static A_exp DesignatorM(CSGNode *x)
{
  register CSGNode obj;
  CSGNode y;
  A_exp p, root, cur;
  root = p = cur = A_StructExp(0, A_VarExp(0, (*x)->name), NULL);

  // CSSident already consumed
  while ((sym == CSSperiod) || (sym == CSSlbrak)) {
    if (sym == CSSperiod) {
      sym = CSSGet();
      if ((*x)->type->form != CSGStruct) CSSError("struct type expected");
      if (sym != CSSident) CSSError("field identifier expected");
      obj = FindObj(&(*x)->type->fields, &CSSid);
      sym = CSSGet();
      if (obj == NULL) CSSError("unknown identifier");
      CSGField(x, obj);
      p->kind = A_structExp;
      p->u.structt.exp = A_StructExp(0, A_VarExp(0, obj->name), NULL);
      p = p->u.structt.exp;
    } else {
      sym = CSSGet();
      if ((*x)->type->form != CSGArray) CSSError("array type expected");
      y = malloc(sizeof(CSGNodeDesc));
      assert(y != NULL);
      p->kind = A_arrayExp;
      p->u.structt.exp = Expression(&y);
      p = p->u.structt.exp;
      CSGIndex(x, y);
      if (sym != CSSrbrak) CSSError("']' expected");
      sym = CSSGet();
    }
  }
  if(root->u.structt.exp == NULL){
      root = root->u.structt.var;
  }
  return root;
}


static A_exp AssignmentM(CSGNode *x)
{
  CSGNode y;

  A_exp p, p2, r;
  assert(x != NULL);
  assert(*x != NULL);
  // CSSident already consumed
  y = malloc(sizeof(CSGNodeDesc));
  assert(y != NULL);
  p = DesignatorM(x);
  if (sym != CSSbecomes) CSSError("'=' expected");
  sym = CSSGet();
  p2 = Expression(&y);
  CSGStore(*x, y);
  r = A_AssignExp(0, p, p2);
  if (sym != CSSsemicolon) CSSError("';' expected");
  sym = CSSGet();
  return r;
}


#define create_ps(a, b, c) \
    a = malloc(sizeof(*a)); \
    a->exp = b; \
    a->next = c;

static A_expList ExpList(CSGNode proc)
{
  register CSGNode curr;
  CSGNode x;
  A_expList ps, cur, tmp;

  x = malloc(sizeof(CSGNodeDesc));
  assert(x != NULL);
  curr = proc->dsc;
  create_ps(ps, Expression(&x), NULL);
  cur = ps;
  if ((curr == NULL) || (curr->dsc != proc)) CSSError("too many parameters");
  if (x->type != curr->type) CSSError("incorrect type");
  CSGParameter(&x, curr->type, curr->class);
  curr = curr->next;
  while (sym == CSScomma) {
    x = malloc(sizeof(CSGNodeDesc));
    assert(x != NULL);
    sym = CSSGet();
    create_ps(tmp, Expression(&x), NULL);
    cur->next = tmp;
    cur = cur->next;
    if ((curr == NULL) || (curr->dsc != proc)) CSSError("too many parameters");
    if (x->type != curr->type) CSSError("incorrect type");
    CSGParameter(&x, curr->type, curr->class);
    curr = curr->next;
  }
  if ((curr != NULL) && (curr->dsc == proc)) CSSError("too few parameters");
  return ps;
}

#undef create_ps
static A_exp ProcedureCallM(CSGNode obj, CSGNode *x)
{
  CSGNode y;

  // CSSident already consumed
  CSGMakeNodeDesc(x, obj);
  S_symbol func = (*x)->name;
  A_expList args;
  if (sym != CSSlparen) CSSError("'(' expected");
  sym = CSSGet();
  if ((*x)->class == CSGSProc) {
    y = malloc(sizeof(CSGNodeDesc));
    assert(y != NULL);
    if ((*x)->val == 1) {
      if (sym != CSSident) CSSError("identifier expected");
      obj = FindObj(&globscope, &CSSid);
      if (obj == NULL) CSSError("unknown identifier");
      CSGMakeNodeDesc(&y, obj);
      sym = CSSGet();  // consume ident before calling Designator
      args = malloc(sizeof(*args));
      args->exp = DesignatorM(&y);
      args->next = NULL;
    } else if ((*x)->val == 2) {
      args = malloc(sizeof(*args));
      args->exp = Expression(&y);
      args->next = NULL;
    } else {
        args = NULL;
    }
    CSGIOCall(*x, y);
  } else {
    assert((*x)->type == NULL);
    if (sym != CSSrparen) {
      args = ExpList(obj);
    } else {
      args = NULL;
      if ((obj->dsc != NULL) && (obj->dsc->dsc == obj))
          CSSError("too few parameters");
    }
    CSGCall(*x);
  }
  if (sym != CSSrparen) CSSError("')' expected");
  sym = CSSGet();
  if (sym != CSSsemicolon) CSSError("';' expected");
  sym = CSSGet();
  return A_Call(0, func, args);
}


static A_expList StatementSequence(void);


static A_exp IfStatement(void)
{
  CSGNode label;
  CSGNode x;
  A_expList iff, elsee = NULL;
  A_exp test = NULL;

  x = malloc(sizeof(CSGNodeDesc));
  assert(x != NULL);
  assert(sym == CSSif);
  sym = CSSGet();
  CSGInitLabel(&label);
  if (sym != CSSlparen) CSSError("'(' expected");
  sym = CSSGet();
  test = Expression(&x);
  CSGTestBool(&x);
  CSGFixLink(x->false);
  if (sym != CSSrparen) CSSError("')' expected");
  sym = CSSGet();
  if (sym != CSSlbrace) CSSError("'{' expected");
  sym = CSSGet();
  iff = StatementSequence();
  if (sym != CSSrbrace) CSSError("'}' expected");
  sym = CSSGet();
  if (sym == CSSelse) {
    sym = CSSGet();
    CSGFJump(&label);
    CSGFixLink(x->true);
    if (sym != CSSlbrace) CSSError("'{' expected");
    sym = CSSGet();
    elsee = StatementSequence();
    if (sym != CSSrbrace) CSSError("'}' expected");
    sym = CSSGet();
  } else {
    CSGFixLink(x->true);
  }
  CSGFixLink(label);
  return A_IfExp(0, test, iff, elsee);
}


static A_exp WhileStatement(void)
{
  CSGNode label;
  CSGNode x;
  A_exp test;
  A_expList body;

  x = malloc(sizeof(CSGNodeDesc));
  assert(x != NULL);
  assert(sym == CSSwhile);
  sym = CSSGet();
  if (sym != CSSlparen) CSSError("'(' expected");
  sym = CSSGet();
  CSGSetLabel(&label);
  test = Expression(&x);
  CSGTestBool(&x);
  CSGFixLink(x->false);
  if (sym != CSSrparen) CSSError("')' expected");
  sym = CSSGet();
  if (sym != CSSlbrace) CSSError("'{' expected");
  sym = CSSGet();
  body = StatementSequence();
  if (sym != CSSrbrace) CSSError("'}' expected");
  sym = CSSGet();
  CSGBJump(label);
  CSGFixLink(x->true);
  return A_WhileExp(0, test, body);
}


static A_exp Statement(void)
{
  register CSGNode obj;
  CSGNode x;
  A_exp p = NULL;

  switch (sym) {
    case CSSif: 
        p = IfStatement(); 
        break;
    case CSSwhile: 
        p = WhileStatement(); 
        break;
    case CSSident:
      obj = FindObj(&globscope, &CSSid);
      if (obj == NULL) CSSError("unknown identifier");
      sym = CSSGet();
      x = malloc(sizeof(CSGNodeDesc));
      assert(x != NULL);
      if (sym == CSSlparen) {
        p = ProcedureCallM(obj, &x);
      } else {
        CSGMakeNodeDesc(&x, obj);
        p = AssignmentM(&x);
      }
      break;
    case CSSsemicolon: 
          break;  /* empty statement */
    default: CSSError("unknown statement");
  }
  return p;
}


static A_expList StatementSequence(void)
{
  A_expList root = malloc(sizeof(*root));
  A_expList it = root;
  A_expList prev = NULL;
  while (sym != CSSrbrace) {
    it->exp = Statement();
    it->next = malloc(sizeof(*it));
    prev = it;
    it = it->next;
  }
  free(prev->next);
  prev->next = NULL;
  return root;
}


/*************************************************************************/


static void FPSection(CSGNode *root, int *paddr)
{
  register CSGNode obj;
  CSGType type;

  Type(&type);
  if (type != CSGlongType) CSSError("only basic type formal parameters allowed");
  if (sym != CSSident) CSSError("identifier expected");
  obj = AddToList(root, &CSSid);
  sym = CSSGet();
  if (sym == CSSlbrak) CSSError("no array parameters allowed");
  InitObj(obj, CSGVar, *root, type, 0);
  *paddr += type->size; 
}


static A_exp FormalParameters(CSGNode *root)
{
  register CSGNode curr;
  int paddr;

  paddr = 16;
  FPSection(root, &paddr);
  while (sym == CSScomma) {
    sym = CSSGet();
    FPSection(root, &paddr);
  }
  curr = (*root)->next;
  while (curr != NULL) {
    paddr -= curr->type->size;
    curr->val = paddr;
    curr = curr->next;
  }
}


static void ProcedureHeading(CSGNode *proc)
{
  CSSIdent name;

  if (sym != CSSident) CSSError("function name expected");
  strcpy(name, CSSid);
  *proc = AddToList(&globscope, &name);
  InitProcObj(*proc, CSGProc, NULL, NULL, CSGpc);
  CSGAdjustLevel(1);
  sym = CSSGet();
  if (sym != CSSlparen) CSSError("'(' expected");
  sym = CSSGet();
  if (sym != CSSrparen) {
    FormalParameters(proc);
  }
  if (sym != CSSrparen) CSSError("')' expected");
  sym = CSSGet();
  if (strcmp(name, "main") == 0) CSGEntryPoint();
}


static A_expList ProcedureBody(CSGNode *proc)
{
  register int returnsize;
  register CSGNode curr;
  A_expList p;

  tos = 0;
  while ((sym == CSSconst) || (sym == CSSstruct) || ((sym == CSSident) && (strcmp(CSSid, "long") == 0))) {
    if (sym == CSSconst) {
      ConstantDeclaration(proc);
    } else {
      VariableDeclaration(proc);
    }
  }
  assert((*proc)->dsc == NULL);
  (*proc)->dsc = (*proc)->next;
  if (-tos > 32768) CSSError("maximum stack frame size of 32kB exceeded");
  CSGEnter(-tos);
  returnsize = 0;
  curr = (*proc)->dsc;
  while ((curr != NULL) && (curr->dsc == *proc)) {
    returnsize += 8;
    curr = curr->next;
  }
  p = StatementSequence();
  if (strcmp((*proc)->name, "main") == 0) {
    CSGClose();
  } else {
    CSGReturn(returnsize);
  }
  CSGAdjustLevel(-1);
  return p;
}


static A_exp ProcedureDeclaration(void)
{
  CSGNode proc;
  A_expList tmp;
  
  assert(sym == CSSvoid);
  sym = CSSGet();
  ProcedureHeading(&proc);
  if (sym != CSSlbrace) CSSError("'{' expected");
  sym = CSSGet();
  tmp = ProcedureBody(&proc);
  if (sym != CSSrbrace) CSSError("'}' expected");
  sym = CSSGet();
  proc->next = NULL;  // cut off rest of list
  return A_Call(0, proc->name, tmp);
}

static void Program(void)
{
  A_exp root;	
  CSGOpen();
  tos = 32768;
  instruct = 0;
  while ((sym != CSSvoid) && (sym != CSSeof)) {
    if (sym == CSSconst) {
      ConstantDeclaration(&globscope);
    } else {
      VariableDeclaration(&globscope);
    }
  }
  CSGStart(32768 - tos);
  if (sym != CSSvoid) CSSError("procedure expected");
  while (sym == CSSvoid) {
    root = ProcedureDeclaration();
    push(&exps, root);
  }
  if (sym != CSSeof) CSSError("unrecognized characters at end of file");
}


/*************************************************************************/


static void InsertObj(CSGNode *root, signed char class, CSGType type, CSSIdent name, long long val)
{
  register CSGNode curr;

  if (*root == NULL) {
    *root = malloc(sizeof(CSGNodeDesc));
    if (*root == NULL) CSSError("out of memory");
    curr = *root;
  } else {
    curr = *root;
    if (strcmp(curr->name, name) == 0) CSSError("duplicate symbol");
    while (curr->next != NULL) {
      curr = curr->next;
      if (strcmp(curr->name, name) == 0) CSSError("duplicate symbol");
    }
    curr->next = malloc(sizeof(CSGNodeDesc));
    assert(curr->next != NULL);
    curr = curr->next;
    if (curr == NULL) CSSError("out of memory");
  }
  curr->next = NULL;
  curr->class = class;
  curr->type = type;
  strcpy(curr->name, name);
  curr->val = val;
  curr->dsc = NULL;
  curr->lev = 0;
}


static void Compile(char *filename)
{
  printf("compiling %s\n", filename);

  globscope = NULL;
  InsertObj(&globscope, CSGTyp, CSGlongType, "long", 8);
  InsertObj(&globscope, CSGSProc, NULL, "ReadLong", 1);
  InsertObj(&globscope, CSGSProc, NULL, "WriteLong", 2);
  InsertObj(&globscope, CSGSProc, NULL, "WriteLine", 3);

  CSSInit(filename);
  sym = CSSGet();
  Program();
}


/*************************************************************************/
void print_space(int times) {
    printf("%02d", times);
    for (int i = 0; i < times; i++)
        printf(" ");
}
void print_tree(int level, A_exp root){
    if(!root) return;
    print_space(level * 5);
    switch(root->kind){
        case A_callExp:
            printf("%s\n", root->u.call.func);
            for(A_expList it = root->u.call.args; it; it = it->next){
                print_tree(level + 1, it->exp);
            }
            break;
        case A_intExp:
            printf("%d\n", root->u.intt);
            break;
        case A_varExp:
            printf("%s\n", root->u.var);
            break;
        case A_opExp:
        case A_op1Exp: // we don't really need this one.
            switch(root->u.op.oper){
                case CSStimes:
                    printf("*\n");
                    break;
                case CSSdiv:
                    printf("/\n");
                    break;
                case CSSmod:
                    printf("%%\n");
                    break;
                case CSSplus:
                    printf("+\n");
                    break;

                case CSSminus:
                    printf("-\n");
                    break;
                case CSSeql:
                    printf("=\n");
                    break;
                case CSSneq:
                    printf("!=\n");
                    break;
                case CSSlss:
                    printf("<\n");
                    break;
                case CSSleq:
                    printf("<=\n");
                    break;
                case CSSgtr:
                    printf(">\n");
                    break;
                case CSSgeq:
                    printf(">=\n");
                    break;
                case CSSperiod:
                    printf(".\n");
                    break;
                case CSScomma:
                    printf(",\n");
                    break;
                case CSSrparen:
                    printf(")\n");
                    break;
                case CSSrbrak:
                    printf("]\n");
                    break;
                case CSSrbrace:
                    printf("}\n");
                    break;
                case CSSlparen: printf("(\n"); break;
                case CSSlbrak: printf("[\n"); break;
                case CSSlbrace: printf("{\n"); break;
                case CSSident: printf("ident\n"); break;
                case CSSsemicolon: printf("semicolon\n"); break;
                case CSSelse: printf("else\n"); break;
                case CSSif: printf("if\n"); break;

                default:
                  break;
            }
            print_tree(level + 1, root->u.op.left);
            if(root->kind != A_op1Exp) print_tree(level + 1, root->u.op.right);
            break;
        case A_assignExp:
            printf("=\n");
            print_tree(level + 1, root->u.assign.var);
            print_tree(level + 1, root->u.assign.exp);
            break;
        case A_ifExp:
            printf("If\n");
            print_tree(level + 1, root->u.iff.test);
            for(A_expList it = root->u.iff.then; it; it = it->next){
                print_tree(level + 1, it->exp);
            }
            if(root->u.iff.elsee) {
                for(A_expList it = root->u.iff.elsee; it; it = it->next){
                    print_tree(level + 1, it->exp);
                }
            }
            break;
        case A_whileExp:
            
            printf("while\n");
            print_tree(level + 1, root->u.whilee.test);
            for(A_expList it = root->u.whilee.body; it; it = it->next){
                print_tree(level + 1, it->exp);
            }
            break;
        case A_arrayExp:
            printf("array\n");
            print_tree(level + 1, root->u.array.var);
            print_tree(level + 1, root->u.array.exp);
            break;
        case A_structExp:
            if(root->u.structt.exp){
                printf(".\n");
                print_tree(level + 1, root->u.structt.var);
                print_tree(level + 1, root->u.structt.exp);
            } else {
                /* fixed bug it's shouldn't enter here */
                assert(1 == 2);
                printf("struct_%s\n", root->u.structt.var->u.var);
            }
            break;
        default:
            break;
    }
}



int main(int argc, char *argv[])
{
//   Block* blocks;
  CSGInit();
  if (argc >= 2) {
    Compile(argv[1]);
  } else {
    Compile("test.c");
  }
 
  for(A_expList it = exps; it; it = it->next){
      // printf(">> %s\n",it->exp->u.call.func);
      print_tree(0, it->exp);
      
  }
  traverse(exps);
  
  
  
  return 0;
}
