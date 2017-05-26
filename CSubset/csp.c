/*  C Subset Parser  9-15-04  Martin Burtscher  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "css.h"
#include "csg.h"
#include "ast.h"

#define CHAR_SIZE 16


static int sym;
static int instruct;
static int tos;
static CSGNode globscope;


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
	}
	else {
		curr = *root;
		while (((curr->lev != CSGcurlev) || (strcmp(curr->name, *id) != 0)) && (curr->next != NULL)) {
			curr = curr->next;
		}
		if ((strcmp(curr->name, *id) == 0) && (curr->lev == CSGcurlev)) {
			CSSError("duplicate identifier");
		}
		else {
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


static void Expression(CSGNode *x, A_exp *ast_root);
static void DesignatorM(CSGNode *x, A_exp *ast_root);

// FINISHED
static void Factor(CSGNode *x, A_exp *ast_root)
{
	register CSGNode obj;

	switch (sym) {
	//DESIGNATOR
	case CSSident:
		obj = FindObj(&globscope, &CSSid);
		if (obj == NULL) CSSError("unknown identifier");
		CSGMakeNodeDesc(x, obj);
		sym = CSSGet();  // consume ident before calling Designator

		(*ast_root)->kind = A_varExp;
		(*ast_root)->next = NULL;
		(*ast_root)->desig_next = NULL;
		(*ast_root)->u.var = malloc(sizeof(A_var));
		(*ast_root)->u.var->name = malloc(sizeof(S_symbol));
		(*ast_root)->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
		strcpy((*ast_root)->u.var->name->name, CSSid);
		// DESIGNATOR
		DesignatorM(x, ast_root);

		break;

	// NUMBER
	case CSSnumber:

		(*ast_root)->kind = A_intExp;
		(*ast_root)->u.intt = CSSval;
		CSGMakeConstNodeDesc(x, CSGlongType, CSSval);
		sym = CSSGet();
		break;

	// ( EXPRESSION )
	case CSSlparen:
		sym = CSSGet();

		// EXPRESSION
		Expression(x, ast_root);
		if (sym != CSSrparen) CSSError("')' expected");
		sym = CSSGet();
		break;
	default: CSSError("factor expected"); break;
	}
}

static void Term(CSGNode *x, A_exp *ast_root)
{
	register int op;
	CSGNode y;

	// Factor
	Factor(x, ast_root);
	

	while ((sym == CSStimes) || (sym == CSSdiv) || (sym == CSSmod)) {

		A_oper operI = malloc(sizeof(A_oper));
		operI->op = malloc(sizeof(S_symbol));
		operI->op->name = (char*) malloc(sizeof(char) * CHAR_SIZE);

		if (sym == CSStimes)
			strcpy(operI->op->name, "*");
		else if (sym == CSSdiv)
			strcpy(operI->op->name, "/");
		else
			strcpy(operI->op->name, "%");

		A_exp factorI = malloc(sizeof(A_exp));
		factorI->next = NULL;
		factorI->desig_next = NULL;

		op = sym;
		sym = CSSGet();
		y = malloc(sizeof(CSGNodeDesc));
		assert(y != NULL);

		// Factor
		Factor(&y, &factorI);
		CSGOp2(op, x, y);

		A_exp expI = malloc(sizeof(A_exp));
		expI->next = NULL;
		expI->desig_next = NULL;
		expI->kind = A_opExp;
		expI->u.op.left = (*ast_root);
		expI->u.op.oper = operI;
		expI->u.op.right = factorI;
		(*ast_root) =expI;
	}
}

static void SimpleExpression(CSGNode *x, A_exp *ast_root)
{
	register int op;
	CSGNode y;

	(*ast_root)->kind = A_opExp;
	A_oper first_oper = malloc(sizeof(A_oper));
	A_exp left_exp = malloc(sizeof(A_exp));
	left_exp->next = NULL;
	left_exp->desig_next = NULL;
	A_exp right_exp = malloc(sizeof(A_exp));
	right_exp->next = NULL;
	right_exp->desig_next = NULL;

	int isHaveInfrontSign = 0;

	if ((sym == CSSplus) || (sym == CSSminus)) {

		isHaveInfrontSign = 1;

		// left exp = 0
		left_exp->kind = A_intExp;
		left_exp->u.intt = 0;

		// oper = +, -
		first_oper->op = malloc(sizeof(S_symbol));
		first_oper->op->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
		if (sym == CSSplus)
			strcpy(first_oper->op->name, "+");
		else
			strcpy(first_oper->op->name, "-");

		op = sym;
		sym = CSSGet();
		
		// right exp
		// TERM
		Term(x, &right_exp);

		CSGOp1(op, x);

		(*ast_root)->u.op.left = left_exp;
		(*ast_root)->u.op.oper = first_oper;
		(*ast_root)->u.op.right = right_exp;
	}
	else {

		// TERM
		Term(x, &left_exp);
	}

	if ((sym == CSSplus) || (sym == CSSminus))
	{
		// Concat Term
		while ((sym == CSSplus) || (sym == CSSminus)) {
		
			A_oper operI = malloc(sizeof(A_oper));
			operI->op = malloc(sizeof(S_symbol));
			operI->op->name = (char*) malloc(sizeof(char)*CHAR_SIZE);
			if (sym == CSSplus)
				strcpy(operI->op->name, "+");
			else
				strcpy(operI->op->name, "-");

			A_exp termI = malloc(sizeof(A_exp));
			termI->next = NULL;
			termI->desig_next = NULL;

			op = sym;
			sym = CSSGet();
			y = malloc(sizeof(CSGNodeDesc));
			assert(y != NULL);

			// TERM
			Term(&y, &termI);

			CSGOp2(op, x, y);


			A_exp expI = malloc(sizeof(A_exp));
			expI->next = NULL;
			expI->desig_next = NULL;
			expI->kind = A_opExp;

			expI->u.op.left = (*ast_root);
			expI->u.op.oper = operI;
			expI->u.op.right = termI;
			(*ast_root) =expI;

		}
	}
	else
	{
		if (isHaveInfrontSign == 0)
			// Single Term
			(*ast_root) = left_exp;
	}
	
}

static void EqualityExpr(CSGNode *x, A_exp *ast_root)
{
	register int op;
	CSGNode y;

	A_exp left_simp_exp = malloc(sizeof(A_exp));
	left_simp_exp->next = NULL;
	left_simp_exp->desig_next = NULL;
	A_exp right_simp_exp = malloc(sizeof(A_exp));
	right_simp_exp->next = NULL;
	right_simp_exp->desig_next = NULL;

	// SIMPLE EXP
	SimpleExpression(x, &left_simp_exp);

	if ((sym == CSSlss) || (sym == CSSleq) || (sym == CSSgtr) || (sym == CSSgeq)) {

		(*ast_root)->kind = A_opExp;
		(*ast_root)->u.op.oper = malloc(sizeof(A_oper));
		(*ast_root)->u.op.oper->op = malloc(sizeof(S_symbol));
		(*ast_root)->u.op.oper->op->name = (char*) malloc(sizeof(char) * CHAR_SIZE);

		switch (sym)
		{
		case CSSlss:
			strcpy((*ast_root)->u.op.oper->op->name, "<");
			break;
		case CSSleq:
			strcpy((*ast_root)->u.op.oper->op->name, "<=");
			break;
		case CSSgtr:
			strcpy((*ast_root)->u.op.oper->op->name, ">");
			break;
		case CSSgeq:
			strcpy((*ast_root)->u.op.oper->op->name, ">=");
			break;
		}

		y = malloc(sizeof(CSGNodeDesc));
		assert(y != NULL);
		op = sym;
		sym = CSSGet();

		// SIMPLE EXP
		SimpleExpression(&y, &right_simp_exp);

		CSGRelation(op, x, y);

		(*ast_root)->u.op.left = left_simp_exp;
		(*ast_root)->u.op.right = right_simp_exp;
	}
	else
	{
		(*ast_root) =left_simp_exp;
	}
}

static void Expression(CSGNode *x, A_exp *ast_root)
{
	register int op;
	CSGNode y;

	A_exp left_eql_exp = malloc(sizeof(A_exp));
	left_eql_exp->next = NULL;
	left_eql_exp->desig_next = NULL;
	A_exp right_eql_exp = malloc(sizeof(A_exp));
	right_eql_exp->next = NULL;
	right_eql_exp->desig_next = NULL;

	// EQUAL EXPR
	EqualityExpr(x, &left_eql_exp);

	if ((sym == CSSeql) || (sym == CSSneq)) {
		(*ast_root)->kind = A_opExp;
		(*ast_root)->u.op.oper = malloc(sizeof(A_oper));
		(*ast_root)->u.op.oper->op = malloc(sizeof(S_symbol));
		(*ast_root)->u.op.oper->op->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
		if (sym == CSSeql)
			strcpy((*ast_root)->u.op.oper->op->name, "==");
		else
			strcpy((*ast_root)->u.op.oper->op->name, "!=");

		op = sym;
		sym = CSSGet();
		y = malloc(sizeof(CSGNodeDesc));
		assert(y != NULL);

		// EQUAL EXPR
		EqualityExpr(&y, &right_eql_exp);

		CSGRelation(op, x, y);

		(*ast_root)->u.op.left = left_eql_exp;
		(*ast_root)->u.op.right = right_eql_exp;
	}
	else
	{
		(*ast_root) =left_eql_exp;
	}
}

static void ConstExpression(CSGNode *expr, A_exp *ast_root)
{
	Expression(expr, ast_root);
	if ((*expr)->class != CSGConst) CSSError("constant expression expected");
}

/*************************************************************************/

static void VariableDeclaration(CSGNode *root, A_exp *ast_root);

static void FieldList(CSGType type)
{
	register CSGNode curr;

	A_exp tmpAexp = malloc(sizeof(A_exp));
	tmpAexp->next = NULL;
	tmpAexp->desig_next = NULL;

	// VARIABLE DECLAR
	VariableDeclaration(&(type->fields), &tmpAexp);
	while (sym != CSSrbrace) {
		//VARIABLE DECLAR
		VariableDeclaration(&(type->fields), &tmpAexp);
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
	}
	else {
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
	}
	else {
		if (sym != CSSident) CSSError("identifier expected");
		obj = FindObj(&globscope, &CSSid);
		sym = CSSGet();
		if (obj == NULL) CSSError("unknown type");
		if (obj->class != CSGTyp) CSSError("type expected");
		*type = obj->type;
	}
}

static void RecurseArray(CSGType *type, A_exp *ast_root)
{
	register CSGType typ;
	CSGNode expr;

	expr = malloc(sizeof(CSGNodeDesc));
	assert(expr != NULL);
	assert(sym == CSSlbrak);
	sym = CSSGet();

	// CONST EXP
	ConstExpression(&expr, ast_root);


	if (expr->type != CSGlongType) CSSError("constant long expression required");
	if (sym != CSSrbrak) CSSError("']' expected");
	sym = CSSGet();
	if (sym == CSSlbrak) {

		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;

		(*ast_root)->desig_next = tmp;

		// RECUR ARR
		RecurseArray(type, ast_root);

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

static void IdentArray(CSGNode *root, CSGType type, A_exp *ast_root)
{
	register CSGNode obj;

	if (sym != CSSident) CSSError("identifier expected");
	// IDENT
	obj = AddToList(root, &CSSid);
	strcpy((*ast_root)->u.var->name->name, CSSid);

	sym = CSSGet();
	if (sym == CSSlbrak) {

		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;

		(*ast_root)->desig_next= tmp;

		// RECUR ARR
		RecurseArray(&type, &(*ast_root)->desig_next);
	}
	if (instruct == 0) tos -= type->size;
	InitObj(obj, CSGVar, NULL, type, tos);
}

static void IdentList(CSGNode *root, CSGType type, A_expList *ast_root_list)
{
	A_exp tmp = malloc(sizeof(A_exp));
	tmp->kind = A_varExp;
	tmp->u.var = malloc(sizeof(A_var));
	tmp->u.var->name = malloc(sizeof(S_symbol));
	tmp->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
	tmp->next = NULL;
	tmp->desig_next = NULL;

	(*ast_root_list)->start = tmp;
	(*ast_root_list)->end = tmp;

	// IDENT ARR
	IdentArray(root, type, &(*ast_root_list)->end);
	
	while (sym == CSScomma) {

		A_exp tmpi = malloc(sizeof(A_exp));
		tmpi->kind = A_varExp;
		tmpi->u.var = malloc(sizeof(A_var));
		tmpi->u.var->name = malloc(sizeof(S_symbol));
		tmpi->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
		tmpi->next = NULL;
		tmpi->desig_next = NULL;

		(*ast_root_list)->end->next = tmp;
		(*ast_root_list)->end = tmp;

		sym = CSSGet();

		// IDENT ARR
		IdentArray(root, type, &(*ast_root_list)->end);
	}
}

static void VariableDeclaration(CSGNode *root, A_exp *ast_root)
{
	CSGType type;

	Type(&type);

	(*ast_root)->kind = A_arrayExp;
	(*ast_root)->u.arr = malloc(sizeof(A_expList));

	// IDENT LIST
	IdentList(root, type, &(*ast_root)->u.arr);

	if (sym != CSSsemicolon) CSSError("';' expected");
	sym = CSSGet();
}

static void ConstantDeclaration(CSGNode *root, A_exp *ast_root)
{
	(*ast_root)->kind = A_assignExp;
	(*ast_root)->u.assign.var = malloc(sizeof(A_exp));
	(*ast_root)->u.assign.var->kind = A_varExp;
	(*ast_root)->u.assign.var->u.var = malloc(sizeof(A_exp));
	(*ast_root)->u.assign.var->u.var->name = malloc(sizeof(S_symbol));
	(*ast_root)->u.assign.var->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);

	(*ast_root)->u.assign.exp = malloc(sizeof(A_exp));


	register CSGNode obj;
	CSGType type;
	CSGNode expr;
	CSSIdent id;

	expr = malloc(sizeof(CSGNodeDesc));
	assert(expr != NULL);
	assert(sym == CSSconst);
	sym = CSSGet();

	// TYPE
	Type(&type);

	if (type != CSGlongType) CSSError("only long supported");
	if (sym != CSSident) CSSError("identifier expected");

	// IDENT
	strcpy(id, CSSid);
	strcpy((*ast_root)->u.assign.var->u.var->name->name, CSSid);
	
	sym = CSSGet();
	if (sym != CSSbecomes) CSSError("'=' expected");
	sym = CSSGet();

	// CONSTANT EXP
	ConstExpression(&expr, &(*ast_root)->u.assign.exp);

	if (expr->type != CSGlongType) CSSError("constant long expression required");
	obj = AddToList(root, &id);
	InitObj(obj, CSGConst, NULL, type, expr->val);
	if (sym != CSSsemicolon) CSSError("';' expected");
	sym = CSSGet();
}

/*************************************************************************/

static void DesignatorM(CSGNode *x, A_exp *ast_root)
{
	register CSGNode obj;
	CSGNode y;

	// CSSident already consumed
	while ((sym == CSSperiod) || (sym == CSSlbrak)) {
		// Ident
		if (sym == CSSperiod) {
			sym = CSSGet();
			if ((*x)->type->form != CSGStruct) CSSError("struct type expected");
			if (sym != CSSident) CSSError("field identifier expected");
			obj = FindObj(&(*x)->type->fields, &CSSid);

			A_exp tmp = malloc(sizeof(A_exp));
			tmp->kind = A_varExp;
			tmp->u.var = malloc(sizeof(A_var));
			tmp->u.var->name = malloc(sizeof(S_symbol));
			tmp->u.var->name->name = (char*)malloc(sizeof(char) * CHAR_SIZE);
			strcpy(tmp->u.var->name->name, CSSid);
			tmp->next = NULL;
			tmp->desig_next = NULL;

			A_exp pnt = (*ast_root);
			pnt->next = NULL;
			pnt->desig_next = NULL;

			while (pnt->desig_next != NULL)
				pnt = pnt->desig_next;

			pnt->next = tmp;

			sym = CSSGet();
			if (obj == NULL) CSSError("unknown identifier");
			CSGField(x, obj);
		}
		// Expression
		else {
			sym = CSSGet();
			if ((*x)->type->form != CSGArray) CSSError("array type expected");
			y = malloc(sizeof(CSGNodeDesc));
			assert(y != NULL);

			A_exp tmp = malloc(sizeof(A_exp));
			tmp->next = NULL;
			tmp->desig_next = NULL;

			A_exp pnt = (*ast_root);

			while (pnt->desig_next != NULL)
				pnt = pnt->desig_next;
			pnt->desig_next = tmp;
			
			// EXPRESSION
			Expression(&y, &pnt->next);

			CSGIndex(x, y);
			if (sym != CSSrbrak) CSSError("']' expected");
			sym = CSSGet();
		}
	}
}

static void AssignmentM(CSGNode *x, A_exp *ast_root)
{
	CSGNode y;

	assert(x != NULL);
	assert(*x != NULL);
	// CSSident already consumed
	y = malloc(sizeof(CSGNodeDesc));
	assert(y != NULL);

	// DESIGNATOR
	DesignatorM(x, &(*ast_root)->u.assign.var);

	if (sym != CSSbecomes) CSSError("'=' expected");
	sym = CSSGet();

	// EXPRESSION
	Expression(&y, &(*ast_root)->u.assign.exp);

	CSGStore(*x, y);
	if (sym != CSSsemicolon) CSSError("';' expected");
	sym = CSSGet();
}

static void ExpList(CSGNode proc, A_expList *ast_root_list)
{
	A_exp tmp = malloc(sizeof(A_exp));
	tmp->next = NULL;
	(*ast_root_list)->start = tmp;
	(*ast_root_list)->end = tmp;

	register CSGNode curr;
	CSGNode x;

	x = malloc(sizeof(CSGNodeDesc));
	assert(x != NULL);
	curr = proc->dsc;

	// EXPRESSION
	Expression(&x, &(*ast_root_list)->end);


	if ((curr == NULL) || (curr->dsc != proc)) CSSError("too many parameters");
	if (x->type != curr->type) CSSError("incorrect type");
	CSGParameter(&x, curr->type, curr->class);
	curr = curr->next;
	while (sym == CSScomma) {
		x = malloc(sizeof(CSGNodeDesc));
		assert(x != NULL);
		sym = CSSGet();


		A_exp tmpi = malloc(sizeof(A_exp));
		(*ast_root_list)->end->next = tmpi;
		(*ast_root_list)->end = tmpi;
		// EXPRESSION
		Expression(&x, &(*ast_root_list)->end);

		if ((curr == NULL) || (curr->dsc != proc)) CSSError("too many parameters");
		if (x->type != curr->type) CSSError("incorrect type");
		CSGParameter(&x, curr->type, curr->class);
		curr = curr->next;
	}
	if ((curr != NULL) && (curr->dsc == proc)) CSSError("too few parameters");
}

static void ProcedureCallM(CSGNode obj, CSGNode *x, A_expList *ast_root_list)
{
	CSGNode y;

	// CSSident already consumed
	CSGMakeNodeDesc(x, obj);
	if (sym != CSSlparen) CSSError("'(' expected");
	sym = CSSGet();

	// S...PROCEDURAL. Special?
	if ((*x)->class == CSGSProc) {
		
		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;
		(*ast_root_list)->start = tmp;
		(*ast_root_list)->end = tmp;

		y = malloc(sizeof(CSGNodeDesc));
		assert(y != NULL);
		if ((*x)->val == 1) {
			if (sym != CSSident) CSSError("identifier expected");
			obj = FindObj(&globscope, &CSSid);
			if (obj == NULL) CSSError("unknown identifier");
			CSGMakeNodeDesc(&y, obj);
			sym = CSSGet();  // consume ident before calling Designator

			(*ast_root_list)->end->kind = A_varExp;
			(*ast_root_list)->end->u.var = malloc(sizeof(A_var));
			(*ast_root_list)->end->u.var->name = malloc(sizeof(S_symbol));
			(*ast_root_list)->end->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
			(*ast_root_list)->end->next = NULL;
			(*ast_root_list)->end->desig_next = NULL;
			strcpy((*ast_root_list)->end->u.var->name->name, CSSid);
			// DESIGNATOR 
			DesignatorM(&y, &(*ast_root_list)->end);

		}
		else if ((*x)->val == 2) {

			// EXPRESSION
			Expression(&y, &(*ast_root_list)->end);
		}
		CSGIOCall(*x, y);
	}
	// NORMAL PROCEDURAL
	else {
		assert((*x)->type == NULL);
		if (sym != CSSrparen) {

			// EXPRESSION LIST
			ExpList(obj, ast_root_list);
		}
		else {
			if ((obj->dsc != NULL) && (obj->dsc->dsc == obj)) CSSError("too few parameters");
		}
		CSGCall(*x);
	}
	if (sym != CSSrparen) CSSError("')' expected");
	sym = CSSGet();
	if (sym != CSSsemicolon) CSSError("';' expected");
	sym = CSSGet();
}

static void StatementSequence(A_exp *ast_root);

static void IfStatement(A_exp *ast_root)
{
	(*ast_root)->kind = A_ifExp;
	(*ast_root)->u.iff.test = malloc(sizeof(A_exp));
	(*ast_root)->u.iff.then = malloc(sizeof(A_exp));
	(*ast_root)->u.iff.elsee = malloc(sizeof(A_exp));

	CSGNode label;
	CSGNode x;

	x = malloc(sizeof(CSGNodeDesc));
	assert(x != NULL);
	assert(sym == CSSif);
	sym = CSSGet();
	CSGInitLabel(&label);
	if (sym != CSSlparen) CSSError("'(' expected");
	sym = CSSGet();
	
	// IF
	// EXPRESSION
	Expression(&x, &(*ast_root)->u.iff.test);

	CSGTestBool(&x);
	CSGFixLink(x->false);
	if (sym != CSSrparen) CSSError("')' expected");
	sym = CSSGet();
	if (sym != CSSlbrace) CSSError("'{' expected");
	sym = CSSGet();

	// THEN
	// STATEMENT SEQUENCE
	StatementSequence(&(*ast_root)->u.iff.then);

	if (sym != CSSrbrace) CSSError("'}' expected");
	sym = CSSGet();

	if (sym == CSSelse) {
		sym = CSSGet();
		CSGFJump(&label);
		CSGFixLink(x->true);
		if (sym != CSSlbrace) CSSError("'{' expected");
		sym = CSSGet();

		// ELSE
		// STATEMENT SEQUENCE
		StatementSequence(&(*ast_root)->u.iff.elsee);

		if (sym != CSSrbrace) CSSError("'}' expected");
		sym = CSSGet();
	}
	else {
		CSGFixLink(x->true);
	}
	CSGFixLink(label);
}

static void WhileStatement(A_exp *ast_root)
{
	(*ast_root)->kind = A_whileExp;
	(*ast_root)->u.whilee.test = malloc(sizeof(A_exp));
	(*ast_root)->u.whilee.body = malloc(sizeof(A_exp));

	CSGNode label;
	CSGNode x;

	x = malloc(sizeof(CSGNodeDesc));
	assert(x != NULL);
	assert(sym == CSSwhile);
	sym = CSSGet();
	if (sym != CSSlparen) CSSError("'(' expected");
	sym = CSSGet();
	CSGSetLabel(&label);

	(*ast_root)->u.whilee.test->kind = A_opExp;
	// EXPRESSION
	Expression(&x, &(*ast_root)->u.whilee.test);

	CSGTestBool(&x);
	CSGFixLink(x->false);
	if (sym != CSSrparen) CSSError("')' expected");
	sym = CSSGet();
	if (sym != CSSlbrace) CSSError("'{' expected");
	sym = CSSGet();
	
	(*ast_root)->u.whilee.body->kind = A_arrayExp;
	// STATEMENT SEQUENCCE
	StatementSequence(&(*ast_root)->u.whilee.body);

	if (sym != CSSrbrace) CSSError("'}' expected");
	sym = CSSGet();
	CSGBJump(label);
	CSGFixLink(x->true);
}

static void Statement(A_exp *ast_root)
{
	register CSGNode obj;
	CSGNode x;

	switch (sym) {

	case CSSif: 
		// IF
		IfStatement(ast_root);
		break;
	case CSSwhile: 
		//WHILE
		WhileStatement(ast_root); 
		break;

	case CSSsemicolon: 
		break;  /* empty statement */

	case CSSident:
		obj = FindObj(&globscope, &CSSid);
		if (obj == NULL) CSSError("unknown identifier");
		sym = CSSGet();
		x = malloc(sizeof(CSGNodeDesc));
		assert(x != NULL);
		if (sym == CSSlparen) {
			// PROC_CALL
			(*ast_root)->kind = A_callExp;
			(*ast_root)->u.call.func = malloc(sizeof(S_symbol));
			(*ast_root)->u.call.func->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
			strcpy((*ast_root)->u.call.func->name, CSSid);
			(*ast_root)->u.call.args = malloc(sizeof(A_expList));
			ProcedureCallM(obj, &x, &(*ast_root)->u.call.args);
		}
		else {
			CSGMakeNodeDesc(&x, obj);
			// ASSIGN
			(*ast_root)->kind = A_assignExp;

			A_exp tmp = malloc(sizeof(A_exp));
			tmp->kind = A_varExp;
			tmp->next = NULL;
			tmp->desig_next = NULL;

			tmp->u.var = malloc(sizeof(A_var));
			tmp->u.var->name = malloc(sizeof(S_symbol));
			tmp->u.var->name->name = (char*) malloc(sizeof(char) * CHAR_SIZE);
			strcpy(tmp->u.var->name->name, CSSid);
			(*ast_root)->u.assign.var = tmp;
			
			(*ast_root)->u.assign.exp = malloc(sizeof(A_exp));

			AssignmentM(&x, ast_root);
		}
		break;
	default: CSSError("unknown statement");
	}
}

static void StatementSequence(A_exp *ast_root)
{
	(*ast_root)->kind = A_arrayExp;
	(*ast_root)->u.arr = malloc(sizeof(A_expList));
	(*ast_root)->u.arr->start = NULL;
	(*ast_root)->u.arr->end = NULL;
	while (sym != CSSrbrace) {
		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;
		if ((*ast_root)->u.arr->start == NULL)
			(*ast_root)->u.arr->start = tmp;
		else
			(*ast_root)->u.arr->end->next = tmp;
		(*ast_root)->u.arr->end = tmp;
		// STATEMENT
		Statement(&(*ast_root)->u.arr->end);
	}
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

static void FormalParameters(CSGNode *root)
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

static void ProcedureHeading(CSGNode *proc, S_symbol *func)
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

	strcpy((*func)->name, name);
}

static void ProcedureBody(CSGNode *proc, A_expList *ast_root_list)
{
	(*ast_root_list)->start = NULL;
	(*ast_root_list)->end = NULL;

	register int returnsize;
	register CSGNode curr;

	tos = 0;
	while ((sym == CSSconst) || (sym == CSSstruct) || ((sym == CSSident) && (strcmp(CSSid, "long") == 0))) {

		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;
		if ((*ast_root_list)->start == NULL)
			(*ast_root_list)->start = tmp;
		else
			(*ast_root_list)->end->next = tmp;
		(*ast_root_list)->end = tmp;

		if (sym == CSSconst) {
			
			// CONST DECLAR
			ConstantDeclaration(proc, &(*ast_root_list)->end);
		}
		else {

			// VAR DECLAR
			VariableDeclaration(proc, &(*ast_root_list)->end);
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
	
	A_exp tmp = malloc(sizeof(A_exp));
	tmp->kind = A_arrayExp;
	tmp->next = NULL;
	tmp->desig_next = NULL;
	
	if ((*ast_root_list)->start == NULL)
		(*ast_root_list)->start = tmp;
	else
		(*ast_root_list)->end->next = tmp;
	(*ast_root_list)->end = tmp;

	// STATEMENT SEQ
	StatementSequence(&(*ast_root_list)->end);
	
	if (strcmp((*proc)->name, "main") == 0) {
		CSGClose();
	}
	else {
		CSGReturn(returnsize);
	}
	CSGAdjustLevel(-1);
}

static void ProcedureDeclaration(A_exp *ast_root)
{
	(*ast_root)->kind = A_callExp;
	
	CSGNode proc;
	assert(sym == CSSvoid);
	sym = CSSGet();

	(*ast_root)->u.call.func = malloc(sizeof(S_symbol));
	(*ast_root)->u.call.func->name = (char*) malloc(sizeof(char) * CHAR_SIZE);

	// PROC HEADING
	ProcedureHeading(&proc, &(*ast_root)->u.call.func);

	if (sym != CSSlbrace) CSSError("'{' expected");
	sym = CSSGet();

	(*ast_root)->u.call.args = malloc(sizeof(A_expList));
	
	// PROC BODY
	ProcedureBody(&proc, &(*ast_root)->u.call.args);
	
	if (sym != CSSrbrace) CSSError("'}' expected");
	sym = CSSGet();
	proc->next = NULL;  // cut off rest of list
}

static void Program(A_exp *ast_root)
{
	
	CSGOpen();
	tos = 32768;
	instruct = 0;

	(*ast_root)->kind = A_arrayExp;

	(*ast_root)->u.arr = malloc(sizeof(A_expList));
	(*ast_root)->u.arr->start = NULL;
	(*ast_root)->u.arr->end = NULL;
	
	while ((sym != CSSvoid) && (sym != CSSeof)) {
		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;
		if ((*ast_root)->u.arr->start == NULL)
			(*ast_root)->u.arr->start = tmp;
		else
			(*ast_root)->u.arr->end->next = tmp;
		(*ast_root)->u.arr->end = tmp;

		if (sym == CSSconst) {
			// CONST DECLAR
			ConstantDeclaration(&globscope, &(*ast_root)->u.arr->end);
		}
		else {
			// VAR DECLAR
			VariableDeclaration(&globscope, &(*ast_root)->u.arr->end);
		}
		
	}

	CSGStart(32768 - tos);
	if (sym != CSSvoid) CSSError("procedure expected");
	while (sym == CSSvoid) {
		A_exp tmp = malloc(sizeof(A_exp));
		tmp->next = NULL;
		tmp->desig_next = NULL;
		if ((*ast_root)->u.arr->start == NULL)
		{
			(*ast_root)->u.arr->start = tmp;
			(*ast_root)->u.arr->end = tmp;
		}
		else
		{
			(*ast_root)->u.arr->end->next = tmp;
			(*ast_root)->u.arr->end = tmp;
		}
		// PROC DECLAR
		ProcedureDeclaration(&(*ast_root)->u.arr->end);
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
	}
	else {
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

static void Compile(char *filename, A_exp *ast_root)
{
	printf("compiling %s\n", filename);

	globscope = NULL;
	InsertObj(&globscope, CSGTyp, CSGlongType, "long", 8);
	InsertObj(&globscope, CSGSProc, NULL, "ReadLong", 1);
	InsertObj(&globscope, CSGSProc, NULL, "WriteLong", 2);
	InsertObj(&globscope, CSGSProc, NULL, "WriteLine", 3);

	CSSInit(filename);
	sym = CSSGet();
	Program(ast_root);
}


/*************************************************************************/

void printGeneralAST(A_exp ast_root, int level);

void printDesignator(A_exp ast_root, int level)
{
	if (ast_root == NULL)
		return;

	// Variable
	if (ast_root->kind == A_varExp)
	{
		int i = 0;
		for (i = 0; i < level; i++)
			printf("\t");
		printf("%s\n", ast_root->u.var->name->name);
		printDesignator(ast_root->next, level+1);
	}
	// Expression
	else
	{
		A_exp pnt = ast_root;
		int i = 0;
		while (pnt != NULL)
		{
			printGeneralAST(pnt, level + i);
			i++;
			pnt = pnt->next;
		}
	}
}

void printGeneralAST(A_exp ast_root, int level)
{
	if (ast_root == NULL)
		return;

	int i = 0;
	A_exp pnt;
	switch (ast_root->kind)
	{
	// Variable
	case A_varExp:
		
		for (i = 0; i < level; i++)
			printf("\t");
		printf("Var: %s\n", ast_root->u.var->name->name);

		if (ast_root->next != NULL)
			printDesignator(ast_root->next, level+1);
		break;

	// Int
	case A_intExp:
		for (i = 0; i < level; i++)
			printf("\t");
		printf("Int: %d\n", ast_root->u.intt);
		break;

	// Calling
	case A_callExp:
		for (i = 0; i < level; i++)
			printf("\t");
		printf("Call: %s\n", ast_root->u.call.func->name);

		pnt = ast_root->u.call.args->start;
		while (pnt != NULL)
		{
			printGeneralAST(pnt, level+1);
			pnt = pnt->next;
		}
		break;

	// OPERATION 
	case A_opExp:
		// Right
		printGeneralAST(ast_root->u.op.right, level+1);

		// Oper
		for (i = 0; i < level; i++)
			printf("\t");
		printf("%s\n", ast_root->u.op.oper->op->name);

		// Left
		printGeneralAST(ast_root->u.op.right, level+1);
		break;

	// ASSIGN
	case A_assignExp:
		// EXP
		printGeneralAST(ast_root->u.assign.exp, level + 1);

		// Oper
		for (i = 0; i < level; i++)
			printf("\t");
		printf("=\n");

		// VAR
		printGeneralAST(ast_root->u.assign.var, level + 1);
		break;

	case A_ifExp:
		// IF
		for (i = 0; i < level; i++)
			printf("\t");
		printf("IF:\n");
		printGeneralAST(ast_root->u.iff.test, level + 1);

		// THEN
		for (i = 0; i < level; i++)
			printf("\t");
		printf("THEN:\n");
		printGeneralAST(ast_root->u.iff.then, level + 1);

		// ELSE
		for (i = 0; i < level; i++)
			printf("\t");
		printf("IF:\n");
		printGeneralAST(ast_root->u.iff.elsee, level + 1);
		break;

	case A_whileExp:
		// TEST
		for (i = 0; i < level; i++)
			printf("\t");
		printf("WHILE:\n");
		printGeneralAST(ast_root->u.whilee.test, level + 1);

		// BODY
		for (i = 0; i < level; i++)
			printf("\t");
		printf("BODY:\n");
		printGeneralAST(ast_root->u.whilee.body, level + 1);
		break;

	case A_arrayExp:
		pnt = ast_root->u.arr->start;
		while (pnt != NULL)
		{
			printGeneralAST(pnt, level + 1);
			pnt = pnt->next;
		}
		break;

	default:
		break;
	}
}


int main(int argc, char *argv [])
{
	A_exp ast_root = malloc(sizeof(A_exp));
	ast_root->next = NULL;
	ast_root->desig_next = NULL;

	CSGInit();

	Compile("samples/qsort.cs", &ast_root);
	printf("Helloword1\n");


	printGeneralAST(ast_root, 0);
	getchar();
	return 0;
}
