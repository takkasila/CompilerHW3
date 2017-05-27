#include<stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#define CHAR_SIZE 16
#include "ast.h"
#include "ir.h"
// #include "cfg.h"
#include "csg.h"
typedef struct IR_ *IR;
typedef struct IR_{
    signed char class;
    char op;
    int line;
    long long val;
    char name[CHAR_SIZE];
    IR x,y;
    IR next,prev;
}IR_;

int count = 0;
int label_true = 0;
int endif = 0;
int endwhile = 0;
int label_test = 0;
int is_first = 0;
int max = 0;
int countIR = 0;
void printByType(A_exp temp){
    switch (temp->kind){
        case A_intExp:
            
            printf("%d", temp->u.intt);
            break;
        case A_varExp:
            printf("%s", temp->u.var);
            break;
    }
    
}
IR root;
void pushTOIR(IR *temp){
    if (!root) return;
    if (!root){
        root = *temp;
        
        (*temp)->next = NULL;
        (*temp)->prev = NULL;
        (*temp)->line = countIR++;
    }else{
        IR it = root;
        while(it->next) it = it->next;
        it->next = *temp;
        (*temp)->prev = it;
        (*temp)->next = NULL;
        (*temp)->line = countIR++;
        
    }
    
}
IR OP(A_exp current){
    // A_exp temp,temp2;
    IR tempir;
    IR temp,temp2;
    switch (current->kind){
    case A_intExp:        
            // return current; 
            tempir = malloc(sizeof(*tempir));
            tempir->class = CSGConst;
            tempir->val = current->u.intt;
            return tempir;
            break;
    case A_varExp:       
            // return current;
            tempir = malloc(sizeof(*tempir));
            tempir->class = CSGVar;
            strcpy(tempir->name ,current->u.var);
            break;
    case A_op1Exp:
            temp = OP(current->u.op.left);
        //    printf("t%d = ",count); 
            tempir = malloc(sizeof(*tempir));
            tempir->x = temp;
             
            
            switch(current->u.op.oper){
            case CSStimes:
                        // printf("*");
                        // tempir->op = imul;
                        break;
                    case CSSdiv:
                        // printf("/");
                        // tempir->op = idiv;
                        break;
                    case CSSmod:
                        // printf("%%");
                        
                        break;
                    case CSSplus:
                        // printf("+");
                        break;

                    case CSSminus:
                        // printf("-");
                        tempir->op = ineg;
                        break;
                    case CSSeql:
                        // printf("=");
                        break;
                    case CSSneq:
                        // printf("!=");
                        break;
                    case CSSlss:
                        // printf("<");
                        break;
                    case CSSleq:
                        // printf("<=");
                        break;
                    case CSSgtr:
                        // printf(">");
                        break;
                    case CSSgeq:
                        // printf(">=");
                        break;
                    case CSScomma:
                    // printf(" , ");
                    break;    
        }
        // printByType(temp);
        // temp = malloc(sizeof(*temp));
        // temp->u.var = malloc(sizeof(char)*CHAR_SIZE);
        // sprintf(temp->u.var,"t%d",count++);
        // temp->kind = A_varExp;
        // printf("\n");
        pushTOIR(&tempir);
        return tempir;
    case A_opExp:
        
        temp = OP(current->u.op.left);
        temp2 = OP(current->u.op.right);
        tempir->x = temp;
        tempir->y = temp2;
        // printf("t%d = ",count);
        // printByType(temp);
        switch(current->u.op.oper){
            case CSStimes:
                        // printf(" * ");
                        tempir->op = imul;
                        break;
                    case CSSdiv:
                        // printf(" / ");
                        tempir->op = idiv;
                        break;
                    case CSSmod:
                        // printf(" %% ");
                        tempir->op = imod;
                        break;
                    case CSSplus:
                        // printf(" + ");
                        tempir->op = iadd;
                        break;

                    case CSSminus:
                        // printf(" - ");
                        tempir->op = isub;
                        break;
                    case CSSeql:
                        // printf(" = ");
                        tempir->op = icmpeq;
                        break;
                    case CSSneq:
                        printf(" != ");
                        break;
                    case CSSlss:
                        printf(" < ");
                        break;
                    case CSSleq:
                        printf(" <= ");
                        break;
                    case CSSgtr:
                        printf(" > ");
                        break;
                    case CSSgeq:
                        printf(" >= ");
                        break;
                    case CSScomma:
                    printf(" , ");
                    break;
        }
        // printByType(temp2);
        // temp = malloc(sizeof(*temp));
        // temp->u.var = malloc(sizeof(char)*CHAR_SIZE);
        // sprintf(temp->u.var,"t%d",count++);
        // temp->kind = A_varExp;
        // printf("\n");
        return temp;
        
    }
    // printf("\n");
    // return count;
}

A_exp gen_ir(A_exp root){
    int local_label_true = label_true;
    int local_endif = endif;
    int local_endwhile = endwhile;
    int local_label_test = label_test;
    A_exp temp;
    if(!root) return NULL;
    switch(root->kind){
        case A_callExp:
            printf(">>%s ",root->u.call.func);
            if (!is_first){
                printf("\n");
                is_first = 1;
            }
            for(A_expList it = root->u.call.args; it; it = it->next){
                
                gen_ir(it->exp);
            }
            printf("\n");
            break;
        case A_intExp:
            printf("%d", root->u.intt);
            return root;
            break;
        case A_varExp:
            printf("%s", root->u.var);
            return root;
            break;
        case A_opExp:
            
        case A_op1Exp: // we don't really need this one.
            
            return OP(root);
            break;
        case A_assignExp:
            if (root->u.assign.exp->kind == A_op1Exp || root->u.assign.exp->kind == A_opExp)
            {
                temp = gen_ir(root->u.assign.exp);
                gen_ir(root->u.assign.var);
                
                printf(" = ");
                printByType(temp);
            }
            else{
                // temp = gen_ir(root->u.assign.exp);
                gen_ir(root->u.assign.var);
                printf(" = ");
                gen_ir(root->u.assign.exp);
            }
            
            
            
            printf("\n");
            break;
        case A_ifExp:
            // printf("If\n");
            label_true+=1;
            endif+=1;
            temp = gen_ir(root->u.iff.test);
            printf("cjump ");
            printByType(temp);
            printf(" true%d\n",local_label_true);
            
            if(root->u.iff.elsee) {
                for(A_expList it = root->u.iff.elsee; it; it = it->next){
                    gen_ir(it->exp);
                }
            }
            printf("jump endI%d\n",local_endif);
             
            printf("label true%d\n",local_label_true++);
            
            for(A_expList it = root->u.iff.then; it; it = it->next){
                gen_ir(it->exp);
            }
            printf("label endI%d\n",local_endif++);
            
            
            break;
        case A_whileExp:
            // printf("while\n");
            label_test+=1;
            endwhile+=1;
            printf("label test%d\n",local_label_test);
            
            temp =  gen_ir(root->u.whilee.test);
        
            printf("t%d = not ",count);
            printByType(temp);
            printf("\ncjump t%d endW%d\n",count++,local_endwhile);
            

            for(A_expList it = root->u.whilee.body; it; it = it->next){
                gen_ir(it->exp);
            }
            printf("jump test%d\n",local_label_test++);
            printf("label endW%d\n",local_endwhile++);
            


            break;
        // case A_arrayExp:
        //     printf("array\n");
        //     break;
        // case A_structExp:
        //     if(root->u.structt.exp){
        //         printf(".\n");
        //     } else {
                
        //         /* fixed bug it's shouldn't enter here */
        //         assert(1 == 2);
        //         printf("struct_%s\n", root->u.structt.var->u.var);
        //     }
        //     break;
        default:
            printf("what?\n");
    }
    // printf("\n");
    
}

void traverse(A_expList expl){
    
    for (A_expList it = expl ; it != NULL; it = it->next)
    {
        is_first= 0;
        gen_ir(it->exp);
                
    }
    // printf("Eiei");
}

