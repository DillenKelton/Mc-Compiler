#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

typedef struct stackVar
{
	char* id;
	int offset;
} stackVar;

typedef struct stackInfo
{
	char* funcId;
	stackVar* vars;
	int numOfVars;
} stackInfo;

typedef struct term
{
	int type;
	char* id;
	int val;
} term;

typedef struct expression
{
	int operation;
	term t1, t2;
} expression;

enum termType
{
	VARIABLE, INTEGERCONSTANT
};

void genCodeRec(tree*, FILE*, stackInfo*);

int getStackOffset(char*, stackInfo*);

expression evalExpr(tree*);
term getTerm(tree*);

void genData(tree* ast, FILE* fp);

void loadTerms(expression, FILE*, stackInfo*);

void genOutputFunction(FILE* fp);

int condLabelCounter = 0;

void gencode(tree *ast){

	FILE *fp = fopen("gencode.asm", "w");
	
	fprintf(fp, "\t.data\n");
	
	genData(ast, fp);

	fprintf(fp, "\n\t.text");
	fprintf(fp, "\n\t.globl main");

	fprintf(fp, "\n\tj main"); // jump to entry-point main

	genOutputFunction(fp); // generate library function for output

	genCodeRec(ast, fp, NULL);	

	fclose(fp);

	return;
}

void genCodeRec(tree* t, FILE* fp, stackInfo* si)
{
	if(!t)
	{
	    return;
	}
	switch(t->nodeKind)
	{
	    case FUNDECL:
	    {
		// TODO: need to remember correct stack frame structure (need more than local variables, also alignment concerns)

		char* funcName = strTable[t->children[0]->children[1]->val].id;
		fprintf(fp, "\n%s:", funcName); // create function label
		// initialize function stack
		stackInfo* newStackInfo = malloc(sizeof(stackInfo));
		newStackInfo->vars = malloc(sizeof(stackVar) * MAXIDS);
		int numOfLocalVars = 0;
		int space = 0;
		for(int i = 0; i < MAXIDS; i++)
		{
		    if(strTable[i].id != NULL && strcmp(strTable[i].scope, funcName) == 0) // variable belongs to this function
		    {
			newStackInfo->vars[numOfLocalVars].id = strTable[i].id;
			newStackInfo->vars[numOfLocalVars].offset = 4 * numOfLocalVars;
			numOfLocalVars++;
		    }
		}

		fprintf(fp, "\naddi $fp, $sp, 0");
		fprintf(fp, "\nsw $ra, 0($fp)");
		space = numOfLocalVars;
		space ++; // for $ra	

		newStackInfo->numOfVars = numOfLocalVars;
		newStackInfo->funcId = funcName;
		fprintf(fp, "\naddi $sp, $sp, -%i", 4 * space); // allocate enough space on stack for local variables

		// if there is an argument in the function, put the value in $a0 into the correct place in stack memory
		if(strTable[t->children[0]->children[1]->val].numParams > 0)
		{
		    int paramOffset = 0; // this should always be at this position on the stack
		    fprintf(fp, "\nsw $a0, %i($sp)", paramOffset);
		}

		// handle code within funBody
		for(int i = 1; i < t->numChildren; i++)
		{
		    genCodeRec(t->children[i], fp, newStackInfo);
		}

		fprintf(fp, "\naddi $sp, $sp, %i", 4 * space); // free up space on stack

		// return code so we don't fall into other functions (in case user didn't manually return)
		if(strcmp(funcName, "main") == 0) 
		{
		    // returning from main, so we need to exit program
		    fprintf(fp, "\nli $v0, 10\nsyscall");
		}
		else
		{
		    fprintf(fp, "\nlw $ra, 0($sp)");		    
		    // non-main function, just return
		    fprintf(fp, "\njr $ra");
		}

		free(newStackInfo->vars);
		free(newStackInfo);
		return;
	    }
	    case ASSIGNSTMT:
	    {
		char* lhsVarId = strTable[t->children[0]->val].id; // this is the variable we should store the value in
		if(!si || !lhsVarId)
		{
		    // break if no stack info or lhsVar doesn't exist in symbol table
		    break;
		}
		int lhsOffset = getStackOffset(lhsVarId, si);

		expression exprInfo = evalExpr(t->children[1]);
		if(exprInfo.operation == -1) // no operation, just simple assignment with one term (exprInfo.t1)
		{
		    if(exprInfo.t1.type == VARIABLE)
		    {	
			// load variable value from stack into free register
			int rhsOffset = getStackOffset(exprInfo.t1.id, si);
			if (rhsOffset == -1)
			{
				fprintf(fp, "\nlw $t0, %s", exprInfo.t1.id); //using global label
			}
			else
			{
				fprintf(fp, "\nlw $t0, %i($sp)", rhsOffset);
				// store value in lhsVar's stack location
			}
			if (lhsOffset == -1)
			{
				fprintf(fp, "\nsw $t0, %s", lhsVarId); // global
			}
			else
			{
				fprintf(fp, "\nsw $t0, %i($sp)", lhsOffset);
			}
		    }
		    else if(exprInfo.t1.type == INTEGERCONSTANT)
		    {
			fprintf(fp, "\nli $t0, %i", exprInfo.t1.val); // load the integer value into a register
			if (lhsOffset == -1)
			{
				fprintf(fp, "\nsw $t0, %s", lhsVarId); //global
			}
			else
			{
				fprintf(fp, "\nsw $t0, %i($sp)", lhsOffset); // store integer value into correct place in stack
			}
		    }
		}
		else // this section is for assignments with simple two term expressions (x = y + z or x = 5 + 2 or x = y + 7, etc)
		{
		    loadTerms(exprInfo, fp, si);
		    // at this point $t0 contains integer from term 1 and $t1 contains integer from term 2

		    switch(exprInfo.operation)
		    {
			case ADD:
			    fprintf(fp, "\nadd $t2, $t0, $t1"); // add values from term 1 and 2 and store the result in $t2
			    break;
			case SUB:
			    fprintf(fp, "\nsub $t2, $t0, $t1"); // subtract values from term 1 and 2 and stored the result in $t2
			    break;
			case MUL:
			    fprintf(fp, "\nmul $t2, $t0, $t1"); // multiply values from term 1 and 2 and store the result in $t2
			    break;
			case DIV:
			    fprintf(fp, "\ndiv $t0, $t1\nmflo $t2"); // divide values from term 1 and 2 and stored the result in $t2
			    break;
		    }

		    fprintf(fp, "\nsw $t2, %i($sp)", lhsOffset); // store value from expression into lhs variable
		} 
		break;
	    }
	    case FUNCCALLEXPR:
	    {
		char* funcCallName = strTable[t->children[0]->val].id;
		
		if(t->numChildren > 1)
		{
		    // has args to pass
		    expression callExpr = evalExpr(t->children[1]);
		    // assuming there to be at most one integer argument (may be variable or integer constant)
		    if(callExpr.t1.type == VARIABLE)
		    {
			int term1Offset = getStackOffset(callExpr.t1.id, si);
			fprintf(fp, "\nlw $a0, %i($sp)", term1Offset);	
		    }
		    else if(callExpr.t1.type == INTEGERCONSTANT)
		    {
			fprintf(fp, "\nli $a0, %i", callExpr.t1.val);
		    }
		}

		// TODO: save appropriate registers prior to jump according to mips standards, will also need to restore them upon return
		fprintf(fp, "\njal %s", funcCallName);
		break;
            }
	    case CONDSTMT:
	    {
		// only need to cover <, >, ==
		// first child is the expression to evaluate, second child is the statement list to execute if expression is true
		// will have a conditional branch instruction at the top, to skip code within statement list if expression is false
		// at the bottom of statement list code will have a label that we skip to
		expression condExpr = evalExpr(t->children[0]); // get expression info
		loadTerms(condExpr, fp, si);

		// at this point $t0 contains integer from term 1 and $t1 contains integer from term 2
		
		// branch on expression evaluating to false (otherwise continue to execute statement list code)
		int condLabelNum = condLabelCounter; // save this for later reference
		switch(condExpr.operation)
		{
		    case LT:
			fprintf(fp, "\nbge $t0, $t1, cond%i", condLabelCounter++);
			break;
		    case GT:
			fprintf(fp, "\nble $t0, $t1, cond%i", condLabelCounter++);
			break;
		    case EQ:
			fprintf(fp, "\nbne $t0, $t1, cond%i", condLabelCounter++);
			break;
		}

		// generate code within conditional statement's brackets
		for(int i = 0; i < t->children[1]->numChildren; i++)
		{
		    genCodeRec(t->children[1]->children[i], fp, si);
		}

		// create label we can skip to on expression being false
		fprintf(fp, "\ncond%i:", condLabelNum);

		return;
	    }
	    case LOOPSTMT:
	    {
		expression loopExpr = evalExpr(t->children[0]);

		int loopStartLabelNum = condLabelCounter++; // saved for later reference
		int loopEndLabelNum = condLabelCounter++; // saved for later reference
		fprintf(fp, "\nloopStart%i:", loopStartLabelNum);

		loadTerms(loopExpr, fp, si);
		// at this point $t0 contains integer from term 1 and $t1 contains integer from term 2

		switch(loopExpr.operation)
		{
		    case LT:
			fprintf(fp, "\nbge $t0, $t1, loopEnd%i", loopEndLabelNum);
			break;
		    case GT:
			fprintf(fp, "\nble $t0, $t1, loopEnd%i", loopEndLabelNum);
			break;
		    case EQ:
			fprintf(fp, "\nbne $t0, $t1, loopEnd%i", loopEndLabelNum);
			break;
		}
			
		// generate code within loop statement's brackets
		for(int i = 0; i < t->children[1]->numChildren; i++)
		{
		    genCodeRec(t->children[1]->children[i], fp, si);
		}

		// jump back to start of loop to check conditional
		fprintf(fp, "\nj loopStart%i", loopStartLabelNum);

		// create label we can skip to on expression being false
		fprintf(fp, "\nloopEnd%i:", loopEndLabelNum);

		return;
	    }
	    case RETURNSTMT:
	    {
		if(t->numChildren > 0)
		{
		    // must returna a value from expression
		    expression returnExp = evalExpr(t->children[0]);
		    // assuming for now that the expression just contains one number or variable to save time
		    if(returnExp.t1.type == VARIABLE)
		    {
			int term1Offset = getStackOffset(returnExp.t1.id, si);
			fprintf(fp, "\nlw $v0, %i($sp)", term1Offset);
		    }
		    else if(returnExp.t1.type == INTEGERCONSTANT)
		    {
			fprintf(fp, "\nli $v0, %i", returnExp.t1.val);
		    }
		}
		// deconstruct the stack
		fprintf(fp, "\naddi $sp, $sp, %i", (4 * si->numOfVars)+ 4); // free up space on stack

		if(strcmp(si->funcId, "main") == 0)
		{
		    // returning from main, so we need to exit program
		    fprintf(fp, "\nli $v0, 10\nsyscall");
		}
		else
		{
		    fprintf(fp, "\njr $ra");
		}
		break;
	    }
	}

	for(int i = 0; i < t->numChildren; i++)
	{
	    genCodeRec(t->children[i], fp, si);
	}
}

void loadTerms(expression exp, FILE* fp, stackInfo* si)
{
	// helper function designed to generate code to load two terms into $t0 and $t1
	
	if(exp.t1.type == VARIABLE)
	{
	    int t1offset = getStackOffset(exp.t1.id, si);
	    if(t1offset == -1)
	    {
	    	fprintf(fp, "\nlw $t0, %s", exp.t1.id); // load integer stored in t1 var into register
	    }
	    else
	    {
	    	fprintf(fp, "\nlw $t0, %i($sp)", t1offset); // load integer stored in t1 var into register
	    }
	}
	else
	{
	    fprintf(fp, "\nli $t0, %i", exp.t1.val); // load imm into register
	}
		    
	if(exp.t2.type == VARIABLE)
	{
	    int t2offset = getStackOffset(exp.t2.id, si);
	    if (t2offset == -1)
	    {
	    	fprintf(fp, "\nlw $t1, %s", exp.t2.id); // load integer stored in t2 var into register
	    }
	    else
	    {
	    	fprintf(fp, "\nlw $t1, %i($sp)", t2offset); // load integer stored in t2 var into register
	    }
	}    
	else
	{
	    fprintf(fp, "\nli $t1, %i", exp.t2.val); // load imm into register
	}
}

int getStackOffset(char* id, stackInfo* si)
{
	int offset = -1; // default of -1 which will refer to global var

	for(int i = 0; i < si->numOfVars; i++)
	{
	    // find var in stackInfo so we can get the offset
	    if(strcmp(id, si->vars[i].id) == 0)
	    {
		offset = si->vars[i].offset;
		break;
	    }
	}

	return offset;
}

expression evalExpr(tree* t)
{
	// evaluate simple expressions and generate an expression structure with info about the terms and operator
	expression expr;

	while(t && t->nodeKind != EXPRESSION)
	{
	    t = t->children[0];
	}
	
	if(t->children[0]->nodeKind != EXPRESSION)
	{
	    t = t->children[0]; // go down one more level
	}

	if(t->numChildren == 1)
	{
	    // single factor expression, likely simple assignment
	    expr.operation = -1; // no op
	    expr.t1 = getTerm(t);
	}
	else if(t->numChildren == 3)
	{
	    // double factor expression, should have an operator in the middle
	    expr.operation = t->children[1]->val;
	    expr.t1 = getTerm(t->children[0]);
	    expr.t2 = getTerm(t->children[2]);
	}

	return expr;
}

term getTerm(tree* t)
{
	term newTerm;

	while(t && t->nodeKind != TERM)
	{
	    t = t->children[0];
	}

	t = t->children[0]->children[0]; // get actual term two levels down
	if(t->nodeKind == IDENTIFIER)
	{
		newTerm.type = VARIABLE;
		newTerm.id = strTable[t->val].id;
	}
	else if(t->nodeKind == INTEGER)
	{
		newTerm.type = INTEGERCONSTANT;
		newTerm.val = t->val;
	}

	return newTerm;
}

void genData(tree *ast, FILE *fp){

	if(!ast)
	{
		return;
	}
	
	switch(ast->nodeKind)
	{
		case VARDECL:
		{
	    		if(strcmp(strTable[ast->children[1]->val].scope, "global") == 0)
			{	
				fprintf(fp,"%s:\t.space %i #generating labels for global int vars\n" ,strTable[ast->children[1]->val].id , 4);
			}
			break;
		}
		default:
		{	
			for(int i = 0; i < ast->numChildren; i++)
			{
		 	   	genData(ast->children[i], fp);
			}
			break;
		}
	}
}

void genOutputFunction(FILE* fp)
{
	fprintf(fp, "\noutput:");
	// allocate stack space
	// TODO: figure out exactly how much is needed here aside from local variable
	fprintf(fp, "\naddi $sp, $sp, -4");
	// output number (number we want printed is already in $a0)
	fprintf(fp, "\nli $v0, 1\nsyscall");
	// cleanup stack space
	fprintf(fp, "\naddi $sp, $sp, 4");
	// return
	fprintf(fp, "\njr $ra");
}

