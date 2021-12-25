%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<../src/tree.h>
#include<../src/strtab.h>

extern int yylineno;
/* nodeTypes refer to different types of internal and external nodes that can be part of the abstract syntax tree.*/

/* NOTE: mC has two kinds of scopes for variables : local and global. Variables declared outside any
function are considered globals, whereas variables (and parameters) declared inside a function foo are local to foo. You should update the scope variable whenever you are inside a production that matches function definition (funDecl production). The rationale is that you are entering that function, so all variables, arrays, and other functions should be within this scope. You should pass this variable whenever you are calling the ST_insert or ST_lookup functions. This variable should be updated to scope = "" to indicate global scope whenever funDecl finishes. Treat these hints as helpful directions only. You may implement all of the functions as you like and not adhere to my instructions. As long as the directory structure is correct and the file names are correct, we are okay with it. */

char scope[50] = "global"; 
int indx, funIndx;
%}

/* the union describes the fields available in the yylval variable */
%union
{
    int value;
    struct treenode *node;
    char *strval;
}

/*Add token declarations below. The type <value> indicates that the associated token will be of a value type such as integer, float etc., and <strval> indicates that the associated token will be of string type.*/
%token <strval> ID
%token <value> INTCONST
%token <value> CHARCONST
%token <strval> STRCONST
%token <value> KWD_IF
%token <value> KWD_ELSE
%token <value> KWD_WHILE
%token <value> KWD_INT
%token <value> KWD_STRING
%token <value> KWD_CHAR
%token <value> KWD_RETURN
%token <value> KWD_VOID
%token <value> OPER_ADD
%token <value> OPER_SUB
%token <value> OPER_MUL
%token <value> OPER_DIV
%token <value> OPER_LT
%token <value> OPER_GT
%token <value> OPER_GTE
%token <value> OPER_LTE
%token <value> OPER_EQ
%token <value> OPER_NEQ
%token <value> OPER_ASGN
%token <value> OPER_AT
%token <value> OPER_INC
%token <value> OPER_DEC
%token <value> OPER_MOD
%token <value> OPER_AND
%token <value> OPER_OR
%token <value> OPER_NOT
%token <value> LSQ_BRKT
%token <value> RSQ_BRKT
%token <value> LCRLY_BRKT
%token <value> RCRLY_BRKT
%token <value> LPAREN
%token <value> RPAREN
%token <value> COMMA
%token <value> SEMICLN
%token <value> ERROR
%token <value> ILLEGAL_TOK

/* TODO: Declate non-terminal symbols as of type node. Provided below is one example. node is defined as 'struct treenode *node' in the above union data structure. This declaration indicates to parser that these non-terminal variables will be implemented using a 'treenode *' type data structure. Hence, the circles you draw when drawing a parse tree, the following lines are telling yacc that these will eventually become circles in an AST. This is one of the connections between the AST you draw by hand and how yacc implements code to concretize that. We provide with two examples: program and declList from the grammar. Make sure to add the rest.  */

%type <node> program declList decl varDecl typeSpecifier funDecl funcTypeName formalDeclList formalDecl funBody localDeclList statementList statement compoundStmt assignStmt condStmt loopStmt returnStmt var expression relop addExpr addop term mulop factor funcCallExpr argList

%start program

%%
/* TODO: Your grammar and semantic actions go here. We provide with two example productions and their associated code for adding non-terminals to the AST.*/

program         :{ libraryFuncs(); } declList
                 {
                    tree* progNode = maketree(PROGRAM);
                    addChild(progNode, $2);
                    ast = progNode;
                 }
                ;

declList        : decl
                 {
                    tree* declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    $$ = declListNode;
                 }
                | declList decl
                 {
                    tree* declListNode = maketree(DECLLIST);
                    addChild(declListNode, $1);
                    addChild(declListNode, $2);
                    $$ = declListNode;
                 }
                ;

decl		: varDecl
		 {
                    tree* declNode = maketree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
		 }
		| funDecl
		 {
                    tree* declNode = maketree(DECL);
                    addChild(declNode, $1);
                    $$ = declNode;
		 }
		;


varDecl    	: typeSpecifier ID LSQ_BRKT INTCONST RSQ_BRKT SEMICLN
		 {
                 indx = ST_lookup($2, scope);
                 if(indx != -1)
                 {
                     // variable has already been declared in this scope
                     yyerror("multiple variable declarations");
                 }
                 if($4 <= 0)
                 {
                     // invalid array size
                     yyerror("Array variable declared with size of zero or less");
                 }
		 indx =  ST_insert( $2, scope, $1->val, ARRAY, $4);
		 tree *varDeclNode = maketree(VARDECL);
                 addChild(varDeclNode, $1);
                 addChild(varDeclNode, maketreeWithVal(IDENTIFIER, indx, NULL));
                 addChild(varDeclNode, maketreeWithVal(INTEGER, $4, NULL));
		 addChild(varDeclNode, maketree(ARRAYDECL));
		 $$ = varDeclNode;
		 }
		| typeSpecifier ID SEMICLN
                 {
                 indx = ST_lookup($2, scope);
                 if(indx != -1)
                 {
                     // variable has already been declared in this scope
                     yyerror("multiple variable declarations");
                 }
		 indx =  ST_insert($2, scope, $1->val, SCALAR, NULL);
		 tree *varDeclNode = maketree(VARDECL);
                 addChild(varDeclNode, $1);
                 addChild(varDeclNode, maketreeWithVal(IDENTIFIER, indx, NULL));
		 $$ = varDeclNode;
	         }
        	;

typeSpecifier	 : KWD_INT
                  {
		   $$ = maketreeWithVal(TYPESPEC, INT_TYPE, NULL);
		  }
		 | KWD_CHAR
		  {
		   $$ = maketreeWithVal(TYPESPEC, CHAR_TYPE, NULL);
		  }
		 | KWD_VOID
		  {
		   $$ = maketreeWithVal(TYPESPEC, VOID_TYPE, NULL);
		  }
		 ;

funDecl		: funcTypeName LPAREN formalDeclList RPAREN funBody
		 {

                 fillParamList(funIndx, $3); // fill the parameter list recursively
		 tree *funDeclNode = maketree(FUNDECL);
		 addChild(funDeclNode, $1);
		 addChild(funDeclNode, $3);
		 addChild(funDeclNode, $5);
		 $$ = funDeclNode;
		 strcpy(scope, "global");
		 }
		| funcTypeName LPAREN RPAREN funBody
		 {

		 tree *funDeclNode = maketree(FUNDECL);
		 addChild(funDeclNode, $1);
		 addChild(funDeclNode, $4);
		 $$ = funDeclNode;
		 strcpy(scope, "global");
		 }
		;

funcTypeName   	:typeSpecifier ID 
		{

                 indx = ST_lookup($2, scope); 
                 if(indx != -1)
                 {
		     // function has already been defined
                     yyerror("multiple function definitions");
                 }

		 tree *funcTypeNameNode = maketree(FUNCTYPENAME);
		 funIndx =  ST_insert( $2, scope, $1->val, FUNCTION, NULL);
		 addChild(funcTypeNameNode, $1);
		 addChild(funcTypeNameNode, maketreeWithVal(IDENTIFIER, funIndx, NULL));

		 $$ = funcTypeNameNode;

		 strcpy(scope, $2);
		}
		;


formalDeclList	: formalDecl
		 {
		 tree *formalDeclListNode = maketree(FORMALDECLLIST);
		 addChild(formalDeclListNode, $1);
		 $$ = formalDeclListNode;
		 }
		| formalDecl COMMA formalDeclList
		 {
		 tree *formalDeclListNode = maketree(FORMALDECLLIST);
		 addChild(formalDeclListNode, $1);
		 addChild(formalDeclListNode, $3);
		 $$ = formalDeclListNode;
		 }
		;

formalDecl	: typeSpecifier ID
		 {
		 indx =  ST_insert($2, scope, $1->val, SCALAR, NULL);
		 tree *formalDeclNode = maketree(FORMALDECL);
		 addChild(formalDeclNode, $1);
		 addChild(formalDeclNode, maketreeWithVal(IDENTIFIER, indx, NULL));
		 $$ = formalDeclNode;
		 }
		| typeSpecifier ID LSQ_BRKT RSQ_BRKT
		 {
		 indx =  ST_insert($2, scope, $1->val, ARRAY, NULL);
		 tree *formalDeclNode = maketree(FORMALDECL);
		 addChild(formalDeclNode, $1);
		 addChild(formalDeclNode, maketreeWithVal(IDENTIFIER, indx, NULL));
		 addChild(formalDeclNode, maketree(ARRAYDECL));
		 $$ = formalDeclNode;
		 }
		;

funBody		: LCRLY_BRKT localDeclList statementList RCRLY_BRKT
		 {
        	 tree *funBodyNode = maketree(FUNBODY);
		 addChild(funBodyNode, $2);
		 addChild(funBodyNode, $3);
		 $$ = funBodyNode;
		 }
		;

localDeclList	:
		 {
		 $$ = maketree(LOCALDECLLIST);
		 }
		| varDecl localDeclList
		 {
         	 tree *localDeclListNode = maketree(LOCALDECLLIST);
		 addChild(localDeclListNode, $1);
		 addChild(localDeclListNode, $2);
		 $$ = localDeclListNode;
		 }
		;

statementList	:
		 {
		 $$ = maketree(STATEMENTLIST);
		 }
		| statement statementList
		 {
         	 tree *statementListNode = maketree(STATEMENTLIST);
         	 addChild(statementListNode, $1);
		 addChild(statementListNode, $2);
		 $$ = statementListNode;
		 }
		;

statement       : compoundStmt
		| assignStmt
		| condStmt
		| loopStmt
		| returnStmt
		;

compoundStmt 	: LCRLY_BRKT statementList RCRLY_BRKT
		 {
        	 tree *compoundStmtNode = maketree(COMPOUNDSTMT);
	         addChild(compoundStmtNode, $2);
		 $$ = compoundStmtNode;
		 }
		;

assignStmt	: var OPER_ASGN expression SEMICLN
                 {

		 tree *assignNode = maketree(ASSIGNSTMT);
		 addChild(assignNode, $1);
                 addChild(assignNode, $3);
		 $$ = assignNode;

		 tree *Var = assignNode->children[0];
 		 while (Var->numChildren > 0) {
        	 	Var = Var->children[0];
		 }

		tree *Expr = assignNode->children[1];
		while (Expr->numChildren > 0) {
			Expr = Expr->children[0];
		}
				
		int varDataType = strTable[Var->val].data_type;
		int expDataType;


		  if (Expr->nodeKind == IDENTIFIER) {
		      expDataType = strTable[Expr->val].data_type;
		  } else { //const 
		    expDataType = Expr->nodeKind;
		  }
		int flag = 0;	
		switch(varDataType){
			case INT_TYPE:
				if(!(expDataType == INT_TYPE || expDataType == INTEGER)){
					flag = 1;
				}
				break;
			case CHAR_TYPE:
				if(!(expDataType == CHAR_TYPE || expDataType == CHAR)){
					flag = 1;
				}
				break;
			case VOID_TYPE:
				if(expDataType != VOID_TYPE){
					flag = 1;
				}
				break;
						
		}
		if(flag){ 			 
		  printf("Error: %d: Type mismatch in assignment\n", yylineno);
		}
		}
	     	| expression SEMICLN
	         {
            	 tree *assignNode = maketree(ASSIGNSTMT);
            	 addChild(assignNode, $1);
            	 $$ = assignNode;
	         }
                ;

condStmt	: KWD_IF LPAREN expression RPAREN statement
		 {
	  	  tree *ifNode = maketree(CONDSTMT);
		  addChild(ifNode, $3);
                  addChild(ifNode, $5);
		  $$ = ifNode;
	      	 }
		| KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement
		 {
    		  tree *ifElseNode = maketree(CONDSTMT);
		  addChild(ifElseNode, $3);
		  addChild(ifElseNode, $5);
		  addChild(ifElseNode, $7);
		  $$ = ifElseNode;
		 }
		;

loopStmt	: KWD_WHILE LPAREN expression RPAREN statement
		 {
         	 tree *loopStmtNode = maketree(LOOPSTMT);
         	 addChild(loopStmtNode, $3);
		 addChild(loopStmtNode, $5);
		 $$ = loopStmtNode;
		 }
		;

returnStmt	: KWD_RETURN SEMICLN
		 {
         	 $$ = maketree(RETURNSTMT);
		 }
		| KWD_RETURN expression SEMICLN
 		 {
         	 tree *returnStmtNode = maketree(RETURNSTMT);
          	 addChild(returnStmtNode, $2);
		 $$ = returnStmtNode;
		 }
		;

var         	: ID
              {
		indx = ST_lookup($1, scope);
		if(indx == -1)
		{
		    indx = ST_lookup($1, "global");
		    	if(indx == -1){
		   	 // identifier not in symbol table
			    yyerror("undeclared variable");
			}
		}
		$$ = maketreeWithVal(IDENTIFIER, indx, NULL);
	      }
	     | ID LSQ_BRKT addExpr RSQ_BRKT
	      {
	        indx = ST_lookup($1, scope);
		if(indx == -1)
		{
		    indx = ST_lookup($1, "global");
		    	if(indx == -1){
		   	 // identifier not in symbol table
			    yyerror("undeclared variable");
			}
		}
                else if(strTable[indx].symbol_type != ARRAY)
                {
                    // variable is not an array but is being indexed like one
                    yyerror("Non-array identifier used as an array");
                }
                else if($3->numChildren == 1) // taking simplest case
                {
                    tree* child = $3->children[0]->children[0]->children[0];
                    if(child->nodeKind == INTEGER)
                    {
                        if(strTable[indx].arraySize != 0 && (strTable[indx].arraySize <= child->val || child->val < 0))
                        {
                            // statically sized array with out of bounds indexing
                            yyerror("Statically sized array indexed with constant, out-of-bounds expression");
                        }
                    }
                    else if(child->nodeKind == IDENTIFIER)
                    {
                        int idIndx = child->val;
                        if(!(strTable[idIndx].symbol_type == SCALAR && strTable[idIndx].data_type == INT_TYPE))
                        {
                            // variable used is not scalar or non-integer variable
                            yyerror("Array indexed using non-integer expression");    
                        }
                    }
                    else
                    {
                        // non-integer expression
                        yyerror("Array indexed using non-integer expression");
                    }
                }
          	tree *varNode = maketree(VAR);
          	addChild(varNode, maketreeWithVal(IDENTIFIER, indx, NULL));
          	addChild(varNode, $3);
          	$$ = varNode;
	      }
            ;

expression	: addExpr
                 {
                 tree *expressionNode = maketree(EXPRESSION);
                 addChild(expressionNode, $1);
                 $$ = expressionNode;
	         }
		| expression relop addExpr
		 {
                 tree *expressionNode = maketree(EXPRESSION);
                 addChild(expressionNode, $1);
                 addChild(expressionNode, $2);
                 addChild(expressionNode, $3);
                 $$ = expressionNode;
		 }
		;

relop		: OPER_LTE
             	 {
             	 $$ = maketreeWithVal(RELOP, LTE, NULL);
             	 }
		| OPER_LT
	         {
         	 $$ = maketreeWithVal(RELOP, LT, NULL);
             	 }
		| OPER_GT
                 {
     	         $$ = maketreeWithVal(RELOP, GT, NULL);
         	 }
		| OPER_GTE
	         {
                 $$ = maketreeWithVal(RELOP, GTE, NULL);
                 }
		| OPER_EQ
	         {
        	 $$ = maketreeWithVal(RELOP, EQ, NULL);
                 }
		| OPER_NEQ
                 {
     	         $$ = maketreeWithVal(RELOP, NEQ, NULL);
         	 }
		;

addExpr		: term
             	 {
              	 tree *addExprNode = maketree(ADDEXPR);
              	 addChild(addExprNode, $1);
              	 $$ = addExprNode;
             	 }	
		| addExpr addop term
             	 {
              	 tree *addExprNode = maketree(ADDEXPR);
              	 addChild(addExprNode, $1);
              	 addChild(addExprNode, $2);
              	 addChild(addExprNode, $3);
              	 $$ = addExprNode;
             	 }
		;

addop		: OPER_ADD
              	 {	
             	 $$ = maketreeWithVal(ADDOP, ADD, NULL);
             	 }
		| OPER_SUB
		 {
		 $$ = maketreeWithVal(ADDOP, SUB, NULL);
		 }
		;

term 		: factor
		 {
              	 tree *termNode = maketree(TERM);
              	 addChild(termNode, $1);
              	 $$ = termNode;
              	 }
		| term mulop factor
		 {
              	 tree *termNode = maketree(TERM);
             	 addChild(termNode, $1);
             	 addChild(termNode, $2);
              	 addChild(termNode, $3);
              	 $$ = termNode;
		 }
		;

mulop		: OPER_MUL
             	 {
             	 $$ = maketreeWithVal(MULOP, MUL, NULL);
             	 }
		| OPER_DIV
           	 {
            	  $$ = maketreeWithVal(MULOP, DIV, NULL);
            	 }
		;

factor		: LPAREN expression RPAREN
            	 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, $2);
             	 $$ = factorNode;
             	 }
		| var
            	 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, $1);
             	 $$ = factorNode;
             	 }
		| funcCallExpr
             	 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, $1);
             	 $$ = factorNode;
             	 }
	     	| INTCONST 
	     	 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, maketreeWithVal(INTEGER, $1, NULL));
             	 $$ = factorNode;
             	 }
		| CHARCONST
		 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, maketreeWithVal(CHAR, $1, NULL));
             	 $$ = factorNode;
             	 }
		| STRCONST
            	 {
             	 tree *factorNode = maketree(FACTOR);
             	 addChild(factorNode, maketreeWithVal(STRING, NULL, $1));
             	 $$ = factorNode;
            	 }
		;

funcCallExpr	: ID LPAREN argList RPAREN
            	{
		indx = ST_lookup($1, scope);
		
		if(indx == -1)
		{
		    indx = ST_lookup($1, "global");
		    	if(indx == -1){
		   	 // identifier not in symbol table
			    yyerror("undefined function called");
			}
		}
                param* argumentList = malloc(sizeof(param) * MAXPARAMS);
                int argSize = getArgList(argumentList, 0, $3);

                if(argSize > strTable[indx].numParams)
                {
                    // too many arguments
                    yyerror("Too many arguments provided in function call");
                }
                else if(argSize < strTable[indx].numParams)
                {
                    // too few arguments
                    yyerror("Too few arguments provided in function call");
                }
                else
                {
                    // same number of args, check if the types match
                    for(int i = 0; i < argSize; i++)
                    {
                        if(argumentList[i].data_type != strTable[indx].parameterList[i].data_type || argumentList[i].symbol_type != strTable[indx].parameterList[i].symbol_type)
                        {
                            // type mismatch
                            yyerror("Argument type mismatch in function call");
                            break;
                        }
                    }
                }

                free(argumentList);

             	tree *funcCallExprNode = maketree(FUNCCALLEXPR);
             	addChild(funcCallExprNode, maketreeWithVal(IDENTIFIER, indx, NULL));
             	addChild(funcCallExprNode, $3);
             	$$ = funcCallExprNode;
             	}
		| ID LPAREN RPAREN
            	{
		indx = ST_lookup($1, scope);
		if(indx == -1)
		{
		    indx = ST_lookup($1, "global");
		    	if(indx == -1){
		   	 // identifier not in symbol table
			    yyerror("undefined function called");
			}
		}
                if(indx != -1 && strTable[indx].numParams > 0)
                {
                    // not enough params supplied
                    yyerror("Too few arguments provided in function call");
                }
             	 tree* funcCallExprNode = maketree(FUNCCALLEXPR);
		 addChild(funcCallExprNode, maketreeWithVal(IDENTIFIER, indx, NULL));
             	 $$ = funcCallExprNode;
		}
		;

argList		: expression
            	{
            	 tree *argListNode = maketree(ARGLIST);
            	 addChild(argListNode, $1);
            	 $$ = argListNode;
            	 }
		| argList COMMA expression
                {
           	  tree *argListNode = maketree(ARGLIST);
           	  addChild(argListNode, $1);
           	  addChild(argListNode, $3);
           	  $$ = argListNode;
           	  }
		;


%%

int yywarning(char * msg){
    printf("warning: line %d: %s\n", yylineno, msg);
    return 0;
}

int yyerror(char * msg){
    printf("error: line %d: %s\n", yylineno, msg);
    return 0;
}

void fillParamList(int functionIndex, tree* t)
{
    if(t->numChildren == 0 && t->nodeKind == IDENTIFIER)
    {
        // fill parameter list with needed data, incrementing number of params as we go
        param newParam;
        newParam.data_type = strTable[t->val].data_type;
        newParam.symbol_type = strTable[t->val].symbol_type;
        strTable[functionIndex].parameterList[strTable[functionIndex].numParams++] = newParam;
    }
    else
    {
        for(int i = 0; i < t->numChildren; i++)
        {
            // recursively fill the list as we traverse the tree
            fillParamList(functionIndex, t->children[i]);
        }
    }
}

int getArgList(param* argList, int listSize, tree* t)
{
    if(t->numChildren == 0)
    {
        param newParam;
        if(t->nodeKind == IDENTIFIER)
        {
             newParam.data_type = strTable[t->val].data_type;
             newParam.symbol_type = strTable[t->val].symbol_type;
        }
        else if(t->nodeKind == INTEGER)
        {
             newParam.data_type = INT_TYPE;
             newParam.symbol_type = SCALAR;
        }
        else if(t->nodeKind == CHAR)
        {
             newParam.data_type = CHAR_TYPE;
             newParam.symbol_type = SCALAR;
        }
        argList[listSize] = newParam;
        return listSize + 1;
    }
    else
    {
        for(int i = 0; i < t->numChildren; i++)
        {
            listSize = getArgList(argList, listSize, t->children[i]);
        }
    }
    return listSize;
}

