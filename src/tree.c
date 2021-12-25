#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

char* nodeTypesStrings[] = {
"program", "declList", "decl", "varDecl", "typeSpec", "funDecl",
"formalDeclList", "formalDecl", "funBody", "localDeclList",
"statementList", "statement", "compoundStmt", "assignStmt",
"condStmt", "loopStmt", "returnStmt", "expression", "relop",
"addExpr", "addOp", "term", "mulOp", "factor", "funcCallExpr",
"argList", "integer", "identifier", "var", "arrayDecl", "char",
"string", "funcTypeName"
};

char* dataTypesStrings[] = {
"int", "char", "void"
};

char* opTypesStrings[] = {
"+", "-", "*", "/", "<", "<=", "==", ">=", ">", "!="
};

// Builds sub tree with zero children
tree* maketree(int kind)
{
    tree* newTree = malloc(sizeof(tree));

    newTree->nodeKind = kind;
    newTree->numChildren = 0;
    newTree->val = 0;
    newTree->strVal = 0;
    newTree->parent = 0;

    return newTree;
}

// Builds sub tree with leaf node (leaf node has a value)
tree* maketreeWithVal(int kind, int val, char* strVal)
{
    tree* newTree = malloc(sizeof(tree));

    newTree->nodeKind = kind;
    newTree->numChildren = 0;
    newTree->val = val;
    if(strVal != NULL)
    {
	newTree->strVal = malloc(strlen(strVal) + 1);
	strcpy(newTree->strVal, strVal);
    }
    newTree->parent = 0;

    return newTree;
}

// Assigns child as a child of parent
void addChild(tree* parent, tree* child)
{
    if(parent->numChildren < MAXCHILDREN)
    {
	parent->numChildren++;
	parent->children[parent->numChildren - 1] = child;
	child->parent = parent;
    }
}

// Prints ast recursively
void printAst(tree* root, int nestLevel)
{
    if(root != NULL)
    {
	// skip this node if empty LOCALDECLLIST or STATEMENTLIST
	if((root->nodeKind == LOCALDECLLIST || root->nodeKind == STATEMENTLIST) && root->numChildren == 0)
	{
	    return;
	}	

	// indent based on nestLevel
	for(int i = 1; i < nestLevel; i++)
	{
	    printf("  ");
	}

	// print current nodeKind (and val if appropriate)
	printf("%s", nodeTypesStrings[root->nodeKind]);
	
	switch(root->nodeKind)
	{
	    case INTEGER:
		printf(",%i", root->val);
		break;
	    case CHAR:
		printf(",%c", root->val);
		break;
	    case STRING:
		printf(",%s", root->strVal);
		break;
	    case IDENTIFIER:
		if(root->val != -1)
		{
		    printf(",%s", strTable[root->val].id);
		}
		else
		{
		    printf(",(undeclared variable)");
		}
		break;
	    case TYPESPEC:
		printf(",%s", dataTypesStrings[root->val]);
		break;
	    case ADDOP:
	    case MULOP:
	    case RELOP:
		printf(",%s", opTypesStrings[root->val]);
		break;
	}

	printf("\n");	

	// print children recursively
	for(int i = 0; i < root->numChildren; i++)
	{
	    printAst(root->children[i], nestLevel + 1);
	}
    }
}
