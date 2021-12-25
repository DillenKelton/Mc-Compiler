#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "strtab.h"


/* Provided is a hash function that you may call to get an integer back. */
unsigned long hash(unsigned char *str)
{
    unsigned long hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int ST_insert(char *id, char *scope, int data_type, int symbol_type, int arraySize){
    char* buffer = malloc(strlen(id) + strlen(scope) + 1); // Create enough space to store id, scope, and null terminator
    strcpy(buffer, id); // Copy id to buffer
    strcat(buffer, scope); // Concatenate scope to id
    unsigned long hashKey = hash(buffer); // Get hash key
    free(buffer); // free memory used by buffer

    int index = ST_lookup(id, scope);

    if(index == -1)
    {
        // id+scope is not already in symbol table, so we need to add it
        int incr = 0;
        while(1)
	{
	    index = (hashKey + incr) % MAXIDS;
	    if(strTable[index].id == NULL)
	    {
		// Empty slot found, insert here
		strTable[index].id = malloc(strlen(id) + 1);
		strcpy(strTable[index].id, id);

		strTable[index].scope = malloc(strlen(scope) + 1);
		strcpy(strTable[index].scope, scope);

		strTable[index].data_type = data_type;
		strTable[index].symbol_type = symbol_type;
		strTable[index].arraySize = arraySize;
                strTable[index].numParams = 0;

		break; // break while loop
	    }
	    incr++;
	}
    }

    return index;
}

int ST_lookup(char *id, char *scope) {
    char* buffer = malloc(strlen(id) + strlen(scope) + 1); // Create enough space to store id, scope, and null terminator
    strcpy(buffer, id); // Copy id to buffer
    strcat(buffer, scope); // Concatenate scope to id
    unsigned long hashKey = hash(buffer); // Get hash key
    free(buffer); // free memory used by buffer

    int index = -1;
    int incr = 0;
    while(1)
    {
	index = (hashKey + incr) % MAXIDS;
	if(strTable[index].id == NULL)
	{
	    // Empty spot, id+scope is not in symbol table
	    index = -1;
	    break;
	}
	
	if(strcmp(strTable[index].id, id) == 0 && strcmp(strTable[index].scope, scope) == 0)
	{
	    // id and scope are equal, we found the correct index in the symbol table
	    break;
	}

	incr++;
    }

    return index;
}

void libraryFuncs(){
	int indx;
	indx =  ST_insert("output" , "global", VOID_TYPE, FUNCTION, NULL);
	param newParam;
        newParam.data_type = INT_TYPE;
        newParam.symbol_type = SCALAR;
        strTable[indx].parameterList[strTable[indx].numParams++] = newParam;
}

void print_sym_tab()
{
    printf("Symbol Table:\n");
    for(int i = 0; i < MAXIDS; i++)
    {
	if(strTable[i].id != NULL)
	{
	    // Non-empty spot, print
	    printf("[id: %s], [scope: %s], [data_type: %i], [symbol_type: %i]", strTable[i].id, strTable[i].scope, strTable[i].data_type, strTable[i].symbol_type);
            if(strTable[i].symbol_type == ARRAY)
            {
                printf(", [arraySize: %i]", strTable[i].arraySize);
            }
            else if(strTable[i].symbol_type == FUNCTION)
            {
                printf(", [numParams: %i]", strTable[i].numParams);
                for(int params = 0; params < strTable[i].numParams; params++)
                {
                    printf("\n\t[param %i]: [data_type: %i], [symbol_type: %i]", params, strTable[i].parameterList[params].data_type, strTable[i].parameterList[params].symbol_type);
                }
            }
            printf("\n");
	}
    }
}
