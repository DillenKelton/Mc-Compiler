
/* definitions */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <../obj/y.tab.h>

int scancol = 1;
int yycol = 1;
int scanlineno = 1;
char* yyerror;

void updateCol(); 
void countLines(); 
int processString(); 
%}

newline         \n
whitespace      [ \t]
integer         0|[1-9][0-9]*
integerlead0    0[0-9]+
character       \'([^\'\n]|\\[\\nt\'])\'

/* String can contain any characters between the double quotes other than a newline or
// unescaped doublequotes.
*/
string       \"([^\"\n]|\\[\\nt\"])*\"

/* multiline string has \n somewhere in the middle */
multlnstring \"([^\"]|\\[\\nt\"])*\"

/* If the end quote is found, one of the two regexes above will be matched. Otherwise,
// the string is unterminated.
*/
untermstring \"([^\"\n]|\\[\\nt\"])*\n

comment         \/\*.*\*\/
multlncomment   \/\*(.|\n)*?\*\/
untermcomment   \/\*.*\n

identifier      [A-Za-z][A-Za-z0-9]*
illidentifier   [0-9]+[A-Za-z][A-Za-z0-9]*

int yylineno = 1;
%%
 /* rules */
 /* Keywords */;
if {countLines(); updateCol(); return KWD_IF;}
else {countLines(); updateCol(); return KWD_ELSE;}
while {countLines(); updateCol(); return KWD_WHILE;}
int {countLines(); updateCol(); return KWD_INT;}
string {countLines(); updateCol(); return KWD_STRING;}
char {countLines(); updateCol(); return KWD_CHAR;}
return {countLines(); updateCol(); return KWD_RETURN;}
void {countLines(); updateCol(); return KWD_VOID;}

 /* Operators */;
\+ {updateCol(); countLines();  return OPER_ADD;}
\- {updateCol(); countLines();  return OPER_SUB;}
\* {updateCol(); countLines(); return OPER_MUL;}
\/ {updateCol(); countLines(); return OPER_DIV;}
\< {updateCol(); countLines(); return OPER_LT;}
\> {updateCol(); countLines(); return OPER_GT;}
\>\= {updateCol(); countLines(); return OPER_GTE;}
\<\= {updateCol(); countLines(); return OPER_LTE;}
\=\= {updateCol(); countLines(); return OPER_EQ;}
\!\= {updateCol(); countLines(); return OPER_NEQ;}
\= {updateCol(); countLines(); return OPER_ASGN;}
\@ {updateCol(); countLines(); return OPER_AT;}
\+\+ {updateCol(); countLines(); return OPER_INC;}
\-\- {updateCol(); countLines(); return OPER_DEC;}
\% {updateCol(); countLines(); return OPER_MOD;}
\&\& {updateCol(); countLines();  return OPER_AND;}
\|\| {updateCol(); countLines(); return OPER_OR;}
\! {updateCol(); countLines(); return OPER_NOT;}

 /* Special symbols */
\[ {updateCol(); countLines(); return LSQ_BRKT;}
\] {updateCol(); countLines(); return RSQ_BRKT;}
\{ {updateCol(); countLines(); return LCRLY_BRKT;}
\} {updateCol(); countLines();  return RCRLY_BRKT;}
\( {updateCol(); countLines(); return LPAREN;}
\) {updateCol(); countLines(); return RPAREN;}
\, {updateCol(); countLines(); return COMMA;}
\; {updateCol(); countLines();  return SEMICLN;}

 /* Constants */;
{integer}       {yylval.value = atoi(yytext);
		 countLines(); updateCol();
		 return INTCONST;}

{integerlead0}  {countLines(); updateCol(); yyerror = "Integers may not have leading zeros"; return ERROR;}
{character}     {countLines();
		 updateCol();
		 yylval.value = yytext[1];
		 return CHARCONST;}

{string}        {countLines(); updateCol(); return processString();}
{multlnstring}  {updateCol(); countLines();  yyerror = "String spans multiple lines"; return ERROR;}
{untermstring}  {updateCol(); countLines();  yyerror = "Unterminated string"; return ERROR;}

 /* Identifiers */;
{identifier}    {yylval.strval = (char *) malloc(yyleng *sizeof(char));
                 strcpy(yylval.strval, yytext);
		 countLines(); updateCol(); 
		 return ID;}

{illidentifier} {countLines(); updateCol();  yyerror = "Illegal identifier"; return ERROR;}

 /* Comments */;
{comment}       //Ignore
{multlncomment} {countLines();}
{untermcomment} {updateCol(); countLines(); yyerror = "Unterminated comment"; return ERROR;}

 /* Other */;
{newline}       {countLines();}
{whitespace}    {updateCol();}
.               {return ILLEGAL_TOK;}

%%

/* user routines */

void updateCol(){
	yycol = scancol;
	scancol += yyleng;
}

void countLines(){
	yylineno = scanlineno;	
	for(int i = 0; i < yyleng; i++)
		if(yytext[i] == '\n'){ // scans for newlines within tokens
			scanlineno++;
			scancol = 1; // resets column to 1
		}
}

int processString(){
	char* buffer = (char*)malloc(yyleng + 1); // allocate memory for a temp string buffer (enough for all chars plus null terminator)

	// process string stored in yytext and store new string in buffer
	for(int i = 0, bufferCount = 0; i <= yyleng; i++)
	{
		if(yytext[i] == '\\') // check for possible escape character
		{
			char nextChar = yytext[++i];
			switch(nextChar)
			{
				case 'n': // newline
					buffer[bufferCount++] = '\n';
					break;
				case 't': // tab
					buffer[bufferCount++] = '\t';
					break;
				case '"': // quote
					buffer[bufferCount++] = '"';
					break;
				case '\\': // backslash
					buffer[bufferCount++] = '\\';
					break;
				default:  // invalid escape char - error
					yyerror = "Unrecognized escape character in String";
					yycol += i; //added to identify the column location of the invalid escape char
					return ERROR;
			}
		}
		else if(yytext[i] != '"') // we want to ignore the non-escape quotation marks
		{
			buffer[bufferCount++] = yytext[i]; // copy non-escape character to buffer
		}
	}
	strcpy(yytext, buffer); // copy buffer string to yytext

	yylval.strval = (char *) malloc(yyleng *sizeof(char)); //copy to yyval for parser
        strcpy(yylval.strval, yytext);

	free(buffer); // free up memory used by temporary string buffer
	return STRCONST;
}
