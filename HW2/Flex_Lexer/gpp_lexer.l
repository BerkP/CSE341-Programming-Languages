%{ 
#include <stdio.h>
#include "tokens.h"
#include <string.h>

#define TRUE 1
#define FALSE 0

#define OPEN 1
#define CLOSE 0
int op_c_status = CLOSE;  // Status for " operator. Open/Close

const char tokenNames[][15] = { "", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_SET", "KW_DEFFUN", "KW_FOR",
    						  "KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISP", "KW_TRUE", "KW_FALSE", "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_DBLMULT", "OP_MULT", "OP_OP", "OP_CP",
    						  "OP_OC", "OP_CC", "OP_COMMA", "COMMENT", "IDENTIFIER", "VALUE", "NEWLINE", "SPACE", "INVALID"};


int oc_or_cc();

%}

%%

[aA][nN][dD]					return KW_AND;
[oO][rR]						return KW_OR;
[nN][oO][tT]					return KW_NOT;
[eE][qQ][uU][aA][lL] 			return KW_EQUAL;
[lL][eE][sS][sS]				return KW_LESS;
[nN][iI][lL]					return KW_NIL;
[lL][iI][sS][tT]				return KW_LIST;
[aA][pP][pP][eE][nN][dD]		return KW_APPEND;
[cC][oO][nN][cC][aA][tT]		return KW_CONCAT;
[sS][eE][tT]					return KW_SET;
[dD][eE][fF][fF][uU][nN]		return KW_DEFFUN;
[fF][oO][rR]					return KW_FOR;
[iI][fF]						return KW_IF;
[eE][xX][iI][tT]				return KW_EXIT;
[lL][oO][aA][dD]				return KW_LOAD;
[dD][iI][sS][pP]				return KW_DISP;
[tT][rR][uU][eE]				return KW_TRUE;
[fF][aA][lL][sS][eE]			return KW_FALSE;

[+] 							return OP_PLUS;
[-] 							return OP_MINUS;
[/] 							return OP_DIV;
[*][*] 							return OP_DBLMULT;
[*] 							return OP_MULT;
[(] 							return OP_OP;
[)] 							return OP_CP;
["] 							return oc_or_cc();

[,]								return OP_COMMA;

[;][;] 							return COMMENT;

[a-zA-Z_][a-zA-Z0-9_]*			return IDENTIFIER;

[0]|[1-9][0-9]*					return VALUE;		   



[\n]							return NEWLINE;
[ \t]+							return SPACE;
.								return INVALID;

%% 

int yywrap(){}


int main( int argc, char *argv[] ){

	int fromFile, exit;

	FILE *fp;
	FILE *ofp;

	if(argc > 1){
		fp = fopen(argv[1], "r");

		if(fp == NULL){
			exit = TRUE;
			printf("Error! File cannot openned!\n");
		}
		else{
			
			exit = FALSE;
			fromFile = TRUE;
			yyin = fp;
			
		}

	}
	else{
		exit = FALSE;
		fromFile = FALSE;

	}

	ofp = fopen("parsed_cpp.txt", "w");
	
	int token = KW_TRUE;  		// Value of this is not important. Should not be 0 or INVALID
	int lastToken, isError = FALSE;
	char lastTokenStr[200];
	
	

	while( token != 0 && exit != TRUE ){

		lastToken = token;
		token = yylex();


		if( fromFile == FALSE && lastToken == NEWLINE && token == NEWLINE){
			
			exit = TRUE;
		}

		else if(token == COMMENT){
			
			printf("%s\n", tokenNames[token]);
			fprintf(ofp, "%s\n", tokenNames[token]);
			while(token != NEWLINE && token != 0)
				token = yylex();

		}

		else if ( (token >= KW_AND && token <= KW_FALSE) || token == IDENTIFIER  || token == VALUE || token == INVALID){

			if( token == INVALID){
				isError = TRUE;
			}

			lastToken = token;
			strcpy(lastTokenStr, yytext);
			

			token = yylex();


			while( !(token >= OP_PLUS && token <= OP_COMMA) && token != NEWLINE && token != SPACE && token != COMMENT){
				
				isError = TRUE;
				strcat(lastTokenStr,yytext);
				lastToken = token;
				token = yylex();

			}
			

			if(isError == TRUE){
				printf("ERROR %s cannot be tokenized!\n", lastTokenStr);
				fprintf(ofp, "ERROR %s cannot be tokenized!\n", lastTokenStr);
				isError = FALSE;
				
			}
			else{
				printf("%s\n", tokenNames[lastToken]);
				fprintf(ofp, "%s\n", tokenNames[lastToken]);
			}

			if(token >= OP_PLUS && token <= OP_COMMA ){
				printf("%s\n", tokenNames[token]);
				fprintf(ofp, "%s\n", tokenNames[token]);
			}
			else if(token == COMMENT){
				printf("%s\n", tokenNames[token]);
				fprintf(ofp, "%s\n", tokenNames[token]);
				while(token != NEWLINE && token != 0)
					token = yylex();

			}
			

		}

		else if( token >= OP_PLUS && token <= OP_COMMA ){
			printf("%s\n", tokenNames[token]);
			fprintf(ofp, "%s\n", tokenNames[token]);
		}
		

		
	}
	

}

int oc_or_cc(){
	if(op_c_status == OPEN){
		op_c_status = CLOSE;
		return OP_CC;
	}
	else{
		op_c_status = OPEN;
		return OP_OC;
	}
}
