%{
#include <stdio.h>
#include <string.h>
#include <math.h>

extern FILE* yyin;



#define MAX_ID_COUNT 1000
#define MAX_ID_SIZE 100
#define MAX_ARR_SIZE 1000


/* to keep identifier values*/
char id_list[MAX_ID_COUNT][MAX_ID_SIZE];
int value_list[MAX_ID_COUNT];
int id_count = 0;
/* for output file */
FILE *ofp;



int yylex();

int yyerror(char *); 

int setID(char* , int );

int getID(char* );

void print_list(int* , int );

void copy_arr(int [], int [], int );

void concat(int [], int [], int [], int , int );

void append(int [], int , int [], int );


%}

 

%union{
    struct{
        int value;
        char id[20];
        int arr[1000];
        int arr_size ;
    };   
}

%start INPUT

%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA 
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE



%token COMMENT
%token <value> VALUE
%token <id> IDENTIFIER


%type <value> INPUT
%type <value> EXPI
%type <value> EXPB
%type <arr> VALUES
%type <arr> EXPLISTI



%%

INPUT: 
    EXPI {fprintf(ofp, "SYNTAX OK. Result = %d\n", $<value>1);} |
    EXPLISTI { fprintf(ofp, "SYNTAX OK. Result = "); print_list($<arr>1,$<arr_size>1); } |
    COMMENT {fprintf(ofp, "SYNTAX OK. Result = COMMENT\n");} |
    INPUT COMMENT {fprintf(ofp, "SYNTAX OK. Result = COMMENT\n");} |
    INPUT EXPLISTI { fprintf(ofp, "SYNTAX OK. Result = "); print_list($<arr>2,$<arr_size>2);  } |
    INPUT EXPI {fprintf(ofp, "SYNTAX OK.  Result = %d\n", $<value>2);} ;



EXPI:
    /* Arithmatic operations */
    OP_OP OP_PLUS EXPI EXPI OP_CP  { $<value>$ = $<value>3 + $<value>4; }    |

    OP_OP OP_MINUS EXPI EXPI OP_CP { $<value>$ = $<value>3 - $<value>4; }    |

    OP_OP OP_MULT EXPI EXPI OP_CP  { $<value>$ = $<value>3 * $<value>4; }    |

    OP_OP OP_DIV EXPI EXPI OP_CP   { $<value>$ = $<value>3 / $<value>4; }    |

    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$<value>$ = pow($<value>3, $<value>4);}|



    /* Assignment operation */
    OP_OP KW_SET IDENTIFIER EXPI OP_CP { $<value>$ = setID($<id>3, $<value>4); }  |



    /* Control statement */
    OP_OP KW_IF EXPB EXPI OP_CP {   if  ($<value>3 == 1)   $<value>$ = $<value>4;
                                    else                   $<value>$ = 0;}      |



    /* Basic values */
    IDENTIFIER  { $<value>$ = getID($<id>1); }  |

    VALUE       { $<value>$ = $<value>1; }      ;




EXPB:
    /* Logic operations */
    OP_OP KW_AND EXPB EXPB OP_CP { $<value>$ = $<value>3 && $<value>4; }     |

    OP_OP KW_OR EXPB EXPB OP_CP  { $<value>$ = $<value>3 || $<value>4; }     |

    OP_OP KW_NOT EXPB OP_CP  { $<value>$ = !$<value>3; }                     |



    /* Compare operations */
    OP_OP KW_EQUAL EXPI EXPI OP_CP { $<value>$ = ($<value>3 == $<value>4); } |

    OP_OP KW_EQUAL EXPB EXPB OP_CP { $<value>$ = ($<value>3 == $<value>4); } |



    /* Basic values */
    KW_TRUE  { $<value>$ = 1; }  |
    KW_FALSE   { $<value>$ = 0; };



EXPLISTI:
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {   concat($<arr>$, $<arr>3, $<arr>4, $<arr_size>3, $<arr_size>4); 
                                                $<arr_size>$ = $<arr_size>3 + $<arr_size>4;}            |

    OP_OP KW_APPEND EXPI EXPLISTI OP_CP {       append($<arr>$, $<value>3, $<arr>4, $<arr_size>4); 
                                                $<arr_size>$ = $<arr_size>4 + 1;}                       |

    OP_OP KW_IF EXPB EXPLISTI OP_CP {           if  ($<value>3 == 1)   { copy_arr($<arr>$, $<arr>4, $<arr_size>4); $<arr_size>$ = $<arr_size>4; }
                                                else                   $<arr_size>$ = 0;}               | 

    OP_OP KW_LIST VALUES OP_CP { copy_arr($<arr>$,$<arr>3,$<arr_size>3); $<arr_size>$ = $<arr_size>3;};



VALUES: 
    VALUES VALUE  { $<arr>$[$<arr_size>$] = $<value>2;
                    $<arr_size>$ = $<arr_size>1 + 1;};  |

    VALUE {         $<arr>$[$<arr_size>$] = $<value>1;
                    $<arr_size>$ = 1;}                  ;


%%


int yyerror(char *err) {
    fprintf(ofp, "%s\n",err);
    return 0;
}

int yywrap(){
    return 1;
}

int setID(char* str, int val){
    int i;

    for( i = 0; i < id_count; i++){
        if (strcmp(id_list[i], str) == 0 ){
            value_list[i] = val;
            return val;
        }    
    }
    if (id_count < MAX_ID_COUNT){
        strcpy(id_list[id_count], str);
        value_list[id_count] = val;
        id_count++;
        return val;
    }
    return -1;
}

int getID(char* str){
    int i;

    for( i = 0; i < id_count; i++){
        if (strcmp(id_list[i], str) == 0 ){
            return value_list[i];
        }     
    }
    
    return 0;
}

void print_list(int* arr, int size){
    fprintf(ofp, "(");
    int i;
    for( i = 0; i < size; i++)
        fprintf(ofp, "%d ", arr[i]);
    fprintf(ofp, ")\n");
}

void copy_arr(int arr1[], int arr2[], int size){

    int i;
    for(i = 0; i < size && i < (MAX_ARR_SIZE -1); i++){
        arr1[i] = arr2[i];
    }
}

void concat(int res[], int arr_1[], int arr_2[], int arr_1_size, int arr_2_size){

    int i, j;

    for( i = 0; i < arr_1_size; i++)
        res[i] = arr_1[i];

    for( j = 0; j < arr_2_size; j++)
        res[i+j] = arr_2[j];
}

void append(int res[], int val, int arr[], int arr_size){

    int i;

    res[0] = val;

    for( i = 1; i <= arr_size; i++)
        res[i] = arr[i-1];
}


int main(int argc, char **argv)
{
	int fromFile, exit;

	FILE *fp;

	ofp = fopen("parsed_cpp.txt", "w");

	if(argc > 1){
		fp = fopen(argv[1], "r");

		if(fp == NULL){
			printf("Error! File cannot openned!\n");
		}
		else{
			yyin = fp;
			yyparse();
		}

	}
	else{
		yyparse();

	}

	

    
    
    
    return 0;
}