/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* Add your own C declarations here */
// This function is to ouput the verbose message of a parse error.
void verr(const char* str,const char* para=0);
/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern char *curr_filename;
Program ast_root;            /* the result of the parse  */
Classes parse_results;       /* for use in semantic analysis */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;

%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <features> dummy_feature_list feature_list
%type <feature> feature
%type <formals> dummy_formal_list formal_list
%type <formal> formal
%type <expressions> dummy_actual_list actual_list exprs
%type <expression> assign_free expr let_body
%type <cases> cases
%type <case_> case


/* Precedence declarations go here. */

%right IN       // IN least priority s.t. LET extends as far to the right as possible
%right ASSIGN
%right NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%right '~'
%left '@'
%left '.'

 %%
// Save the root of the abstract syntax tree in a global variable.
program : class_list { ast_root = program($1); }
        ;

class_list
        : class                         // single class 
                { $$ = single_Classes($1); }
        | class_list class              // several classes 
                { $$ = append_Classes($1,single_Classes($2)); }
        ;

// If no parent is specified, the class inherits from the Object class. 
class   : CLASS TYPEID '{' dummy_feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }

        | CLASS error CLASS                                             // Restart at the next class definition
                { verr("Skipping to next class due to a syntax error.\n"); 
                YYBACKUP(CLASS,yylval);
                yyerrok; }
        | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' error // Restart at the next class definition
                { verr("Semicolon missing from end of declaration of class %s.\n",$2->get_string()); 
                yyerrok; }
        | CLASS TYPEID '{' dummy_feature_list '}' error                 // Restart at the next class definition
                { verr("Semicolon missing from end of declaration of class %s.\n",$2->get_string()); 
                yyerrok; }

        ;

// Feature list may be empty, but no empty features in list.
dummy_feature_list
        :       {  $$ = nil_Features(); }
        | feature_list
                {  $$ = $1; }
        ;

// Return no_expr when there is no initialization
assign_free
        :       { $$ = no_expr(); }
        | ASSIGN expr
                { $$ = $2; }
        ;

feature_list
        : feature                       // single feature
                { $$ = single_Features($1); }
        | feature_list feature          // several features
                { $$ = append_Features($1,single_Features($2)); }
        ;

feature : OBJECTID '(' dummy_formal_list ')' ':' TYPEID '{' expr '}' ';'        // method
                { $$ = method($1,$3,$6,$8); }
        | OBJECTID ':' TYPEID assign_free ';'                                   // attribute
                { $$ = attr($1,$3,$4); }
        
        | OBJECTID '(' dummy_formal_list ')' ':' TYPEID '{' expr '}' error      // Semicolon missing
                { verr("Semicolon missing from end of declaration of method %s.\n",$1->get_string()); 
                yyerrok; }
        | OBJECTID '(' dummy_formal_list ')' ':' error '{' expr '}' ';'         // error type
                { verr("Syntax error in the type of method %s.\n",$1->get_string()); 
                yyerrok; }
        | OBJECTID ':' TYPEID assign_free error                                 // semicolon missing
                { verr("Semicolon missing from end of declaration of attribute %s.\n",$1->get_string()); 
                yyerrok; }
        | OBJECTID ':' TYPEID ASSIGN error ';'                                  // attribute init error
                { verr("Syntax error in initialization of attribute %s.\n",$1->get_string()); 
                yyerrok; }
        | OBJECTID error ';'                                                    // other error
                { verr("Syntax error in attribute or method %s.\n",$1->get_string()); 
                yyerrok; }
        | error OBJECTID                                                        // skipping to next feature
                { verr("Skipping to next feature due to a syntax error.\n"); 
                YYBACKUP(OBJECTID,yylval);
                yyerrok; }
        ;

// Formal list may be empty, but no empty formals in list.
dummy_formal_list
        :       { $$ = nil_Formals(); }
        | formal_list
                { $$ = $1; }
        
        | error ')'     // skip the formal list when error
                { verr("Syntax error in the formal argument.\n"); 
                YYBACKUP(')',yylval);
                yyerrok; }
        ;

formal_list
        : formal                        // single formal
                { $$ = single_Formals($1); }
        | formal_list ',' formal        // several formals
                { $$ = append_Formals($1,single_Formals($3)); }
        ;

formal  : OBJECTID ':' TYPEID
                { $$ = formal($1,$3); }
        ;

// Actual list may be empty, but no empty actuals in list.
dummy_actual_list
        :       { $$ = nil_Expressions(); }
        | actual_list
                { $$ = $1; }
        
        // skip the actual list when error
        | error ')'
                { verr("Syntax error in the dispatch argument.\n"); 
                YYBACKUP(')',yylval);
                yyerrok; }
        ;
        
actual_list
        : expr                          // single actual
                { $$ = single_Expressions($1); }
        | actual_list ',' expr          // several actuals
                { $$ = append_Expressions($1,single_Expressions($3)); }
        ;

// non-empty expressions with ';'
exprs   : expr ';'
                { $$ = single_Expressions($1); }
        | exprs expr ';'
                { $$ = append_Expressions($1,single_Expressions($2)); }
        
        | error                 // other error
                { verr("Syntax error in expression of block.\n"); 
                yyclearin;
                yyerrok; }
        | expr error            // maybe miss semicolon
                { verr("Maybe you forgot a semicolon (;).\n"); 
                yyerrok; }
        | exprs expr error      // maybe miss semicolon
                { verr("Maybe you forgot a semicolon (;).\n"); 
                yyerrok; }
        ;

// print let error message promptly using a specified reduction
let_error_prompt_nonlast
        : error ','     // skip to next variable
                { verr("Syntax error in the let variable declaration.\n"); 
                yyclearin;
                yyerrok; }

let_body
        : OBJECTID ':' TYPEID assign_free IN expr       // single assignment
                { $$ = let($1,$3,$4,$6); }
        | OBJECTID ':' TYPEID assign_free ',' let_body  // several assignments
                { $$ = let($1,$3,$4,$6); }

        | let_error_prompt_nonlast let_body             // handling nonlast variable error
                {}
        | error IN expr                                 // handling last variable error
                { verr("Syntax error in the last let variable declaration.\n"); 
                yyerrok; }
        ;

// print case head error message promptly using a specified reduction
case_error_prompt_head
        : error { verr("Syntax error in the case head expression.\n");
                yyclearin; 
                yyerrok; }

case    : OBJECTID ':' TYPEID DARROW expr ';'
                { $$ = branch($1,$3,$5); }
        
        | error ';'     // skip to next case branch
                { verr("Syntax error in the case branch.\n"); 
                yyclearin;
                yyerrok; }
        ;

cases   : case          // single case
                { $$ = single_Cases($1); }
        | cases case    // several cases
                { $$ = append_Cases($1,single_Cases($2)); }
        ;

expr    : OBJECTID ASSIGN expr                                          // assignment
                { $$ = assign($1,$3); }
        | expr '@' TYPEID '.' OBJECTID '(' dummy_actual_list ')'        // static call
                { $$ = static_dispatch($1,$3,$5,$7); }
        | expr '.' OBJECTID '(' dummy_actual_list ')'                   // dynamic call
                { $$ = dispatch($1,$3,$5); }
        | OBJECTID '(' dummy_actual_list ')'                            // call
                { $$ = dispatch(object(idtable.add_string("self")),$1,$3); }
        | IF expr THEN expr ELSE expr FI                                // if statement
                { $$ = cond($2,$4,$6); }
        | WHILE expr LOOP expr POOL                                     // while statement
                { $$ = loop($2,$4); }
        | '{' exprs '}'                                                 // expression block
                { $$ = block($2); }
        | LET let_body                                                  // variable declaration
                { $$ = $2; }
        | CASE expr OF cases ESAC                                       // case statement
                { $$ = typcase($2,$4); }
        | NEW TYPEID                                                    // instantiation
                { $$ = new_($2); }
        | ISVOID expr                                                   // isvoid or not
                { $$ = isvoid($2); }
        | expr '+' expr 
                { $$ = plus($1,$3); }
        | expr '-' expr
                { $$ = sub($1,$3); }
        | expr '*' expr
                { $$ = mul($1,$3); }
        | expr '/' expr
                { $$ = divide($1,$3); }
        | '~' expr                                                      // nagative
                { $$ = neg($2); }
        | expr '<' expr
                { $$ = lt($1,$3); }
        | expr LE expr
                { $$ = leq($1,$3); }
        | expr '=' expr
                { $$ = eq($1,$3); }
        | NOT expr                                                      // negation
                { $$ = comp($2); }
        | '(' expr ')'                                                  // parentheses
                { $$ = $2; }
        | OBJECTID
                { $$ = object($1); }
        | INT_CONST
                { $$ = int_const($1); }
        | STR_CONST
                { $$ = string_const($1); }
        | BOOL_CONST
                { $$ = bool_const($1); }

        | WHILE error POOL                                              // general errors in while
                { verr("Syntax error in the while statement.\n"); 
                yyerrok; }
        | IF error FI                                                   // general errors in if
                { verr("Syntax error in the if statement.\n"); 
                yyerrok; }
        | CASE case_error_prompt_head OF cases ESAC                     // head errors in case
                {}
        ;

/* end of grammar */
 %%
// This function is to ouput the verbose message of a parse error.
void verr(const char* str,const char* para) {if(VERBOSE_ERRORS)fprintf(stderr,str,para);}

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>20) {
      if (VERBOSE_ERRORS)
         fprintf(stderr, "More than 20 errors\n");
      exit(1);
  }
}

