%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1024
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

int nested_comment_level=0; /* count the nested '(**)' comment */

/* define macros to accelerate */

#define ADD_ID (yylval.symbol = idtable.add_string(yytext, yyleng)) /* dump id from yytext */
#define ADD_INT (yylval.symbol = inttable.add_string(yytext, yyleng)) /* dump integer from yytext */
#define ADD_STR (yylval.symbol = stringtable.add_string(yytext, yyleng)) /* dump string from yytext */
#define ADD_BOOL (yylval.boolean = yytext[0]=='t'?1:0) /* dump boolean from yytext */
#define ADD_ERR(s) (cool_yylval.error_msg=s) /* deliver custom error message */

#define ADD_CHAR(c) (*(string_buf_ptr++)=c) /* assemble string */
#define HAVE_BUF (string_buf_ptr-string_buf<MAX_STR_CONST) /* whether buffer is full */
#define CHECK_IF_TOO_LONG(c) {if(HAVE_BUF)ADD_CHAR(c);else {ADD_ERR("String constant too long");BEGIN(RESUME_AFTER_STR);return ERROR;}} /* check length and assemble string or return error*/ 
%}

%option noyywrap

/* define states */
/* whether in a '(**)' comment */
%x COMMENT 
/* whether assembling a string */
%x STRCONST 
/* whether ignoring an incomplete string */
%x RESUME_AFTER_STR 

/* basic element */
digit               [0-9]
token_char          [A-Za-z0-9_]
WS                  [ \t\r\f\v]

/* one-line comment */
COMMENT_INLINE      --.*

/* tokens defined in `cool-parse.h` */
CLASS           (?i:class)
ELSE            (?i:else)
FI              (?i:fi)
IF              (?i:if)
IN              (?i:in)
INHERITS        (?i:inherits)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
ESAC            (?i:esac)
OF              (?i:of)
DARROW          =>
NEW             (?i:new)
ISVOID          (?i:isvoid)
INT_CONST       {digit}+
BOOL_CONST      t(?i:rue)|f(?i:alse)
TYPEID          [A-Z]{token_char}*
OBJECTID        [a-z]{token_char}*
ASSIGN          <-
NOT             (?i:not)
LE              <=

/* single valid characters */
SYNTACTIC       ","|";"|":"|"("|")"|"@"|"{"|"}"
OPERATOR        "+"|"/"|"-"|"*"|"="|"<"|"."|"~"


%%

 /* ignore whitespaces */
{WS}                            ;

 /* ignore one-line comment */
{COMMENT_INLINE}                ;


 /* deal with nested `(**)` comment */
 /* step in when meet `(*` */
"(*"                            {++nested_comment_level;
                                BEGIN(COMMENT);}
 /* report unmatched `*)` */
<INITIAL>"*)"                   {ADD_ERR("Unmatched *)");
                                return ERROR;}
<COMMENT>{
 /* step out when meet `*)` */
"*)"                            {--nested_comment_level;
                                if(nested_comment_level==0){
                                BEGIN(INITIAL);
                                }}
"(*"                            {++nested_comment_level;}
\n                              {++curr_lineno;}
.                               ;
 /* report when meet EOF */
<<EOF>>                         {ADD_ERR("EOF in comment");
                                BEGIN(INITIAL);
                                return ERROR;}
}


 /* deal with string */
 /* enter <STRCONST> and reset buffer pointer when meet `"` */
\"                              {string_buf_ptr=string_buf;
                                BEGIN(STRCONST);}
<STRCONST>{
 /* escape when meet `"` and judge if string has null character, and add string to string table from buffer */
\"                              {for(register auto i=string_buf;i!=string_buf_ptr;++i)
                                if((*i)==0){
                                ADD_ERR("String contains null character.");
                                BEGIN(INITIAL);
                                return ERROR;}
                                yylval.symbol = stringtable.add_string(string_buf, string_buf_ptr-string_buf);
                                BEGIN(INITIAL);
                                return STR_CONST;
                                }
 /* report unescaped newline */
\n                              {++curr_lineno;
                                ADD_ERR("Unterminated string constant");
                                BEGIN(INITIAL);
                                return ERROR;
                                }
 /* pick escaped newline (also work for CRLF) */
\\\r?\n                         {++curr_lineno;
                                CHECK_IF_TOO_LONG(yytext[1]);}
 /* translate meaningful escape */
\\b                             {CHECK_IF_TOO_LONG('\b');}
\\t                             {CHECK_IF_TOO_LONG('\t');}
\\n                             {CHECK_IF_TOO_LONG('\n');}
\\f                             {CHECK_IF_TOO_LONG('\f');}
 /* copy meaningless escape except `\0` */
\\[^btnf\0]                     {CHECK_IF_TOO_LONG(yytext[1]);}
 /* simply copy */
.                               {CHECK_IF_TOO_LONG(yytext[0]);}
 /* report when meet EOF */
<<EOF>>                         {ADD_ERR("EOF in string constant");
                                BEGIN(INITIAL);
                                return ERROR;}
}


 /* ignore the rest of incomplete string */
<RESUME_AFTER_STR>{
 /* new line is ok */
\n                              {++curr_lineno;
                                BEGIN(INITIAL);}
 /* ignore all character and `\"` except `"` */
\\\"|[^\"]                      ;
 /* end `"` is ok */
\"                              BEGIN(INITIAL);
}

 /* simple token */
{CLASS}                         return CLASS;
{ELSE}                          return ELSE;
{FI}                            return FI;
{IF}                            return IF;
{IN}                            return IN;
{INHERITS}                      return INHERITS;
{LET}                           return LET;
{LOOP}                          return LOOP;
{POOL}                          return POOL;
{THEN}                          return THEN;
{WHILE}                         return WHILE;
{CASE}                          return CASE;
{ESAC}                          return ESAC;
{OF}                            return OF;
{DARROW}                        return DARROW;
{NEW}                           return NEW;
{ISVOID}                        return ISVOID;
{NOT}                           return NOT;
{ASSIGN}                        return ASSIGN;
{LE}                            return LE;

 /* token need to dump from yytext */
{INT_CONST}                     {ADD_INT;return INT_CONST;}
{BOOL_CONST}                    {ADD_BOOL;return BOOL_CONST;}
{TYPEID}                        {ADD_ID;return TYPEID;}
{OBJECTID}                      {ADD_ID;return OBJECTID;}

 /* character of length 1 */
{SYNTACTIC}|{OPERATOR}          return * yytext;

 /* count line number */
\r?\n                           {++curr_lineno;}

 /* report all invalid characters */
.                               {ADD_ERR(yytext);return ERROR;}

%%
