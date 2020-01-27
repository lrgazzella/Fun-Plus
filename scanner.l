%{
  #include <llvm-c/Core.h>
  #include "y.tab.h"
  #include "utils.h"
%}

DQUOTE      \"
BACKSLASH   \\
ESCAPE      [n | t | \\ | \" | \']

%x quote
%x escape
%option noyywrap
/* [\x0A | \x09 | \x5C | \x22 | \x27]  =  \n, \t, \\, \", \' */
%%
if                     return IF_KW;
then                   return THEN_KW;
else                   return ELSE_KW;
while                  return WHILE_KW;
do                     return DO_KW;
var                    return VAR_KW;
let                    return LET_KW;
in                     return IN_KW;
true                   return LIT_TRUE;
false                  return LIT_FALSE;

{DQUOTE}               {yylval.string = strdup(""); BEGIN(quote);}
<quote>[^\\\"]*        {yylval.string = strcat_(yylval.string, yytext);}
<quote>{BACKSLASH}     {BEGIN(escape);}
<quote>{DQUOTE}        {BEGIN(INITIAL); return STRING_KW;}
<quote><<EOF>>         return ERROR_KW;
<escape>{ESCAPE}       {int len = strlen(yylval.string);
                        yylval.string = realloc(yylval.string, len + 2);
                        yylval.string[len] = escaped(yytext[0]);
                        BEGIN(quote);}
<escape>.              return ERROR_KW;

[A-Za-z_][A-Za-z_0-9]* { yylval.ident = strdup(yytext); return IDENTIFIER; }
[0-9]+                 { yylval.lit_value = atoi(yytext); return VAL; }
[-+*/\n()<>=&|!]       return *yytext;
\<=                    return LE;
\>=                    return GE;
!=                     return NE;
:=                     return ASSIGN_OP;
[[:space:]]            /* ignore */
.                      return *yytext;

%%
