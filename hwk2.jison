
/* description: Parses and executes mathematical expressions. */

/* lexical grammar */
%lex
%%

\s+                   /* skip whitespace */
[0-9]+("."[0-9]+)?\b  return 'NUMBER'
"*"                   return '*'
"/"                   return '/'
"-"                   return '-'
"+"                   return '+'
">"                   return '>'
"<"                   return '<'
"=="                  return '=='
"("                   return '('
")"                   return ')'
"&&"                  return '&&'
"||"                  return '||'
"!"                   return '!'
<<EOF>>               return 'EOF'
.                     return 'INVALID'

/lex
 
%start expressions

%% /* language grammar */

expressions
    : e EOF
        { typeof console !== 'undefined' ? console.log($1) : print($1);
          return $1; }
    ;

e
    : e '+' e
        {$$ = '(' + $1 + ' + ' + $3 + ')';}
    | e '-' e
        {$$ = '(' + $1 + ' - ' + $3 + ')';}
    | e '*' e
        {$$ = '(' + $1 + ' * ' + $3 + ')';}
    | e '/' e
        {$$ = '(' + $1 + ' / ' + $3 + ')';}
    | e '==' e
        {$$ = '(' + $1 + ' == ' + $3 + ')';}
    | e '>' e
        {$$ = '(' + $1 + ' > ' + $3 + ')';}
    | e '<' e
        {$$ = '(' + $1 + ' < ' + $3 + ')';}
    | e '&&' e
        {$$ = '(' + $1 + ' && ' + $3 + ')';}
    | e '||' e
        {$$ = '(' + $1 + ' || ' + $3 + ')';}
    | '!' e
        {$$ = '(' + '!' + $2 + ')';}
    | '-' e
        {$$ = '(-' + $2 + ')';}
    | '(' e ')'
        {$$ = '(' + $2 + ')';}
    | NUMBER
        {$$ = Number(yytext);}
    ;

CompExp
    : CompExp '&&' MulExp
	{$$ = '(' + $1 + ' && ' + $3 + ')';}
    | CompExp '||' MulExp
	{$$ = '(' + $1 + ' || ' + $3 + ')';}
    | MulExp
	{$$ = $1;}
    ;

PlusExp
    : PlusExp '+' MulExp
	{$$ = '(' + $1 + ' + ' + $3 + ')';}
    | MulExp
	{$$ = $1}
    ;

MulExp
    : MultExp '*' RootExp
	{$$ = '(' + $1 + ' * ' + $3 + ')';}
    | RootExp 
        {$$ = $1;}
    ;

NegExp
    : e '-' RootExp 
	{$$ = '(' + $1 + ' - ' + $3 + ')';}
    | RootExp
	{$$ = $1;}
    ;

RootExp
    : '(' e ')'
	{$$ = '(' + $2 + ')';}
    | NUMBER
	{$$ = Number(yytext);}
    ;

