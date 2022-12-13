grammar Polyglot;

query: GET type_key where_clause order_by? EOF;

where_clause: (WHERE expr_or | );

type_key: '{' type=IDENTIFIER '}';

expr_or: expr_and (operator=OR_OPERATOR expr_and)*;
expr_and: expr_atom (operator=AND_OPERATOR expr_atom)*;
expr_atom: attribute_key operator=CMP_OPERATOR '?' param=IDENTIFIER | attribute_key null_check=NULL_OPERATOR  | '(' expr_or ')';

attribute_key: '{' qualifier=IDENTIFIER ('[' lang=IDENTIFIER ']')? '}';

order_key: key=attribute_key (direction=ORDER_DIRECTION)?;
order_by: ORDER_BY order_key  (',' order_key)*;

ORDER_BY: O R D E R WS+ B Y;
GET: G E T;
WHERE: W H E R E;
CMP_OPERATOR: '='|'<>'|'>'|'<'|'>='|'<=';
NULL_OPERATOR: I S (WS+ N O T)? WS+ N U L L;
AND_OPERATOR: A N D;
OR_OPERATOR: O R;
ORDER_DIRECTION: (A S C) | (D E S C);
IDENTIFIER: LETTER (LETTER|DIGIT|UNDERSCORE|'.')*;

fragment DIGIT: [0-9];
fragment LETTER: [a-zA-Z];
fragment UNDERSCORE: [_];
fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment L: [lL];
fragment N: [nN];
fragment O: [oO];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment W: [wW];
fragment Y: [yY];

WS: [ \u000B\t\r\n] -> skip;