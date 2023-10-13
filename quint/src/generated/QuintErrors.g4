/**
 * This is a specialized grammar of Quint that is tuned for error explanation.
 * We keep QuintErrors.g4 separate from Quint.g4, as we do not want to obfuscate
 * the grammar with error handling.  It should be updated accordingly, whenever
 * Quint.g4 is updated.
 *
 * @author: Igor Konnov, Informal Systems, 2023
 */
grammar QuintErrors;

import QuintTokens;

@header {

// Used for forming errors
import { quintErrorToString } from '../quintError'

}
// entry point for the parser
modules : module+ EOF;

module : DOCCOMMENT* 'module' qualId '{' (DOCCOMMENT* declaration)* '}';

// a module declaration
declaration : 'const' constDef
            | 'var'   qualId ':' type
            | 'assume' identOrHole '=' expr
            | operDef
            | typeDef
            | 'import' importOrInstance
            | 'export' exportMod
            | ~('import') {
                const m = "[QNT000] expected one of definition, const, var, import/export, assume"
                this.notifyErrorListeners(m)
              }
           ;

constDef : qualId ':' type
         | . {
                const m = "[QNT015] expected a constant definition"
                this.notifyErrorListeners(m)
         }
         ;

// An operator definition.
// We embed two kinds of parameters right in this rule.
// Otherwise, the parser would start recognizing parameters everywhere.
operDef : qualifier normalCallName
            ( /* ML-like parameter lists */
                '(' (parameter (',' parameter)*)? ')' (':' type)?
                | ':' type
              /* C-like parameter lists */
                | '(' (parameter ':' type (',' parameter ':' type)*) ')' ':' type
            )?
            ('=' expr)? ';'?
        ;

typeDef
    : 'type' qualId                                                         # typeAbstractDef
    | 'type' qualId '=' type                                                # typeAliasDef
    | 'type' typeName=qualId '=' '|'? typeSumVariant ('|' typeSumVariant)*  # typeSumDef
    ;

// A single variant case in a sum type definition or match statement.
// E.g., `A(t)` or `A`.
typeSumVariant : sumLabel=simpleId["variant label"] ('(' type ')')? ;

nondetOperDef : 'nondet' qualId (':' type)? '=' expr ';'?;

qualifier : 'val'
          | 'def'
          | 'pure' 'val'
          | 'pure' 'def'
          | 'action'
          | 'run'
          | 'temporal'
          ;

importOrInstance
          : name '.' identOrStar ('from' fromSource)?
          | name ('as' name)? ('from' fromSource)?
          // creating an instance and importing all names introduced in the instance
          | moduleName '(' (name '=' expr (',' name '=' expr)*) ')' '.' '*'
                  ('from' fromSource)?
          // creating an instance and importing all names with a prefix
          | moduleName '(' (name '=' expr (',' name '=' expr)*) ')' 'as' qualifiedName
                  ('from' fromSource)?
          | . {
                const m = "[QNT000] expected an import or an instance"
                this.notifyErrorListeners(m)
              }
          ;

exportMod : name '.' identOrStar
          | name ('as' name)?
          ;

moduleName : qualId;
name: qualId;
qualifiedName : qualId;
fromSource: STRING;

// Types in Type System 1.2 of Apalache, which supports discriminated unions
type :          <assoc=right> type '->' type                    # typeFun
        |       <assoc=right> type '=>' type                    # typeOper
        |       '(' (type (',' type)*)? ','? ')' '=>' type      # typeOper
        |       SET '[' type ']'                                # typeSet
        |       LIST '[' type ']'                               # typeList
        |       '(' type ',' type (',' type)* ','? ')'          # typeTuple
        |       '{' row '}'                                     # typeRec
        |       typeUnionRecOne+                                # typeUnionRec
        |       'int'                                           # typeInt
        |       'str'                                           # typeStr
        |       'bool'                                          # typeBool
        |       qualId                                          # typeConstOrVar
        |       '(' type ')'                                    # typeParen
        ;

typeUnionRecOne : '|' '{' qualId ':' STRING (',' row)? ','? '}'
                ;

row : (rowLabel ':' type ',')* ((rowLabel ':' type) (',' | '|' (rowVar=IDENTIFIER))?)?
    | '|' (rowVar=IDENTIFIER)
    ;

rowLabel : simpleId["record"] ;


// A Quint expression. The order matters, it defines the priority.
// Wherever possible, we keep the same order of operators as in TLA+.
// We are also trying to be consistent with mainstream languages, e.g.,
// check the precedence table in Java:
// https://www.cs.bilkent.edu.tr/~guvenir/courses/CS101/op_precedence.html
expr:           // apply a built-in operator via the dot notation
                expr '.' nameAfterDot (LPAREN argList? RPAREN)?     # dotCall
        |       lambda                                              # lambdaCons
                // Call a user-defined operator or a built-in operator.
                // The operator has at least one argument (otherwise, it's a 'val').
        |       normalCallName '(' argList? ')'                     # operApp
                // list access via index
        |       expr '[' expr ']'                                   # listApp
                // power over integers
        |       <assoc=right> expr op='^' expr                      # pow
        |       // unary minus
                MINUS expr                                          # uminus
                // integer arithmetic
        |       expr op=(MUL | DIV | MOD) expr                      # multDiv
        |       expr op=(PLUS | MINUS) expr                         # plusMinus
                // standard relations
        |       expr op=(GT | LT | GE | LE | NE | EQ) expr          # relations
        |       qualId '\'' ASGN expr                               # asgn
        |       expr '=' expr {
                  const m = "[QNT006] unexpected '=', did you mean '=='?"
                  this.notifyErrorListeners(m)
                }                                                   # errorEq
                // Boolean operators. Note that not(e) is just a normal call
                // similar to indented /\ and indented \/ of TLA+
        |       'and' '{' expr (',' expr)* ','? '}'                 # andExpr
        |       expr AND expr                                       # and
        |       'or'  '{' expr (',' expr)* ','? '}'                 # orExpr
        |       expr OR expr                                        # or
        |       expr IFF expr                                       # iff
        |       expr IMPLIES expr                                   # implies
        |       matchSumExpr                                        # match
        |       'all' '{' expr (',' expr)* ','? '}'                 # actionAll
        |       'any' '{' expr (',' expr)* ','? '}'                 # actionAny
        |       ( qualId | INT | BOOL | STRING)                     # literalOrId
        //      a tuple constructor, the form tup(...) is just an operator call
        |       '(' expr ',' expr (',' expr)* ','? ')'              # tuple
        //      short-hand syntax for pairs, mainly designed for maps
        |       expr '->' expr                                      # pair
        |       '{' recElem (',' recElem)* ','? '}'                 # record
        //      a list constructor, the form list(...) is just an operator call
        |       '[' (expr (',' expr)*)? ','? ']'                    # list
        |       'if' '(' expr ')' expr 'else' expr                  # ifElse
        |       operDef expr                                        # letIn
        |       nondetOperDef expr                                  # nondet
        |       '(' expr ')'                                        # paren
        |       '{' expr '}'                                        # braces
        ;

// match e { A(a) => e1 | B => e2 | C(_) => e3 | ... | _ => en }
matchSumExpr: MATCH expr '{' '|'? matchCase+=matchSumCase ('|' matchCase+=matchSumCase)* '}' ;
matchSumCase: (variantMatch=matchSumVariant | wildCardMatch='_') '=>' expr ;
matchSumVariant
    : (variantLabel=simpleId["variant label"]) ('(' (variantParam=simpleId["match case parameter"] | '_') ')')? ;

// A probing rule for REPL.
// Note that a top-level declaration has priority over an expression.
// For example, see: https://github.com/informalsystems/quint/issues/394
declarationOrExpr :    declaration EOF | expr EOF | DOCCOMMENT EOF | EOF;

// This rule parses anonymous functions, e.g.:
// 1. x => e
// 2. (x) => e
// 3. (x, y, z) => e            (arity 3)
// 4. ((x, y, z)) => e          (syntax sugar: arity 1, unboxed into a 3-field tuple)
lambda          : lambdaUnsugared
                | lambdaTupleSugar ;
lambdaUnsugared : parameter '=>' expr
                | '(' parameter (',' parameter)* ')' '=>' expr
                ;
// A lambda operator over a single tuple parameter,
// unpacked into named fields
lambdaTupleSugar : '(' '(' parameter (',' parameter)+ ')' ')' '=>' expr;

// an identifier or a hole '_'
identOrHole :   '_' | qualId
        ;

parameter: identOrHole;

// an identifier or a star '*'
identOrStar :   '*' | qualId
        ;

argList :      expr (',' expr)*
        ;

recElem : simpleId["record"] ':' expr
        | '...' expr
        ;

// operators in the normal call may use a few reserved names,
// which are not recognized as identifiers.
normalCallName :   qualId
        |       op=(AND | OR | IFF | IMPLIES | SET | LIST | MAP)
        ;

// A few infix operators may be called via lhs.oper(rhs),
// without causing any ambiguity.
nameAfterDot :  qualId
        |       op=(AND | OR | IFF | IMPLIES)
        ;

// special operators
operator: (AND | OR | IFF | IMPLIES |
           GT  | LT  | GE  | LE | NE | EQ |
           MUL | DIV | MOD | PLUS | MINUS | '^')
        ;

// literals
literal: (STRING | BOOL | INT)
        ;

// A (possibly) qualified identifier, like `Foo` or `Foo::bar`
qualId : IDENTIFIER ('::' IDENTIFIER)* ;
// An unqualified identifier that raises an error if a qualId is supplied
simpleId[context: string]
    : IDENTIFIER
    | qualId {
        const err = quintErrorToString(
          { code: 'QNT008',
            message: "Identifiers in a " + $context + " cannot be qualified with '::'. Found " + $qualId.text + "."
          },
        )
        this.notifyErrorListeners(err)
      }
    ;