%-----------------------------------------------------------------------------%

:- module tdop.
:- interface.

:- import_module list.

:- type ps == list(token).

:- type token
    --->    name(name)
    ;       integer(string)
    ;       lparen
    ;       rparen
    ;       comma
    ;       equals
    ;       plus
    ;       minus
    ;       star
    ;       slash
    ;       caret
    ;       bang
    ;       question
    ;       colon
    ;       lt
    ;       gt.

:- type name == string.

:- type expr
    --->    name(name)
    ;       integer(string)
    ;       assign(name, expr)
    ;       cond(expr, expr, expr)
    ;       prefix(prefixop, expr)
    ;       postfix(expr, postfixop)
    ;       infix(expr, infixop, expr)
    ;       call(expr, list(expr)).

:- type prefixop
    --->    plus
    ;       minus
    ;       bang.

:- type postfixop
    --->    bang.

:- type infixop
    --->    plus
    ;       minus
    ;       star
    ;       slash
    ;       caret
    ;       lt
    ;       gt.

:- pred parse_expr(expr::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

:- type expr_prec == {expr, prec}.

:- type prec == int.

:- func prec_min = prec.
:- func prec_assign = prec.
:- func prec_cond = prec.
:- func prec_compare = prec.
:- func prec_sum = prec.
:- func prec_product = prec.
:- func prec_prefix = prec.
:- func prec_exponent = prec.
:- func prec_postfix = prec.
:- func prec_call = prec.

prec_min = 0.       % lowest (non-binding operators)
prec_assign = 10.
prec_cond = 20.
prec_compare = 25.
prec_sum = 30.
prec_product = 40.
prec_prefix = 50.
prec_exponent = 60.
prec_postfix = 70.
prec_call = 80.

:- func lower(prec) = prec.

lower(P) = P-1.

%-----------------------------------------------------------------------------%

:- pred consume(token::out, ps::in, ps::out) is semidet.

consume(Token, [Token | PS], PS).

%-----------------------------------------------------------------------------%

parse_expr(E, !PS) :-
    expr(prec_min, E, !PS).

:- pred expr(prec::in, expr::out, ps::in, ps::out) is semidet.

expr(Prec, E, !PS) :-
    consume(Token, !PS),
    prefix_parser(Token, Parser, PrefixPrec),
    Parser(Token, E0, !PS),
    expr_loop(Prec, {E0, PrefixPrec}, {E, _}, !PS).

:- pred expr_loop(prec::in, expr_prec::in, expr_prec::out, ps::in, ps::out)
    is semidet.

expr_loop(Prec, E0, E, !PS) :-
    (
        consume(Token, !PS),
        infix_parser(Token, Parser, PrecR),
        Prec < PrecR
    ->
        Parser(E0, Token, PrecR, E1, !PS),
        expr_loop(Prec, {E1, PrecR}, E, !PS)
    ;
        E = E0
    ).

%-----------------------------------------------------------------------------%

% "nud" (null denotation) - values and prefix operators

:- type prefix_parser == pred(token, expr, ps, ps).
:- inst prefix_parser == (pred(in, out, in, out) is semidet).

:- pred prefix_parser(token, prefix_parser, prec).
:- mode prefix_parser(in, out(prefix_parser), out) is semidet.

prefix_parser(Token, Parser, Prec) :-
    (
        Token = name(_),
        Parser = name,
        Prec = prec_min
    ;
        Token = integer(_),
        Parser = integer,
        Prec = prec_min
    ;
        Token = lparen,
        Parser = group,
        Prec = prec_min
    ;
        Token = plus,
        Parser = prefixop(plus),
        Prec = prec_prefix
    ;
        Token = minus,
        Parser = prefixop(minus),
        Prec = prec_prefix
    ;
        Token = bang,
        Parser = prefixop(bang),
        Prec = prec_prefix
    ).

:- pred name `with_type` prefix_parser.
:- mode name `with_inst` prefix_parser.

name(name(N), name(N), !PS).

:- pred integer `with_type` prefix_parser.
:- mode integer `with_inst` prefix_parser.

integer(integer(N), integer(N), !PS).

:- pred group `with_type` prefix_parser.
:- mode group `with_inst` prefix_parser.

group(lparen, E, !PS) :-
    parse_expr(E, !PS),
    consume(rparen, !PS).

:- pred prefixop(prefixop) `with_type` prefix_parser.
:- mode prefixop(in) `with_inst` prefix_parser.

prefixop(Op, _Token, E, !PS) :-
    expr(prec_prefix, E0, !PS),
    E = prefix(Op, E0).

%-----------------------------------------------------------------------------%

% "led" (left denotation) - infix and suffix operators

:- type infix_parser == pred(expr_prec, token, prec, expr, ps, ps).
:- inst infix_parser == (pred(in, in, in, out, in, out) is semidet).

:- pred infix_parser(token, infix_parser, prec).
:- mode infix_parser(in, out(infix_parser), out) is semidet.

infix_parser(Token, Parser, Prec) :-
    (
        Token = equals,
        Parser = assign,
        Prec = prec_assign
    ;
        Token = question,
        Parser = cond,
        Prec = prec_cond
    ;
        Token = lt,
        Parser = infixnon(lt),
        Prec = prec_compare
    ;
        Token = gt,
        Parser = infixnon(gt),
        Prec = prec_compare
    ;
        Token = plus,
        Parser = infixl(plus),
        Prec = prec_sum
    ;
        Token = minus,
        Parser = infixl(minus),
        Prec = prec_sum
    ;
        Token = star,
        Parser = infixl(star),
        Prec = prec_product
    ;
        Token = slash,
        Parser = infixl(slash),
        Prec = prec_product
    ;
        Token = caret,
        Parser = infixr(caret),
        Prec = prec_exponent
    ;
        Token = bang,
        Parser = postfix(bang),
        Prec = prec_postfix
    ;
        Token = lparen,
        Parser = call,
        Prec = prec_call
    ).

:- pred assign `with_type` infix_parser.
:- mode assign `with_inst` infix_parser.

assign({E0, _}, _Token, Prec, E, !PS) :-
    E0 = name(Name),
    expr(lower(Prec), E1, !PS),
    E = assign(Name, E1).

:- pred cond `with_type` infix_parser.
:- mode cond `with_inst` infix_parser.

cond({E0, _}, _Token, _Prec, E, !PS) :-
    parse_expr(E1, !PS),
    consume(colon, !PS),
    parse_expr(E2, !PS),
    E = cond(E0, E1, E2).

:- pred infixl(infixop) `with_type` infix_parser.
:- mode infixl(in) `with_inst` infix_parser.

infixl(Op, {E0, _}, _Token, Prec, E, !PS) :-
    expr(Prec, E1, !PS),
    E = infix(E0, Op, E1).

:- pred infixr(infixop) `with_type` infix_parser.
:- mode infixr(in) `with_inst` infix_parser.

infixr(Op, {E0, _}, _Token, Prec, E, !PS) :-
    expr(lower(Prec), E1, !PS),
    E = infix(E0, Op, E1).

:- pred infixnon(infixop) `with_type` infix_parser.
:- mode infixnon(in) `with_inst` infix_parser.

infixnon(Op, {E0, Prec0}, _Token, Prec, E, !PS) :-
    % Precedence of left expr must not be the same as the operator.
    Prec0 \= Prec,
    expr(Prec, E1, !PS),
    E = infix(E0, Op, E1).

:- pred postfix(postfixop) `with_type` infix_parser.
:- mode postfix(in) `with_inst` infix_parser.

postfix(Op, {E0, _}, _Token, _Prec, E, !PS) :-
    E = postfix(E0, Op),
    semidet_true.

:- pred call `with_type` infix_parser.
:- mode call `with_inst` infix_parser.

call({E0, _}, _Token, _Prec, E, !PS) :-
    ( consume(rparen, !PS) ->
        Args = []
    ;
        args1(Args, !PS),
        consume(rparen, !PS)
    ),
    E = call(E0, Args).

:- pred args1(list(expr), ps, ps).
:- mode args1(out, in, out) is semidet.

args1(Args, !PS) :-
    parse_expr(E, !PS),
    ( consume(comma, !PS) ->
        args1(Es, !PS),
        Args = [E | Es]
    ;
        Args = [E]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
