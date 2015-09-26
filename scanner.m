%-----------------------------------------------------------------------------%

:- module scanner.
:- interface.

:- import_module char.
:- import_module list.

:- import_module tdop.

:- pred scan(list(char)::in, list(token)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

scan(Cs, Tokens) :-
    scan(Cs, [], RevTokens),
    reverse(RevTokens, Tokens).

:- pred scan(list(char)::in, list(token)::in, list(token)::out) is semidet.

scan([], !Acc).
scan([C | Cs0], !Acc) :-
    ( is_whitespace(C) ->
        scan(Cs0, !Acc)
    ; op(C, Token) ->
        cons(Token, !Acc),
        scan(Cs0, !Acc)
    ; name(C, Cs0, Token, Cs) ->
        cons(Token, !Acc),
        scan(Cs, !Acc)
    ; integer(C, Cs0, Token, Cs) ->
        cons(Token, !Acc),
        scan(Cs, !Acc)
    ;
        fail
    ).

:- pred op(char::in, token::out) is semidet.

op('(', lparen).
op(')', rparen).
op(',', comma).
op('=', equals).
op('+', plus).
op('-', minus).
op('*', star).
op('/', slash).
op('^', caret).
op('!', bang).
op('?', question).
op(':', colon).

:- pred name(char::in, list(char)::in, token::out, list(char)::out) is semidet.

name(C, Cs0, name(Name), Cs) :-
    is_alpha(C),
    takewhile(is_alnum, Cs0, NameCs, Cs),
    from_char_list([C | NameCs], Name).

:- pred integer(char::in, list(char)::in, token::out, list(char)::out)
    is semidet.

integer(C, Cs0, integer(Integer), Cs) :-
    is_decimal_digit(C),
    takewhile(is_decimal_digit, Cs0, IntCs, Cs),
    from_char_list([C | IntCs], Integer).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
