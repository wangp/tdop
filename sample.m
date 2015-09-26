%-----------------------------------------------------------------------------%

:- module sample.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

:- import_module scanner.
:- import_module tdop.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = ["--echo" | _] ->
        loop(yes, !IO)
    ;
        loop(no, !IO)
    ).

:- pred loop(bool::in, io::di, io::uo) is det.

loop(Echo, !IO) :-
    io.write_string("> ", !IO),
    io.flush_output(!IO),
    io.read_line(ReadRes, !IO),
    (
        ReadRes = ok(Cs),
        ( scan(Cs, Tokens) ->
            (
                Echo = yes,
                foldl(io.write_char, Cs, !IO)
            ;
                Echo = no
            ),
            ( parse_expr(E, Tokens, TokensRem) ->
                io.write(E, !IO),
                io.nl(!IO),
                (
                    TokensRem = []
                ;
                    TokensRem = [_ | _],
                    io.write_string("remaining tokens: ", !IO),
                    io.write(TokensRem, !IO),
                    io.nl(!IO)
                )
            ;
                write_string("parse failed\n", !IO)
            )
        ;
            io.write_string("scan failed\n", !IO)
        ),
        io.nl(!IO),
        loop(Echo, !IO)
    ;
        ReadRes = eof,
        io.nl(!IO)
    ;
        ReadRes = error(Error),
        io.write_string("error: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
