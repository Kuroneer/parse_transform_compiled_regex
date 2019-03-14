% Part of parse_transform_compiled_regex Erlang App
% MIT License
% Copyright (c) 2019 Jose Maria Perez Ramos

-module(parse_transform_compiled_regex_SUITE).
-compile(export_all).
-compile({parse_transform, parse_transform_compiled_regex}).
-include_lib("common_test/include/ct.hrl").

-define(HTTP_REGEX, "(http://|https://)[a-z0-9]+\\.[a-z0-9]+").

all() -> [
          compile,
          replace,
          run,
          split,
          timed
         ].


suite() ->
    [{timetrap, {seconds, 30}}].


init_per_suite(Config) ->
    Config.


end_per_suite(_) -> ok.


init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(_Case, _Config) ->
    ok.


%% =============================================================================
%% Test cases
%% =============================================================================

compile(_Config) ->
    HttpRegex = ?HTTP_REGEX,
    Compiled1 = re:compile(HttpRegex), %% Not transformed
    Compiled1 = re:compile(?HTTP_REGEX), %% Parse transformed
    _ = re:compile([$a | ?HTTP_REGEX]), %% Parse transformed
    F = fun() -> ?HTTP_REGEX end,
    Compiled1 = re:compile(F()), %% Not transformed
    {ok, _} = Compiled1,

    CompileOptions = [
                      anchored,
                      caseless,
                      dollar_endonly,
                      dotall,
                      extended,
                      firstline,
                      multiline,
                      no_auto_capture,
                      dupnames,
                      ungreedy,
                      {newline, any},
                      bsr_unicode,
                      no_start_optimize,
                      ucp,
                      never_utf
                     ],

    Compiled2 = re:compile(HttpRegex, CompileOptions),
    Compiled2 = re:compile(?HTTP_REGEX, [ %% Parse transformed
                                         anchored,
                                         caseless,
                                         dollar_endonly,
                                         dotall,
                                         extended,
                                         firstline,
                                         multiline,
                                         no_auto_capture,
                                         dupnames,
                                         ungreedy,
                                         {newline, any},
                                         bsr_unicode,
                                         no_start_optimize,
                                         ucp,
                                         never_utf
                                        ]),
    {ok, _} = Compiled2,
    {'EXIT', {badarg, _}} = (catch re:compile(?HTTP_REGEX, [patata])),
    ok.


replace(_Config) ->
    Sentence = "TOMATE: The tomate strikes back.",
    Replacement = "tomato",
    Tomate = "tomate",
    CompiledResult = re:replace(Sentence, Tomate, Replacement, [caseless, anchored, {return, binary}, {match_limit, 2}]),
    PrecompiledResult = re:replace(Sentence, "tomate", Replacement, [caseless, anchored, {return, binary}, {match_limit, 2}]),
    PrecompiledResult = CompiledResult,
    <<"tomato: The tomate strikes back.">> = CompiledResult,
    ok.


run(_Config) ->
    Sentence = "aaaaaaaaaaaz",
    Pattern = "(A*)z",
    CompiledResult = re:run(Sentence,  Pattern, [caseless, global]),
    PrecompiledResult = re:run(Sentence, "(A*)z", [caseless, global]),
    CompiledResult = PrecompiledResult,
    {match,[[{0,12},{0,11}]]} = CompiledResult,
    ok.

split(_Config) ->
    Sentence = "aaaaaaaaaaaz",
    Pattern = "(A*)z",
    CompiledResult = re:split(Sentence,  Pattern, [caseless, {return, binary}]),
    PrecompiledResult = re:split(Sentence, "(A*)z", [caseless, {return, binary}]),
    CompiledResult = PrecompiledResult,
    [<<>>,<<"aaaaaaaaaaa">>,<<>>] = CompiledResult,
    ok.


timed(_Config) ->
    %% Yes, this test is not a proper test, as it relies on the implementation,
    %% it's here just as a commodity
    HttpRegex = ?HTTP_REGEX,
    Iterations = 10000,
    Subject = "http://erlang.org/doc/man/re.html#split-2",

    PreCompiledRunFun = fun() -> match = re:run(Subject, ?HTTP_REGEX, [{capture, none}]) end,
     ReCompiledRunFun = fun() -> match = re:run(Subject,   HttpRegex, [{capture, none}]) end,

    {TimePrecompiled0, ok} = timer:tc(?MODULE, repeat, [PreCompiledRunFun, Iterations]),
    {TimeReCompiled0 , ok} = timer:tc(?MODULE, repeat, [ ReCompiledRunFun, Iterations]),
    ct:pal("re:run time:~nPrecompiled (~p iterations): ~p~nRepeated compile (~p Iterations): ~p",
           [Iterations, TimePrecompiled0, Iterations, TimeReCompiled0]
          ),
    true = TimeReCompiled0 > TimePrecompiled0,

    PreCompiledCompileFun = fun() -> re:compile(?HTTP_REGEX) end,
     ReCompiledCompileFun = fun() -> re:compile(  HttpRegex) end,

    {TimePrecompiled1, ok} = timer:tc(?MODULE, repeat, [PreCompiledCompileFun, Iterations]),
    {TimeReCompiled1 , ok} = timer:tc(?MODULE, repeat, [ ReCompiledCompileFun, Iterations]),
    ct:pal("re:compile time:~nPrecompiled (~p iterations): ~p~nRepeated compile (~p Iterations): ~p",
           [Iterations, TimePrecompiled1, Iterations, TimeReCompiled1]
          ),
    true = TimeReCompiled1 > TimePrecompiled1,
    % throw(error), %% Uncomment to see results in terminal
    ok.


%% =============================================================================
%% Internal functions
%% =============================================================================

repeat(_Fun, 0) ->
    ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

