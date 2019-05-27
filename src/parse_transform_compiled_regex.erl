%%%-------------------------------------------------------------------
%%% Part of parse_transform_compiled_regex Erlang App
%%% MIT License
%%% Copyright (c) 2019 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(parse_transform_compiled_regex).

-export([parse_transform/2]).

-define(
   AST_REMOTE_CALL(ModuleName, FunctionName),
   {remote, _CallLine, {atom, _ModuleLine, ModuleName}, {atom, _FunctionLine, FunctionName}}
  ).

%%====================================================================
%% Parse transform
%%====================================================================

parse_transform(Ast, _Opt) ->
    [parse(X) || X <- Ast].

parse({function, Line, FunctionName, ArgNum, Clauses}) ->
    {function, Line, FunctionName, ArgNum, [parse_compiled_regex_clause(Clause) || Clause <- Clauses]};
parse(X) ->
    X.

parse_compiled_regex_clause({clause, ClauseLine, Vars, Guard, Expressions}) ->
    {clause, ClauseLine, Vars, Guard, [parse_compiled_regex_expression(Expression) || Expression <- Expressions]}.

%% Compile regex:
parse_compiled_regex_expression({call, _L1, ?AST_REMOTE_CALL(re, compile), [RE | MaybeOpts]} = Expression) ->
    % re:compile/*
    try
        REData = erl_parse:normalise(RE),
        RELine = erl_anno:line(element(2, RE)),
        Options = case MaybeOpts of
                      [] -> [];
                      [Opts] -> erl_parse:normalise(Opts)
                  end,

        io:format("Parse transform (~p): Compiling regex ~p with options ~w~n", [RELine, REData, Options]),
        try
            erl_parse:abstract(erlang:apply(re, compile, [REData, Options]), [{line, RELine}])
        catch
            _:Reason ->
                io:format(
                  "Parse transform (~p): Falling back: Failed to compile regex ~p with options ~w: ~p~n",
                  [RELine, REData, Options, Reason]
                 ),
                Expression
        end
    catch
        _:_ -> Expression
    end;
parse_compiled_regex_expression({call, _, ?AST_REMOTE_CALL(re, replace), [Subject, RE, Replacement | MaybeOpts]} = Expression) ->
    case compile_regex(RE, MaybeOpts, [anchored, newline]) of
        error ->
            Expression;
        {CompiledRE, FunctionOptions} ->
            erlang:setelement(4, Expression, [Subject, CompiledRE, Replacement | FunctionOptions ])
    end;
parse_compiled_regex_expression({call, _, ?AST_REMOTE_CALL(re, Fun), [Subject, RE | MaybeOpts]} = Expression)
  when Fun == run; Fun == split ->
    case compile_regex(RE, MaybeOpts, [anchored, newline]) of
        error ->
            Expression;
        {CompiledRE, FunctionOptions} ->
            erlang:setelement(4, Expression, [Subject, CompiledRE | FunctionOptions ])
    end;
parse_compiled_regex_expression(Expression) when is_tuple(Expression) ->
    % Recursive search:
    lists:foldl(fun(Index, TupleIn) ->
                        Current = element(Index, Expression),
                        case parse_compiled_regex_expression(Current) of
                            Current ->
                                TupleIn;
                            Other ->
                                % This copies the record
                                erlang:setelement(Index, TupleIn, Other)
                        end
                end,
                Expression,
                lists:seq(2, tuple_size(Expression))
               );
parse_compiled_regex_expression(Expressions) when is_list(Expressions) ->
    [ parse_compiled_regex_expression(Expression) || Expression <- Expressions ];
parse_compiled_regex_expression(Expression) ->
    Expression.

compile_regex(RE, MaybeOpts, PassthroughOptionsKeys) ->
    case catch erl_parse:normalise(RE) of
        {'EXIT', _} ->
            error;
        Tuple when is_tuple(Tuple) ->
            error;
        REData ->
            RELine = element(2, RE),
            {CompileOptions, FunctionOptions} = case MaybeOpts of
                                                    [] ->
                                                        {[], []};
                                                    [Opts] ->
                                                        AllOptions = erl_parse:normalise(Opts),
                                                        {Lists, Rest} = proplists:split(AllOptions, re_compile_options()),
                                                        {lists:flatten(Lists), Rest}
                                                end,

            {PassthroughOptions,_} = proplists:split(CompileOptions, PassthroughOptionsKeys),
            FunctionOptionsWithPassthrough = lists:flatten(PassthroughOptions) ++ FunctionOptions,

            io:format("Parse transform (~p): Compiling regex ~p with options ~w~n", [RELine, REData, CompileOptions]),
            try
                {ok, Compiled} = erlang:apply(re, compile, [REData, CompileOptions]),
                {erl_parse:abstract(Compiled, [{line, RELine}]), [erl_parse:abstract(FunctionOptionsWithPassthrough)]}
            catch
                _:Reason ->
                    io:format(
                      "Parse transform (~p): Falling back: Failed to compile regex ~p with options ~w: ~p~n",
                      [RELine, REData, CompileOptions, Reason]
                     ),
                    error
            end
    end.

re_compile_options() -> [
                         unicode,
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
                         newline,
                         bsr_anycrlf,
                         bsr_unicode,
                         no_start_optimize,
                         ucp,
                         never_utf
                        ].

