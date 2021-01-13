%%%-------------------------------------------------------------------
%%% Part of parse_transform_compiled_regex Erlang App
%%% MIT License
%%% Copyright (c) 2019 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(parse_transform_compiled_regex).

-export([parse_transform/2]).

-define(
   AST_REMOTE_CALL(ModuleName, FunctionName),
   {remote, _CallAnno, {atom, _ModuleAnno, ModuleName}, {atom, _FunctionAnno, FunctionName}}
  ).

%%====================================================================
%% Parse transform
%%====================================================================

parse_transform(Ast, _Opt) ->
    {AllOk, CurrentModuleName} = lists:foldl(
                                   fun({attribute, _, module, ModuleName}, {true, undefined}) -> {true, ModuleName};
                                      ({attribute, _, module, ModuleName}, {true, ModuleName}) -> {true, ModuleName};
                                      ({attribute, _, module, _}, {true, _}) -> {false, undefined};
                                      (_, State) -> State
                                   end, {true, undefined}, Ast),
    case AllOk of
        true ->
            [parse(X, CurrentModuleName) || X <- Ast];
        _ ->
            io:format("Parse transform (~p): Trouble getting compiling module's name, falling back", [?MODULE]),
            Ast
    end.

parse({function, Anno, FunctionName, ArgNum, Clauses}, CurrentModuleName) ->
    {function, Anno, FunctionName, ArgNum, [parse_compiled_regex_clause(Clause, CurrentModuleName) || Clause <- Clauses]};
parse(X, _CurrentModuleName) ->
    X.

parse_compiled_regex_clause({clause, Anno, Vars, Guard, Expressions}, CurrentModuleName) ->
    {clause, Anno, Vars, Guard, [parse_compiled_regex_expression(Expression, CurrentModuleName) || Expression <- Expressions]}.

%% Compile regex:
parse_compiled_regex_expression({call, _L1, ?AST_REMOTE_CALL(re, compile), [RE | MaybeOpts]} = Expression, CurrentModuleName) ->
    % re:compile/*
    try
        REData = erl_parse:normalise(RE),
        RELine = erl_anno:line(element(2, RE)),
        Options = case MaybeOpts of
                      [] -> [];
                      [Opts] -> erl_parse:normalise(Opts)
                  end,

        io:format("Parse transform (~p:~p): Compiling regex ~p with options ~w~n", [CurrentModuleName, RELine, REData, Options]),
        try
            erl_parse:abstract(erlang:apply(re, compile, [REData, Options]), [{line, RELine}])
        catch
            _:Reason ->
                io:format(
                  "Parse transform (~p:~p): Falling back: Failed to compile regex ~p with options ~w: ~p~n",
                  [CurrentModuleName, RELine, REData, Options, Reason]
                 ),
                Expression
        end
    catch
        _:_ -> Expression
    end;
parse_compiled_regex_expression({call, _, ?AST_REMOTE_CALL(re, replace), [Subject, RE, Replacement | MaybeOpts]} = Expression, CurrentModuleName) ->
    case compile_regex(RE, MaybeOpts, [anchored, newline], CurrentModuleName) of
        error ->
            Expression;
        {CompiledRE, FunctionOptions} ->
            erlang:setelement(4, Expression, [Subject, CompiledRE, Replacement | FunctionOptions ])
    end;
parse_compiled_regex_expression({call, _, ?AST_REMOTE_CALL(re, Fun), [Subject, RE | MaybeOpts]} = Expression, CurrentModuleName)
  when Fun == run; Fun == split ->
    case compile_regex(RE, MaybeOpts, [anchored, newline], CurrentModuleName) of
        error ->
            Expression;
        {CompiledRE, FunctionOptions} ->
            erlang:setelement(4, Expression, [Subject, CompiledRE | FunctionOptions ])
    end;
parse_compiled_regex_expression(Expression, CurrentModuleName) when is_tuple(Expression) ->
    % Recursive search:
    lists:foldl(fun(Index, TupleIn) ->
                        Current = element(Index, Expression),
                        case parse_compiled_regex_expression(Current, CurrentModuleName) of
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
parse_compiled_regex_expression(Expressions, CurrentModuleName) when is_list(Expressions) ->
    [ parse_compiled_regex_expression(Expression, CurrentModuleName) || Expression <- Expressions ];
parse_compiled_regex_expression(Expression, _CurrentModuleName) ->
    Expression.

compile_regex(RE, MaybeOpts, PassthroughOptionsKeys, CurrentModuleName) ->
    try
        REData = erl_parse:normalise(RE),
        false = is_tuple(REData),
        RELine = erl_anno:line(element(2, RE)),
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

        io:format("Parse transform (~p:~p): Compiling regex ~p with options ~w. Runtime options are ~w~n", [CurrentModuleName, RELine, REData, CompileOptions, FunctionOptionsWithPassthrough]),
        try
            {ok, Compiled} = erlang:apply(re, compile, [REData, CompileOptions]),
            {erl_parse:abstract(Compiled, [{line, RELine}]), [erl_parse:abstract(FunctionOptionsWithPassthrough)]}
        catch
            _:Reason ->
                io:format(
                  "Parse transform (~p:~p): Falling back: Failed to compile regex ~p with options ~w: ~p~n",
                  [CurrentModuleName, RELine, REData, CompileOptions, Reason]
                  ),
                error
        end
    catch
        _:_ -> error
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

