%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2012 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_debug).

-export([start_link/1, eval_cmd/4, cmd_attach/3, check_break/2, markdown/0]).

-include("lux.hrl").
-include_lib("kernel/include/file.hrl").

-type debug_type() :: 'integer' |
                      {'integer', integer(), integer() | infinity} |
                      'string' |
                      'lineno'.
-record(debug_param,
        {name     :: string(),
         type     :: debug_type(),
         presence :: [mandatory | optional],
         help     :: string()}).

-type debug_fun() :: fun((#istate{}, [term()], term()) -> {term(), #istate{}}).

-record(debug_cmd,
        {name     :: string(),
         params   :: [debug_type()],
         help     :: string(),
         callback :: debug_fun()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read commands from stdin and communicate with interpreter

start_link(I) ->
    Parent = self(),
    spawn_link(fun() -> init(I, Parent) end).

init(I, IntPid) ->
    CmdState = undefined,
    NewCmdState =
        case I#istate.debug_file of
            undefined ->
                CmdState;
            DebugFile ->
                LoadCmd = "load " ++ DebugFile,
                call(IntPid, LoadCmd, CmdState)
        end,
    loop(I, IntPid, "help", NewCmdState, 1).

loop(I, IntPid, PrevCmd, CmdState, N) ->
    case io:get_line("") of
        eof when N =:= 1 ->
            %% Closed already at startup
            exit(normal);
        eof ->
            catch io:format("\nEOF: stdin closed\n", []),
            exit(normal);
        {error, Reason} ->
            ReasonStr = file:format_error(Reason),
            catch io:format("\nERROR: ~s\n", [ReasonStr]),
            exit(Reason);
        Cmd0 ->
            [$\n | Rest] = lists:reverse(Cmd0),
            Cmd = lists:reverse(Rest),
            case string:tokens(Cmd, " ") of
                [] when Cmd =:= "" ->
                    %% Repeat previous command
                    NewCmdState = call(IntPid, PrevCmd, CmdState),
                    loop(I, IntPid, PrevCmd, NewCmdState, N+1);
                [] ->
                    %% Ignore empty command
                    loop(I, IntPid, Cmd, CmdState, N+1);
                _ ->
                    %% Execute new command
                    NewCmdState = call(IntPid, Cmd, CmdState),
                    loop(I, IntPid, Cmd, NewCmdState, N+1)
            end
    end.

call(IntPid, CmdStr, CmdState) ->
    %% io:format("DEBUG: ~p\n", [CmdStr]),
    IntPid ! {debug_call,self(),CmdStr, CmdState},
    receive
        {debug_reply,IntPid,NewCmdState} -> NewCmdState
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse and evaluate one command

eval_cmd(I, ReplyTo, CmdStr, CmdState) ->
    {CmdState2,I2} = do_eval_cmd(I, CmdStr, CmdState),
    ReplyTo ! {debug_reply,self(),CmdState2},
    I2.

do_eval_cmd(I, CmdStr, CmdState) ->
    [CmdName | Args] = string:tokens(CmdStr, " "),
    case select(CmdName) of
        {ok, #debug_cmd{name=_Name, params=Params, callback=Fun}} ->
            case parse_params(I, Params, Args, [], []) of
                {ok,Args2} ->
                    %% io:format("Eval: ~s ~p\n", [_Name, Args2]),
                    Fun(I, Args2, CmdState);
                {error,ReasonStr} ->
                    io:format("\nERROR: ~s: ~s\n", [CmdName, ReasonStr]),
                    {CmdState,I}
            end;
        {error,ReasonStr} ->
            io:format("\nERROR: ~s", [ReasonStr]),
            {CmdState,I}
    end.

select(CmdStr) ->
    NamedCmds = [{C#debug_cmd.name,C} || C <- cmds()],
    do_select(CmdStr, NamedCmds, CmdStr, NamedCmds).

do_select([H | T], NamedCmds, Orig, Prev) ->
    case select_first(H, NamedCmds, []) of
        [] ->
            {error, ambiguous(Orig, Prev)};
        [{_,Cmd}] ->
            case lists:prefix(Orig, Cmd#debug_cmd.name) of
                true ->
                    {ok,Cmd};
                false ->
                    {error,ambiguous(Orig, Prev)}
            end;
        Ambiguous ->
            do_select(T, Ambiguous, Orig, NamedCmds)
    end;
do_select([], [{_,Cmd}], _Orig, _Prev) ->
    {ok,Cmd};
do_select([], [], Orig, Prev) ->
    {error,ambiguous(Orig, Prev)};
do_select([], Ambiguous, Orig, _Prev) ->
    {error,ambiguous(Orig, Ambiguous)}.

select_first(Char, [{[Char | Chars], Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, [{Chars, Cmd} | Acc]);
select_first(Char, [{_Chars, _Cmd} | NamedCmds], Acc) ->
    select_first(Char, NamedCmds, Acc);
select_first(_Char, [], Acc) ->
    lists:reverse(Acc).

ambiguous(Orig, NamedCmds) ->
    Longest = longest(NamedCmds),
    Fun = fun({_, #debug_cmd{name = Name, help = Help}}) ->
                  {Slogan, _} =
                      lists:splitwith(fun(Char) -> Char =/= $\n end, Help),
                  ["* ", string:left(Name, Longest, $\ ), " - ", Slogan, "\n"]
          end,
    DeepList = lists:map(Fun, NamedCmds),
    lists:flatten(["Available commands: ", Orig, "\n",
                   "-------------------\n", DeepList]).

parse_params(I, [#debug_param{name=Name,type=Type} | Params],
             [Arg | Args], Acc, _Bad) ->

    try
        Val = parse_param(I, Type, Arg),
        parse_params(I, Params, Args, [{Name,Val} | Acc], [])
    catch
        error:_ ->
            Type2 =
                if
                    is_atom(Type) -> atom_to_list(Type);
                    true          -> atom_to_list(element(1, Type))
                end,
            ReasonStr = lists:flatten(["Bad type of parameter ", Name,
                                       ". Expected ", Type2, "\n"]),
            {error, ReasonStr}
    end;
parse_params(_I, Params, [], Acc, Bad) ->
    case [P || P <- Params, P#debug_param.presence =/= optional] of
        [] ->
            {ok, lists:reverse(Acc)};
        Mandatory ->
            All = Bad ++ Mandatory,
            Longest = longest(All),
            DeepList = ["Missing parameter.\nPossible parameters:\n",
                        [pretty_param(P, Longest) || P <- All]],
            {error, lists:flatten(DeepList)}
    end;
parse_params(_I, [], [Arg | _], _Acc, Bad) ->
    Longest = longest(Bad),
    DeepList = ["Value ", Arg, " has the wrong type.\nPossible parameters:\n",
                [pretty_param(P, Longest) || P <- Bad]],
    {error,lists:flatten(DeepList)}.

parse_param(I, Type, Val) ->
    case Type of
        lineno when is_list(Val) ->
            RevFile =
                case lists:splitwith(fun(Char) -> Char =/= $@ end, Val) of
                    {Line,[]} ->
                        I#istate.orig_rev_file;
                    {[],[$@|Line]} ->
                        I#istate.orig_rev_file;
                    {File,[$@|Line]} ->
                        OrigDir = filename:dirname(I#istate.orig_file),
                        lux_utils:filename_split(OrigDir, File)
                end,
            ParseInt =
                fun(Str) -> parse_param(I, {integer,1,infinity}, Str) end,
            ParseBreakPos =
                fun(L) -> lists:map(ParseInt, string:tokens(L, ":")) end,
            #lineno{rev_file=RevFile,
                    rev_pos=lists:reverse(ParseBreakPos(Line))};
        string when is_list(Val) ->
            Val;
        {enum, List} when is_list(List) ->
            [Single] = [Elem || Elem <- List, lists:prefix(Val, Elem)],
            Single;
        binary ->
            list_to_binary(Val);
        atom ->
            list_to_atom(Val);
        existing_atom ->
            list_to_existing_atom(Val);
        integer ->
            list_to_integer(Val);
        {integer,_Min,infinity} when Val =:= "infinity" ->
            999999;
        {integer,Min,infinity} ->
            Int = list_to_integer(Val),
            if
                Int >= Min -> Int
            end;
        {integer,Min,Max} ->
            Int = list_to_integer(Val),
            if
                Int >= Min, Int =< Max -> Int
            end;
        float ->
            list_to_float(Val);
        {float,_Min,infinity} when Val =:= "infinity" ->
            999999.0;
        {float,Min,infinity} ->
            Float = list_to_float(Val),
            if
                Float >= Min -> Float
            end;
        {float,Min,Max} ->
            Float = list_to_float(Val),
            if
                Float >= Min, Float =< Max -> Float
            end;
        boolean when Val =:= true; Val =:= "true" ->
            true;
        boolean when Val =:= false; Val =:= "false" ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Specification of all available debugger commands

cmds() ->
    [
     #debug_cmd{name="attach",
                params=[],
                help="attach to script and pause its execution",
                callback=fun cmd_attach/3},
     #debug_cmd{name="break",
                params=[#debug_param{name="lineno",
                                     type=lineno,
                                     presence=optional,
                                     help="lineno in source file"},
                        #debug_param{name="duration",
                                     type={enum, ["normal",
                                                  "temporary",
                                                  "delete"]},
                                     presence=optional,
                                     help="controls the duration of "
                                     "the breakpoint"}],
                help="set, delete and list breakpoints\n\n"
                "When a breakpoint is set it may either be normal (default)\n"
                "or temporary. The difference between them is that normal\n"
                "breakpoints remains after break while temporary breakpoints\n"
                "are automatically deleted when they have been used once.\n\n"
                "delete means that the breakpoint immediately is removed\n"
                "Without parameters, all breakpoints are listed.\n",
                callback=fun cmd_break/3},
     #debug_cmd{name="continue",
                params=[#debug_param{name="lineno",
                                     type=lineno,
                                     presence=optional,
                                     help="run to temporary breakpoint"
                                     " at lineno"}],
                help="continue script execution",
                callback=fun cmd_continue/3},
     #debug_cmd{name="help",
                params=[#debug_param{name="command",
                                     type=string,
                                     presence=optional,
                                     help="debugger command"}],
                help="display description of a command",
                callback=fun cmd_help/3},
     #debug_cmd{name="tail",
                params=[#debug_param{name="index",
                                     type={integer, 1, infinity},
                                     presence=optional,
                                     help="log number"},
                        #debug_param{name="format",
                                     type={enum, ["compact", "verbose"]},
                                     presence=optional,
                                     help="display format"},
                        #debug_param{name="n_lines",
                                     type={integer, 1, infinity},
                                     presence=optional,
                                     help="fixed number of lines"}],

                help="display log files\n\n"
                "With no argument, the names of the log files will be listed.\n"
                "Each one is preceeded by its index number and optionally a\n"
                "star. Star means that the log has been updated since the\n"
                "previous status check. Use the index to display a particular\n"
                "log. Such as \"t 2\" for the event log. Press enter to\n"
                "display more lines. n_lines can be used to override that\n"
                "behavior andonly display a fixed number of lines regardless\n"
                "of the command is repeated or not.",
                callback=fun cmd_tail/3},
     #debug_cmd{name="list",
                params=[#debug_param{name="depth",
                                     type={enum, ["local", "include", "macro"]},
                                     presence=optional,
                                     help="depth of listing.\n"
                                     "local < include < macro"},
                        #debug_param{name="n_lines",
                                     type={integer, 1, infinity},
                                     presence=optional,
                                     help="number of lines"},
                        #debug_param{name="lineno",
                                     type=lineno,
                                     presence=optional,
                                     help="start listing at lineno"}],
                help="list script source\n\n"
                "If no \"lineno\" is given, the listing will start from the\n"
                "current line or from the latest given \"lineno\" if no other\n"
                "commands have been given in between. \"depth\" controls the\n"
                "depth of the listing. \"local\" only provides listing of the\n"
                "local file. \"include\" does also include commands in\n"
                "include files. \"macro\" provides the full listing,\n"
                "including commands in invoked macros and commands in\n"
                "include files. \"macro\" is default.",
                callback=fun cmd_list/3},
     #debug_cmd{name="load",
                params=[#debug_param{name="file",
                                     type=string,
                                     presence=optional,
                                     help="file name. Default is "
                                     "\"lux.debug\"."}],
                help="load file with debug commands",
                callback=fun cmd_load/3},
     #debug_cmd{name="next",
                params=[#debug_param{name="n_commands",
                                     type={integer, 1, infinity},
                                     presence=optional,
                                     help="number of commands"}],
                help="execute one or more commands. "
                "A multiline command counts as one command.",
                callback=fun cmd_next/3},
     #debug_cmd{name="progress",
                params=[#debug_param{name="level",
                                     type={enum, ["silent", "brief", "doc",
                                                  "compact", "verbose"]},
                                     presence=optional,
                                     help="verbosity level. "
                                     "Toggles between brief and "
                                     "verbose by default."}],
                help="set verbosity level of progress info",
                callback=fun cmd_progress/3},
     #debug_cmd{name="quit",
                params=[],
                help="exit lux in a controlled manner. "
                "Runs cleanup if applicable.",
                callback=fun cmd_quit/3},
     #debug_cmd{name="save",
                params=[#debug_param{name="file",
                                     type=string,
                                     presence=optional,
                                     help="file name. Default is "
                                     "\"lux.debug\"."}],
                help="save debug state to file",
                callback=fun cmd_save/3},
     #debug_cmd{name="skip",
                params=[#debug_param{name="n_commands",
                                     type={integer, 1, infinity},
                                     presence=optional,
                                     help="number of commands"}],
                help="skip execution of one or more commands. "
                "A multiline command counts as one command.",
                callback=fun cmd_skip/3}
    ].

help_intro() ->
    ""
    "Debugger for Lux scripts\n"
    "========================\n"
    "When `lux` is started with the `--debug` option, the debugger\n"
    "will attach to the script before its execution has started. An\n"
    "optional file with saved commands may be processed at this stage.\n"
    "The debugger can also be attached to the script in the middle of\n"
    "the execution by entering the command \"attach\" (or an abbreviation\n"
    "of the command) and pressing the enter key.\n"
    "\n"
    "Several parameters has a lineno as parameter see `help lineno`.\n"
    "\n"
    "Blank command lines implies that the previous command is repeated.\n"
    "If no command has been entered yet, the command `help` is assumed.\n"
    "\n"
    "Commands may be abbreviated. Use the help command (for example\n"
    "`help help` to get more detailed descriptions of the commands.\n\n".

help_lineno() ->
    "\n"
    "lineno parameter\n"
    "----------------\n"
    "Several commands has a lineno as parameter. It is a string which\n"
    "starts with an optional file component and is followed by a\n"
    "mandatory sequence of numbers. The file is separated from the\n"
    "numbers by an at-sign (file@) and the numbers are separated from\n"
    "each others by a colon (3:12:6). If the file component is omitted\n"
    "the main script file is assumed.\n"
    "\n"
    "Assume the following files:\n"
    "\n"
    "  --- main.lux ---\n"
    "    1: [doc Example of include files and macros]\n"
    "    2:\n"
    "    3:     [include outer.luxinc]\n"
    "    4:\n"
    "    5: [shell demo]\n"
    "    6:     [invoke first]\n"
    "\n"
    "  --- outer.luxinc ---\n"
    "    1: [include inner.luxinc]\n"
    "\n"
    "  --- inner.luxinc ---\n"
    "    1: [macro first]\n"
    "    2:     [invoke second]\n"
    "    3: [endmacro]\n"
    "    4:\n"
    "    5: [macro second]\n"
    "    6:     !echo hello\n"
    "    7: [endmacro]\n"
    "    8:\n"
    "    9: [shell demo]\n"
    "   10:     !echo world\n"
    "   11:     ?echo world\n"
    "   12:     ?world\n"
    "\n"
    "Here are a few examples of how lineno can be used:\n"
    "\n"
    "main.lux@3  - line 3 in file main.lux\n"
    "3           - same as main.lux@3\n"
    "inner@9     - line 9 in file inner.lux\n"
    "inner@6     - line 6 in (the second macro in) file inner.lux\n"
    "main@3:2:6  - on line 4 main.lux invokes the first macro, which\n"
    "              on line 2 invokes the second macro which has a line 6\n"
    "3:2:6       - same as main.lux@3:2:6\n"
    "main@1:1:10 - on line 1 main.lux includes outer.luxinc, which\n"
    "              on line 1 includes inner.lux which has a line 10\n"
    "1:1:10      - same as main.lux@1:1:10\n".

markdown() ->
    Intro = help_intro(),
    {error,Ambiguous} = select(""),
    LineNo = help_lineno(),
    Cmds = lists:flatten([["\n", pretty_cmd(Cmd)] || Cmd <- cmds()]),
    io:format("~s\n", [Intro]),
    io:format("~s", [Ambiguous]),
    io:format("~s\n", [LineNo]),
    io:format("~s\n", [Cmds]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_attach(I, _, CmdState) ->
    Current = current_call_stack(I),
    ListOpts =
        case CmdState of
            {attach,next} ->
                LineNo = lux_utils:call_stack_to_lineno(Current),
                [{"n_lines",1}, {"lineno",LineNo}];
            _ ->
                io:format("\nBreak at ~s\n",
                          [lux_utils:pretty_lineno(I, Current)]),
                [{RevFile,Pos} | ParentCallStack] = Current,
                Pos2 =
                    if
                        Pos > 3 ->
                            %% Display 2 extra lines before actual lineno
                            TmpPos = Pos-2,
                            case callers(I, RevFile, TmpPos, macro) of
                                []       -> Pos; % No cmd at that line
                                _Callers -> TmpPos
                            end;
                        true->
                            Pos
                    end,
                CallStack = [{RevFile,Pos2} | ParentCallStack],
                LineNo2 = lux_utils:call_stack_to_lineno(CallStack),
                [{"n_lines",10}, {"lineno",LineNo2}]
        end,
    {Blocked,I2} = opt_block(I),
    case Blocked of
        false ->
            {undefined,I2};
        true ->
            io:format("\n",[]),
            cmd_list(I2, ListOpts, CmdState)
    end.

opt_block(I) ->
    if
        I#istate.blocked ->
            {false,I};
        true ->
            lists:foreach(fun(#shell{pid=Pid}) -> Pid ! {block,self()} end,
                          I#istate.shells),
            {true,
             I#istate{blocked=true,
                      has_been_blocked=true,
                      want_more=false,
                      old_want_more=I#istate.want_more}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_break(I, Args, _CmdState) ->
    {_Res, I2} =
        case Args of
            [{"lineno", LineNo}, {"duration", Duration}] ->
                case Duration of
                    "temporary"  ->
                        add_break(I, LineNo, temporary);
                    "next"  ->
                        add_break(I, LineNo, next);
                    "normal"  ->
                        add_break(I, LineNo, enabled);
                    "delete"  ->
                        Breaks = I#istate.breakpoints,
                        PrettyLineNo = lux_utils:pretty_lineno(I, LineNo),
                        case lists:keyfind(LineNo, #break.lineno, Breaks) of
                            false ->
                                io:format("\nNo breakpoint at: ~s\n",
                                          [PrettyLineNo]),
                                {added, I};
                            Break ->
                                io:format("\nDelete breakpoint at: ~s\n",
                                          [PrettyLineNo]),
                                Breaks2 = delete_break(Break, Breaks),
                                {added, I#istate{breakpoints = Breaks2}}
                        end
                end;
            [{"lineno", LineNo}] ->
                %% Normal breakpoint
                add_break(I, LineNo, enabled);
            [] ->
                %% List Breakpoints
                Print =
                    fun(#break{lineno=LineNo, type=T}) ->
                            Format =
                                case T of
                                    temporary -> "  ~s\ttemporary\n";
                                    next      -> "  ~s\tnext\n";
                                    enabled   -> "  ~s\n";
                                    disabled  -> "  ~s\n"
                                end,
                            PrettyLineNo = lux_utils:pretty_lineno(I, LineNo),
                            io:format(Format, [PrettyLineNo])
                    end,
                case I#istate.commands of
                    [#cmd{} | _] ->
                        Current = current_call_stack(I),
                        io:format("\nCurrent line: ~s\n",
                                  [lux_utils:pretty_lineno(I, Current)]);
                    [] ->
                        ok
                end,
                case I#istate.breakpoints of
                    [] ->
                        io:format("\nNo breakpoints.\n", []);
                    Breaks ->
                        io:format("\nBreakpoints:\n", []),
                        lists:foreach(Print, Breaks)
                end,
                {added,I}
        end,
    {undefined,I2}.

add_break(I, LineNo, Type) ->
    %% Search for matching command
    case callers(I, LineNo, macro) of
        [] ->
            PrettyLineNo = lux_utils:pretty_lineno(I, LineNo),
            io:format("\nERROR: No such lineno: ~p\n", [PrettyLineNo]),
            {not_added,I};
        CallStacks -> % May be ambiguous
            PrettyLineNo = lux_utils:pretty_lineno(I, LineNo),
            case Type of
                temporary ->
                    io:format("\nSet temporary breakpoint at ~s\n",
                              [PrettyLineNo]);
                next ->
                    ok;
                _ ->
                    io:format("\nSet breakpoint at ~s\n",
                              [PrettyLineNo])
            end,
            RevFile = LineNo#lineno.rev_file,
            RevPos = LineNo#lineno.rev_pos,
            CallStacks2 = [[{RevFile,RevPos}|CS] || CS <- CallStacks],
            NewBreak = #break{lineno=LineNo,
                              type=Type,
                              call_stacks=CallStacks2},
            Breaks = replace_break(NewBreak, I#istate.breakpoints),
            {added,I#istate{breakpoints=Breaks}}
    end.

replace_break(NewBreak, Breaks) ->
    Breaks2 = delete_break(NewBreak, Breaks),
    lists:keysort(#break.lineno, [NewBreak|Breaks2]).

delete_break(Break, Breaks) ->
    lists:keydelete(Break#break.lineno, #break.lineno, Breaks).

check_break(I, Pos) ->
    Breaks = I#istate.breakpoints,
    case find_breaks(I, Pos) of
        [] ->
            {true,I};
        [Break|_AmbigBreaks] ->
            Type = Break#break.type,
            CmdState = {attach,Type},
            Breaks = I#istate.breakpoints,
            case Type of
                temporary ->
                    %% Temporary breakpoint - remove it
                    {_,I2} = cmd_attach(I, [], CmdState),
                    Breaks2 = delete_break(Break, Breaks),
                    {false,I2#istate{breakpoints=Breaks2}};
                next ->
                    %% Temporary breakpoint - remove it
                    {_,I2} = cmd_attach(I, [], CmdState),
                    Breaks2 = delete_break(Break, Breaks),
                    {false,I2#istate{breakpoints=Breaks2}};
                enabled ->
                    %% Normal breakpoint
                    %% Disable it to not get stuck when we continue
                    {_,I2} = cmd_attach(I, [], CmdState),
                    NewBreak = Break#break{type=disabled},
                    Breaks2 = replace_break(NewBreak, Breaks),
                    {false,I2#istate{breakpoints=Breaks2}};
                disabled ->
                    %% Normal breakpoint
                    %% Enable it again for later reuse
                    NewBreak = Break#break{type = enabled},
                    Breaks2 = replace_break(NewBreak, Breaks),
                    {true,I#istate{breakpoints=Breaks2}}
            end
    end.

find_breaks(I, Pos) ->
    Current = [{I#istate.rev_file,Pos} | I#istate.call_stack],
    Filter = fun(Break) -> lists:member(Current, Break#break.call_stacks) end,
    Breaks = I#istate.breakpoints,
    lists:filter(Filter, Breaks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_continue(I, Args, _CmdState) ->
    do_continue(I, Args, _CmdState, temporary).

do_continue(I, Args, _CmdState, Type) ->
    {Res, I2} =
        case Args of
            [{"lineno",BreakPos}] ->
                add_break(I, BreakPos, Type);
            [] ->
                {added,I}
        end,
    case Res of
        added ->
            case Type of
                next ->
                    ok;
                _ ->
                    Current = current_call_stack(I),
                    io:format("\nContinue to run from ~s\n",
                              [lux_utils:pretty_lineno(I, Current)])
            end,
            {Blocked,I3} = opt_unblock(I2),
            case Blocked of
                false ->
                    {undefined,I3};
                true ->
                    I4 = lux_interpret:opt_dispatch_cmd(I3),
                    {undefined, I4}
            end;
        not_added ->
            {undefined,I2}
    end.

opt_unblock(I) ->
    if
        not I#istate.blocked ->
            {false,I};
        I#istate.blocked, I#istate.old_want_more =/= undefined ->
            lists:foreach(fun(#shell{pid=P}) -> P ! {unblock,self()} end,
                          I#istate.shells),
            {true, I#istate{blocked=false,
                            want_more=I#istate.old_want_more,
                            old_want_more=undefined}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_help(I, [], CmdState) ->
    {error, Ambiguous} = select(""),
    io:format("\n~s~s", [help_intro(), Ambiguous]),
    {CmdState,I};
cmd_help(I, [{_,"lineno"}], CmdState) ->
    io:format("~s", [help_lineno()]),
    {CmdState,I};
cmd_help(I, [{_,CmdName}], CmdState) ->
    case select(CmdName) of
        {ok, Cmd} ->
            Pretty = lists:flatten(pretty_cmd(Cmd)),
            io:format("\n~s\n", [Pretty]),
            {CmdState, I};
        {error,ReasonStr} ->
            io:format("\nERROR: ~s", [ReasonStr]),
            {CmdState,I}
    end.

pretty_cmd(#debug_cmd{name=Name, params=Params, help=Help}) ->
    Longest = longest(Params),
    Fun = fun(#debug_param{name=N, presence=Pres}) ->
                  case Pres of
                      optional  -> [" [", N, "]"];
                      mandatory -> [" ", N]
                  end
          end,
    Header = lists:flatten([Name, lists:map(Fun, Params)]),
    [Header,
     "\n",
     lists:duplicate(length(Header), "-"),
     "\n\n",
     Help,
     "\n\n**Parameters:**  \n\n",
     case Params of
         [] ->
             "* no parameters\n\n";
         _ ->
             [pretty_param(P, Longest) || P <-Params]
     end
    ].

pretty_param(#debug_param{name=Name, type=Type, help=Help}, Longest) ->
    PrettyType =
        case Type of
            binary ->
                "string";
            atom ->
                "string";
            existing_atom ->
                "string";
            {enum, [H | T]} ->
                [
                 "enum(",
                 H, [["|", Elem] || Elem <- T],
                 ")"
                ];
            {Atom, Min, Max} ->
                [
                 io_lib:format("~p", [Min]),
                 " >= ",
                 atom_to_list(Atom),
                 " =< ",
                 io_lib:format("~p", [Max])
                ];
            Atom ->
                atom_to_list(Atom)
        end,
    ["* ", string:left(Name, Longest, $\ ),
     " - ", Help, "; ", PrettyType, "  \n"].

longest(Elems) ->
    longest2(Elems, 0).

longest2([H | T], Longest) ->
    case H of
        {_Name, #debug_cmd{name=Str}} -> ok;
        #debug_cmd{name=Str} -> ok;
        #debug_param{name=Str} -> ok;
        Str when is_list(Str) -> ok
    end,
    longest2(T, lists:max([Longest, length(Str)]));
longest2([], Longest) ->
    Longest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_tail(#istate{log_dir=LogDir, tail_status=Status} = I, [], CmdState) ->
    {ok, Cwd} =  file:get_cwd(),
    io:format("Log files at ~s:\n\n",
              [lux_utils:drop_prefix(Cwd, LogDir)]),
    Logs = all_logs(I),
    Print = fun(Abs, Index) ->
                    Rel = lux_utils:drop_prefix(LogDir, Abs),
                    {Curr,Display} =
                        case file:read_file_info(Abs) of
                            {ok, #file_info{size=Size}} ->
                                {Size, ""};
                            {error, Reason} ->
                                FileStr = file:format_error(Reason),
                                io:format("~s: ~s\n", [Reason, FileStr])
                        end,
                    Prefix =
                        case lists:keyfind(Abs, 1, Status) of
                            false ->
                                "*";
                            {_, Prev} when Prev =:= Curr ->
                                " ";
                            _ ->
                                "*"
                        end,
                    io:format("~s~3w ~s~s\n", [Prefix, Index, Rel, Display]),
                    {{Abs,Curr}, Index+1}
            end,
    {Status2, _} = lists:mapfoldl(Print, 1, Logs),
    io:format("\n", []),
    TailOpts = [{"index",2}, {"format","compact"}, {"n_lines",10}],
    _ = cmd_tail(I, TailOpts, CmdState),
    {undefined, I#istate{tail_status=Status2}};
cmd_tail(I, [{"index",Index} | Rest], CmdState) ->
    case Rest of
        [{"format",Format} | Rest2] ->
            case Rest2 of
                [{"n_lines",UserN}] ->
                    ok;
                [] ->
                    UserN = undefined
            end;
        [] ->
            Format = "compact",
            UserN = undefined
    end,
    Logs = all_logs(I),
    case catch lists:nth(Index, Logs) of
        {'EXIT', _} ->
            io:format("ERROR: ~p is not a valid log index."
                      " Must be within ~p..~p.\n",
                      [Index, 1, length(Logs)]),
            {CmdState,I};
        LogFile ->
            tail(I, LogFile, CmdState, Format, UserN)
    end.

all_logs(#istate{orig_file=Script, log_dir=LogDir, logs=StdLogs}) ->
    Split = fun({_Name, Stdin, Stdout}, Acc) -> [Stdout, Stdin | Acc] end,
    Logs = lists:reverse(lists:foldl(Split, [], StdLogs)),
    Base = filename:basename(Script),
    EventLog = filename:join([LogDir, Base ++ ".event.log"]),
    ConfigLog = filename:join([LogDir, Base ++ ".config.log"]),
    [ConfigLog, EventLog | Logs].

tail(#istate{log_dir=LogDir} = I, AbsFile, CmdState, Format, UserN) ->
    RelFile = lux_utils:drop_prefix(LogDir, AbsFile),
    case file:read_file(AbsFile) of
        {ok, Bin} ->
            AllRows = binary:split(Bin, <<"\n">>, [global]),
            Max = length(AllRows),
            N =
                case CmdState of
                    _ when is_integer(UserN) ->
                        %% User specified N
                        UserN;
                    {debug_tail, AbsFile, PrevMin} ->
                        %% Add 10
                        PrevMin + 10;
                    _ ->
                        %% Last 10
                        10
                end,
            Min = lists:max([0, Max - N]),
            TailRows = lists:nthtail(Min, AllRows),
            Actual = length(TailRows),
            io:format("Last ~p (~p..~p) lines of log file: ~s\n\n",
                      [Actual, Max-Actual+1, Max, RelFile]),
            [tail_format(Format, "~s\n", [Row]) || Row <- TailRows],
            {{debug_tail,AbsFile,N}, I};
        {error,FileReason}->
            FileStr = file:format_error(FileReason),
            io:format("ERROR: ~s: ~s\n", [RelFile, FileStr]),
            {undefined, I}
    end.

tail_format("compact", Format, Data) ->
    io:format(Format, Data);
tail_format("verbose", Format, Data) ->
    Str = lists:flatten(io_lib:format(Format, Data)),
    io:format(lux_utils:dequote(Str)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_list(I, Args, CmdState) ->
    Current = current_call_stack(I),
    case CmdState of
        {debug_list,OldDepth,OldN,OldLineNo,OldCount} ->
            ok;
        _ ->
            OldDepth = macro,
            OldN = 10,
            OldLineNo = lux_utils:call_stack_to_lineno(Current),
            OldCount = 0
    end,
    {Depth, N, LineNo, Count} =
        pick_list_args(Args, OldDepth, OldN, OldLineNo, OldCount),
    case callers(I, LineNo, Depth) of
        [] ->
            io:format("\nERROR: No such lineno: ~p\n",
                      [lux_utils:pretty_lineno(I, LineNo)]),
            {undefined,I};
        [CallStack|_] ->
            %% Possibly ambiguous. Just pick one call stack.
            RevFile = LineNo#lineno.rev_file,
            Pos = lists:last(LineNo#lineno.rev_pos),
            do_list2(I, LineNo, N, Count, Current, RevFile, Pos, 
                     CallStack, Depth)
    end.

pick_list_args([], Depth, N, LineNo, Count) ->
    {Depth, N, LineNo, Count};
pick_list_args([Arg|Args], Depth, N, LineNo, _Count) ->
    case Arg of
        {"depth",NewDepth} ->
            pick_list_args(Args, list_to_atom(NewDepth), N, LineNo, 0);
        {"n_lines",NewN} ->
            pick_list_args(Args, Depth, NewN, LineNo, 0);
        {"lineno",NewLineNo} ->
            pick_list_args(Args, Depth, N, NewLineNo, 0)
    end.

do_list2(I, LineNo, N, Skip, Current, RevFile, FirstPos,
         ParentCallStack, Depth) ->
    Print =
        fun(#cmd{rev_file=RF, pos=Pos}=Cmd,
            PCS,
            searching_for_first=Acc) ->
                if
                    RF =:= RevFile, Pos =:= FirstPos, PCS =:= ParentCallStack,
                    Skip =< 0 ->
                        %% Found first line
                        print_line(Cmd, ParentCallStack, Current),
                        {printing, 1, 1};
                    RF =:= RevFile, Pos =:= FirstPos, PCS =:= ParentCallStack,
                    Skip > 0 ->
                        %% Found first line, skipping
                        CountAcc = 1,
                        {skipping, CountAcc, Skip};
                    true ->
                        %% Still searching
                        Acc
                end;
           (Cmd, PCS, {skipping, CountAcc, SkipAcc}) ->
                if
                    CountAcc < SkipAcc ->
                        %% Still skipping
                        {skipping, CountAcc+1, SkipAcc};
                    true ->
                        %% Start printing
                        print_line( Cmd, PCS, Current),
                        {printing, 1, SkipAcc+1}
                end;
           (Cmd, PCS, {printing, CountAcc, SkipAcc}) ->
                if
                    CountAcc < N ->
                        %% Still printing
                        print_line(Cmd, PCS, Current),
                        {printing, CountAcc+1, SkipAcc+1};
                    true ->
                        %% Printing done
                        {printing_done, SkipAcc}
                end;
           (_Cmd, _PCF, {printing_done, _SkipAcc}=Acc) ->
                Acc
        end,
    case lux_utils:foldl_cmds(I, Print, searching_for_first, [], Depth) of
        searching_for_first ->
            io:format("\nERROR: No such lineno: ~p\n",
                      [lux_utils:pretty_lineno(I, LineNo)]),
            {undefined,I};
        {skipping, _CountAcc, _SkipAcc} ->
            %% Skipped enough for this iteration
            {undefined,I};
        {printing, _CountAcc, _SkipAcc} ->
            %% Printed last line
            {undefined,I};
        {printing_done, SkipAcc} ->
            %% Printed enough for this iteration
            {{debug_list,Depth,N,LineNo,SkipAcc},I}
    end.

print_line(#cmd{rev_file=RF, pos=Pos, raw=Text}, ParentCallStack, Current) ->
    CallStack = [{RF,Pos} | ParentCallStack],
    Delim =
        if
            CallStack =:= Current -> ">";
            true                  -> ":"
        end,
    CallStack = [{RF,Pos} | ParentCallStack],
    PrettyLineNo = lux_utils:pretty_call_stack(CallStack),
    io:format("~s~s ~s\n", [PrettyLineNo, Delim, Text]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_load(I, Args, CmdState) ->
    case Args of
        [{"file",File}] ->
            ok;
        [] ->
            File = "lux.debug"
    end,
    case file:read_file(File) of
        {ok,Bin} ->
            io:format("\nLoad commands from file: ~s\n", [File]),
            Fun = fun(CmdStr, {CS,IS,Pos}) ->
                          io:format("~p: ~s\n", [Pos, CmdStr]),
                          {CS2,IS2} = do_eval_cmd(IS, CmdStr, CS),
                          {CS2,IS2,Pos+1}
                  end,
            Lines = string:tokens(binary_to_list(Bin), "\n"),
            {_CmdState,I2,_} = lists:foldl(Fun, {CmdState,I,1}, Lines),
            {undefined,I2};
        {error,Reason} ->
            io:format("\nERROR: Cannot read from file ~p: ~s\n",
                      [File, file:format_error(Reason)]),
            {undefined,I}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_next(I, Args, CmdState) ->
    case Args of
        [{"n_commands", N}] ->
            ok;
        [] ->
            N = 1
    end,
    case catch lists:nth(N+1, I#istate.commands) of
        {'EXIT', _} ->
            do_continue(I, [], CmdState, next);
        #cmd{rev_file=RevFile, pos=Pos} ->
            CallStack = [{RevFile, Pos} | I#istate.call_stack],
            LineNo = lux_utils:call_stack_to_lineno(CallStack),
            do_continue(I, [{"lineno", LineNo}], CmdState, next)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_progress(I, Args, CmdState) ->
    case Args of
        [{"level",Level0}] ->
            Level = list_to_atom(Level0);
        [] ->
            Level =
                case I#istate.progress of
                    verbose -> brief;
                    compact -> brief;
                    doc     -> verbose;
                    brief   -> verbose;
                    silent  -> verbose
                end
    end,
    lists:foreach(fun(#shell{pid=Pid}) -> Pid ! {progress, self(), Level} end,
                  I#istate.shells),
    {CmdState, I#istate{progress=Level}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_quit(I, _Args, _CmdState) ->
    io:format("\nWARNING: Stopped by user.\n", []),
    {_, I2} = opt_unblock(I),
    InterpreterPid = self(),
    InterpreterPid ! stopped_by_user,
    {undefined, I2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_save(I, Args, _CmdState) ->
    case Args of
        [{"file",File}] ->
            ok;
        [] ->
            File = "lux.debug"
    end,
    Save =
        fun(#break{lineno=LineNo, type=Type}) ->
                [
                 "break ", lux_utils:pretty_lineno(I, LineNo),
                 case Type of
                     enabled   -> "";
                     temporary -> " temporary";
                     next      -> " next";
                     disabled  -> ""
                 end,
                 "\n"
                ]
        end,
    IoList = lists:map(Save, I#istate.breakpoints),
    case file:write_file(File, IoList) of
        ok ->
            io:format("\nSave debugger state to file: ~s\n", [File]);
        {error, Reason} ->
            io:format("\nERROR: Cannot write to file ~p: ~s\n",
                      [File, file:format_error(Reason)])
    end,
    {undefined, I}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_skip(I, Args, _CmdState) ->
    case Args of
        [{"n_commands",N}] ->
            ok;
        [] ->
            N = 1
    end,
    {Skipped, Cmds} = skip_cmds(N, I#istate.commands, []),
    case Skipped of
        [] ->
            io:format("\nNo more lines to skip.\n", []);
        [#cmd{pos=Pos}] ->
            io:format("\nSkipped line ~p.\n", [Pos]);
        [Last|Rest] ->
            First = lists:last(Rest),
            io:format("\nSkipped lines ~p..~p.\n", [First, Last])
    end,
    I2 = I#istate{commands = Cmds},
    {undefined, I2}.

skip_cmds(N, [Cmd|Cmds], Skipped) when N > 0 ->
    skip_cmds(N-1, Cmds, [Cmd|Skipped]);
skip_cmds(0, Cmds, Skipped) ->
    {Skipped, Cmds}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_call_stack(I) ->
    Current =
        case I#istate.commands of
            [] ->
                {I#istate.orig_rev_file,1};
            [#cmd{rev_file=RevFile, pos=Pos} | _] ->
                {RevFile, Pos}
        end,
    [Current|I#istate.call_stack].

%% Returns list of valid parent call stacks
callers(I, RevFile, Pos, Depth) ->
    RevDir = lux_utils:filename_split(filename:dirname(I#istate.orig_file)),
    Fun = fun(#cmd{rev_file=RF, pos=P}, CallStack, Acc) ->
                  {Short, Long} = skip_common_parent(RevDir, RevFile, RF),
                  case match_prefix(Short, Long) of
                      true when P =:= Pos ->
                          CallStack2 = [{RF,P}|CallStack],
                          [CallStack2|Acc];
                      _ ->
                          Acc
                  end
          end,
    lux_utils:foldl_cmds(I, Fun, [], [], Depth).

skip_common_parent([Common|Parent], [Common|Short], [Common|Long]) ->
    skip_common_parent(Parent, Short, Long);
skip_common_parent(_Parent, Short, Long) ->
    {Short, Long}.

match_prefix([ShortComp|Short], [LongComp|Long]) ->
    lists:prefix(ShortComp, LongComp) andalso
        match_prefix(Short, Long);
match_prefix([], _Long) ->
    true;
match_prefix(_Short, _Long) ->
    false.

%% Returns list of valid parent call stacks
callers(I, #lineno{rev_file=RevFile, rev_pos=RevPos}, Depth) ->
    TopPos = lists:last(RevPos),
    case callers(I, RevFile, TopPos, Depth) of
        [] ->
            [];
        CallStacks ->
            [CS || CS <- CallStacks,
                   equal_call_stack([{RevFile,TopPos}|CS], RevPos)]
    end.

equal_call_stack([{_RevFile, Pos}|CallStack], [Pos|RevPos]) ->
    equal_call_stack(CallStack, RevPos);
equal_call_stack([], []) ->
    true;
equal_call_stack(_CallStack, _RevPos) ->
    false.
