%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2014 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([open_event_log/5, close_event_log/1, write_event/4, scan_events/1,
         parse_events/2, parse_config/1, parse_io_logs/2, parse_result/1,
         open_config_log/3, close_config_log/2,
         safe_format/5, safe_write/4]).

-include("lux.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Event log

open_event_log(LogDir, Script, Progress, LogFun, Verbose) ->
    Base = filename:basename(Script),
    EventLog = filename:join([LogDir, Base ++ ".event.log"]),
    ok = filelib:ensure_dir(EventLog),
    case file:open(EventLog, [write]) of
        {ok, EventFd} ->
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "~s~s\n", [?TAG("event log"), ?EVENT_LOG_VERSION]),
            safe_format(Progress, LogFun, {Verbose, EventFd},
                        "\n~s\n\n", [filename:absname(Script)]),
            {ok, EventLog, EventFd};
        {error, Reason} ->
            {error, Reason}
    end.

close_event_log(EventFd) ->
    file:close(EventFd).

write_event(Progress, LogFun, Fd, {event, LineNo, Shell, Op, "", []}) ->
    Data = io_lib:format("~s(~p): ~p\n", [Shell, LineNo, Op]),
    safe_write(Progress, LogFun, Fd, Data);
write_event(Progress, LogFun, Fd, {event, LineNo, Shell, Op, Format, Args}) ->
    Data = io_lib:format("~s(~p): ~p "++Format++"\n",
                         [Shell, LineNo, Op | Args]),
    safe_write(Progress, LogFun, Fd, Data).

scan_events(EventLog) ->
    case file:read_file(EventLog) of
        {ok, <<"event log         : 0.1\n\n", LogBin/binary>>} ->
            scan_events_0_1(EventLog, LogBin);
        {ok, LogBin} ->
            scan_events_old(EventLog, LogBin);
        {error, FileReason} ->
            {error, EventLog, file:format_error(FileReason)}
    end.

scan_events_0_1(EventLog, LogBin) ->
    EventSections = binary:split(LogBin, <<"\n\n">>, [global]),
    EventSections2 = [binary:split(S, <<"\n">>, [global]) ||
                         S <- EventSections],
    case EventSections2 of
        [[Script], EventBins, ResultBins] -> ok;
        [[Script], ResultBins]            -> EventBins = []
    end,
    Dir = filename:dirname(EventLog),
    Base = filename:basename(EventLog, ".event.log"),
    ConfigLog = filename:join([Dir, Base ++ ".config.log"]),
    case file:read_file(ConfigLog) of
        {ok, <<"config log        : 0.1\n", ConfigBin/binary>>} ->
            ConfigSections = binary:split(ConfigBin, <<"\n\n">>, [global]),
            ConfigSections2 = [binary:split(S, <<"\n">>, [global])
                               || S <- ConfigSections],
            [ConfigBins, LogBins] = ConfigSections2,
            {ok, EventLog, ConfigLog,
             Script, EventBins, ConfigBins, LogBins, ResultBins};
        {error, FileReason} ->
            {error, ConfigLog, file:format_error(FileReason)}
    end.

scan_events_old(EventLog, LogBin) ->
    Sections = binary:split(LogBin, <<"\n\n">>, [global]),
    Sections2 = [binary:split(S, <<"\n">>, [global]) ||
                    S <- Sections],
    case Sections2 of
        [ScriptBins, EventBins, ConfigBins, LogBins, ResultBins] ->
            ok;
        [ScriptBins, EventBins, ConfigBins, [<<>>|ResultBins]] ->
            LogBins = [];
        [ScriptBins, [<<>>|ConfigBins], [<<>>|ResultBins]] ->
            LogBins = [],
            EventBins = []
    end,
    [Script] = ScriptBins,
    {ok, EventLog, <<"">>, Script, EventBins, ConfigBins, LogBins, ResultBins}.

parse_events([<<>>], Acc) ->
    %% Error case
    lists:reverse(Acc);
parse_events(Events, Acc) ->
    do_parse_events(Events, Acc).

do_parse_events([<<"include_begin ", SubFile/binary>> | Events], Acc) ->
    %% include_begin 11 47 53 demo/test.include
    %% include_end 11 47 53 demo/test.include
    Pred = fun(E) ->
                   case E of
                       <<"include_end ", SubFile/binary>> ->
                           false;
                       _ ->
                           true
                   end
           end,
    {SubEvents, [_| Events2]} = lists:splitwith(Pred, Events),
    [RawLineNoRange, SubFile2] = binary:split(SubFile, <<" \"">>),
    [RawLineNo, RawFirstLineNo, RawLastLineNo] =
        binary:split(RawLineNoRange, <<" ">>, [global]),
    Len = byte_size(SubFile2) - 1 ,
    <<SubFile3:Len/binary, _/binary>> = SubFile2,
    LineNo = list_to_integer(binary_to_list(RawLineNo)),
    FirstLineNo = list_to_integer(binary_to_list(RawFirstLineNo)),
    LastLineNo = list_to_integer(binary_to_list(RawLastLineNo)),
    SubEvents2 = parse_events(SubEvents, []),
    E = {include, LineNo, FirstLineNo, LastLineNo, SubFile3, SubEvents2},
    do_parse_events(Events2, [E | Acc]);
do_parse_events([Event | Events], Acc) ->
    [Prefix, Details] = binary:split(Event, <<"): ">>),
    [Shell, RawLineNo] = binary:split(Prefix, <<"(">>),
    LineNo = list_to_integer(binary_to_list(RawLineNo)),
    [Op | RawContents] = binary:split(Details, <<" ">>),
    Data =
        case RawContents of
            [] ->
                %% cli(86): suspend
                [<<>>];
            [Contents] ->
                case unquote(Contents) of
                    {quote, C} ->
                        %% cli(26): recv "echo ==$?==\r\n==0==\r\n$ "
                        split_lines(C);
                    {plain, C} ->
                        %% cli(70): timer start (10 seconds)
                        [C]
                end
        end,
    E = {event, LineNo, Shell, Op, Data},
    do_parse_events(Events, [E | Acc]);
do_parse_events([], Acc) ->
    lists:reverse(Acc).

split_lines(<<"">>) ->
    [];
split_lines(Bin) ->
    Opts = [global],
    Replace = fun(NL, B) -> binary:replace(B , NL, <<"\n">>, Opts) end,
    NLs = [<<"[\\r\\n]+">>, <<"\\r\\n">>,
           <<"\n\r">>, <<"\r\n">>,
           <<"\\n">>, <<"\\r">>],
    Normalized = lists:foldl(Replace, Bin, NLs),
    binary:split(Normalized, <<"\n">>, Opts).

parse_config(RawConfig) ->
    %% io:format("Config: ~p\n", [RawConfig]),
    RawConfig.

parse_io_logs([StdinLog, StdoutLog | Logs], Acc) ->
    [_, Shell, Stdin] = binary:split(StdinLog, <<": ">>, [global]),
    [_, Shell, Stdout] = binary:split(StdoutLog, <<": ">>, [global]),
    L = {log, Shell, Stdin, Stdout},
    %% io:format("Logs: ~p\n", [L]),
    parse_io_logs(Logs, [L | Acc]);
parse_io_logs([<<>>], Acc) ->
    lists:reverse(Acc);
parse_io_logs([], Acc) ->
    lists:reverse(Acc).

parse_result(RawResult) ->
    case RawResult of
        [<<>>, LongResult | Rest] -> ok;
        [LongResult | Rest]       -> ok
    end,
    [_, Result] = binary:split(LongResult, <<": ">>),
    R =
        case Result of
            <<"SUCCESS">> ->
                success;
            <<"ERROR at ", Error/binary>> ->
                [RawLineNo, Reason] = binary:split(Error, <<":">>),
                {error_line, RawLineNo, [Reason | Rest]};
            <<"ERROR ", Reason/binary>> ->
                {error, [Reason | Rest]};
            <<"FAIL at ", Fail/binary>> ->
                [<<"expected">>, Expected,
                 <<"actual ", Actual/binary>>, Details | _] = Rest,
                [Script, RawLineNo] = binary:split(Fail, <<":">>),
                {quote, Expected2} = unquote(Expected),
                Expected3 = split_lines(Expected2),
                {quote, Details2} = unquote(Details),
                Details3 = split_lines(Details2),
                {fail, Script, RawLineNo, Expected3, Actual, Details3}
        end,
    %% io:format("Result: ~p\n", [R]),
    {result, R}.

unquote(Bin) ->
    Quote = <<"\"">>,
    Size = byte_size(Bin)-2,
    case Bin of
        <<Quote:1/binary, Plain:Size/binary, Quote:1/binary>> ->
            {quote, Plain};
        Plain ->
            {plain, Plain}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Config log

open_config_log(LogDir, Script, Config) ->
    Base = filename:basename(Script),
    ConfigFile = filename:join([LogDir, Base ++ ".config.log"]),
    ok = filelib:ensure_dir(ConfigFile),
    case file:open(ConfigFile, [write]) of
        {ok, ConfigFd} ->
            Data = config_data(Config),
            ok = file:write(ConfigFd, Data),
            ok = file:write(ConfigFd, "\n"),
            ConfigFd;
        {error, FileReason} ->
            ReasonStr = ConfigFile ++ ": " ++file:format_error(FileReason),
            erlang:error(ReasonStr)
    end.

close_config_log(ConfigFd, Logs) ->
    ShowLog =
        fun({Name, Stdin, Stdout}) ->
                Data =
                    [
                     io_lib:format("~s~s: ~s\n",
                                   [?TAG("stdin  log file"), Name, Stdin]),
                     io_lib:format("~s~s: ~s\n",
                                   [?TAG("stdout log file"), Name, Stdout])
                    ],
                ok = file:write(ConfigFd, Data)
        end,
    lists:foreach(ShowLog, Logs),
    file:close(ConfigFd).

config_data(Config) ->
    Fun =
        fun({Tag, Type, Val}) ->
                case Type of
                    string ->
                        io_lib:format("~s~s\n", [?TAG(Tag), Val]);
                    dict ->
                        Val2 = [lux_utils:to_string(E) || E <- Val],
                        io_lib:format("~s~p\n", [?TAG(Tag), Val2]);
                    term ->
                        io_lib:format("~s~p\n", [?TAG(Tag), Val])
                end
        end,
    lists:map(Fun, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_format(Progress, LogFun, Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Progress, LogFun, Fd, IoList).

safe_write(Progress, LogFun, Fd, IoList) when is_list(IoList) ->
    safe_write(Progress, LogFun, Fd, list_to_binary(IoList));
safe_write(Progress, LogFun, Fd0, Bin) when is_binary(Bin) ->
    case Fd0 of
        undefined  ->
            Fd = Fd0,
            Verbose = false;
        {Verbose, Fd} ->
            ok
    end,
    case Progress of
        silent ->
            ok;
        brief ->
            ok;
        doc ->
            ok;
        compact when Verbose ->
            try
                io:format("~s", [binary_to_list(Bin)])
            catch
                _:CReason ->
                    exit({safe_write, verbose, Bin, CReason})
            end;
        compact ->
            ok;
        verbose when Verbose ->
            try
                io:format("~s", [lux_utils:dequote(binary_to_list(Bin))])
            catch
                _:VReason ->
                    exit({safe_write, verbose, Bin, VReason})
            end;
        verbose ->
            ok
    end,
    case Fd of
        undefined ->
            try
                case LogFun(Bin) of
                    <<_/binary>> ->
                        ok;
                    BadRes ->
                        exit({safe_write, log_fun, Bin, BadRes})
                end
            catch
                _:LReason ->
                    exit({safe_write, log_fun, Bin, LReason})
            end;
        _ ->
            try file:write(Fd, Bin) of
                ok ->
                    ok;
                {error, FReason} ->
                    Str = file:format_error(FReason),
                    io:format("\nfile write failed: ~s\n", [Str]),
                    exit({safe_write, file, Fd, Bin, {error, FReason}})
            catch
                _:WReason ->
                    exit({safe_write, file, Bin, WReason})
            end
    end,
    Bin.
