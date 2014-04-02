%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2014 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_log).

-export([parse_summary_log/1,
         open_summary_log/1, close_summary_log/2,
         print_results/4,
         safe_format/3, safe_write/2, double_write/2,
         open_event_log/5, close_event_log/1, write_event/4, scan_events/1,
         parse_events/2, parse_config/1, parse_io_logs/2, parse_result/1,
         open_config_log/3, close_config_log/2,
         safe_format/5, safe_write/4]).

-include("lux.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Summary log

open_summary_log(SummaryLog) ->
   TmpSummaryLog = SummaryLog ++ ".tmp",
    case file:open(TmpSummaryLog, [write]) of
        {ok, SummaryFd} ->
            IoList = io_lib:format("~s~s\n",
                                   [?TAG("summary log"), SummaryLog]),
            double_write(SummaryFd, IoList),
            {ok, SummaryFd};
        {error, Reason} ->
            {error, Reason}
    end.

close_summary_log(SummaryFd, SummaryLog) ->
    file:close(SummaryFd),
    TmpSummaryLog = SummaryLog ++ ".tmp",
    ok = file:rename(TmpSummaryLog, SummaryLog).


print_results(Fd, Summary, Results, Warnings) ->
    %% Display most important results last
    io:nl(),
    safe_format(Fd, "\n", []),
    SuccessScripts =
        [Script || {ok, Script, success, _FullLineNo, _Events} <- Results],
    double_format(Fd, "~s~p\n",
                  [?TAG("successful"),
                   length(SuccessScripts)]),
    print_skip(Fd, Results),
    print_warning(Fd, Warnings),
    print_fail(Fd, Results),
    print_error(Fd, Results),
    double_format(Fd, "~s~s\n",
                  [?TAG("summary"),
                   [string:to_upper(Char) ||
                       Char <- atom_to_list(Summary)]]).

print_skip(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, skip, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        SkipScripts ->
            double_format(Fd, "~s~p\n",
                          [?TAG("skipped"),
                           length(SkipScripts)]),
            [double_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- SkipScripts]
    end.


print_warning(Fd, Warnings) ->
    case [{Script, FullLineNo} ||
             {warning, Script, FullLineNo, _String} <- Warnings] of
        [] ->
            ok;
        WarnScripts ->
            double_format(Fd, "~s~p\n",
                          [?TAG("warnings"),
                           length(WarnScripts)]),
            [double_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- WarnScripts]
    end.

print_fail(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {ok, Script, fail, FullLineNo, _Events} <- Results] of
        [] ->
            ok;
        FailScripts ->
            double_format(Fd, "~s~p\n",
                          [?TAG("failed"),
                           length(FailScripts)]),
            [double_format(Fd, "\t~s:~s\n", [F, L]) || {F, L} <- FailScripts]
    end.

print_error(Fd, Results) ->
    case [{Script, FullLineNo} ||
             {error, Script, FullLineNo, _String} <- Results] of
        [] ->
            ok;
        ErrorScripts ->
            double_format(Fd, "~s~p\n",
                          [?TAG("errors"),
                           length(ErrorScripts)]),
            [double_format(Fd, "\t~s:~s\n", [F, L]) ||
                {F, L} <- ErrorScripts]
    end.

parse_summary_log(SummaryLog) ->
    case file:read_file(SummaryLog) of
        {ok, LogBin} ->
            Sections = binary:split(LogBin, <<"\n\n">>, [global]),
            [Summary, ArchConfig | Rest] = Sections,
            [_, SummaryLog2] = binary:split(Summary, <<": ">>),
            [Result | Rest2] = lists:reverse(Rest),
            Result2 = split_result(Result),
            {Groups, EventLogs} = split_groups(Rest2, [], []),
            {ok, FI} = file:read_file_info(SummaryLog),
            {ok, SummaryLog2, Result2, Groups, ArchConfig, FI, EventLogs};
        {error, FileReason} ->
            {error, SummaryLog, file:format_error(FileReason)}
    end.

split_result(Result) ->
    Lines = binary:split(Result, <<"\n">>, [global]),
    [_, Summary | Rest] = lists:reverse(Lines),
    [_, Summary2] = binary:split(Summary, <<": ">>),
    Lines2 = lists:reverse(Rest),
    Sections = split_result2(Lines2, []),
    {result, Summary2, Sections}.

split_result2([Heading | Lines], Acc) ->
    [Slogan, Count] = binary:split(Heading, <<": ">>),
    [Slogan2, _] = binary:split(Slogan, <<" ">>),
    Pred = fun(Line) ->
                   case Line of
                       <<"\t", _File/binary>> -> true;
                       _ -> false
                   end
           end,
    {Files, Lines2} = lists:splitwith(Pred, Lines),
    Parse = fun(<<"\t", File/binary>>) ->
                    [File2, LineNo] = binary:split(File, <<":">>),
                    {file, File2, LineNo}
            end,
    Files2 = lists:map(Parse, Files),
    split_result2(Lines2, [{section, Slogan2, Count, Files2} | Acc]);
split_result2([], Acc) ->
    Acc. % Return in reverse order (most important first)

split_groups([GroupEnd | Groups], Acc, EventLogs) ->
    Pred = fun(Case) ->
                   case binary:split(Case, <<": ">>) of
                       %% BUGBUG: Kept for backwards compatibility a while
                       [<<"test suite begin", _/binary>> |_] -> false;
                       [<<"test group begin", _/binary>> |_] -> false;
                       _ -> true
                   end
           end,
    Split = lists:splitwith(Pred, Groups),
    {Cases, [GroupBegin | Groups2]} = Split,
    [_, Group] = binary:split(GroupBegin, <<": ">>),
    [_, Group] = binary:split(GroupEnd, <<": ">>),
    {Cases2, EventLogs2} =
        split_cases(lists:reverse(Cases), [], EventLogs),
    split_groups(Groups2, [{test_group, Group, Cases2} | Acc], EventLogs2);
split_groups([], Acc, EventLogs) ->
    {Acc, EventLogs}.

split_cases([Case | Cases], Acc, EventLogs) ->
    [NameRow | Sections] = binary:split(Case, <<"\n">>, [global]),
    [<<"test case", _/binary>>, Name] = binary:split(NameRow, <<": ">>),
    case Sections of
        [] ->
            Res = {result_case, Name, <<"ERROR">>, <<"unknown">>},
            split_cases(Cases, [Res | Acc], EventLogs);
        [Reason] ->
            Res =
                case binary:split(Reason,    <<": ">>) of
                    [<<"result", _/binary>>, Reason2] ->
                        {result_case, Name, Reason2, Reason};
                    [<<"error", _/binary>>, Reason2] ->
                        {result_case, Name, <<"ERROR">>, Reason2}
                end,
            split_cases(Cases, [Res | Acc], EventLogs);
        [_ScriptRow, LogRow | DocAndResult] ->
            [<<"event log", _/binary>>, RawEventLog] =
                binary:split(LogRow,  <<": ">>),
            {Doc, ResultCase} = split_doc(DocAndResult, []),
            Result = lux_log:parse_result(ResultCase),
            EventLog = binary_to_list(RawEventLog),
            HtmlLog = EventLog ++ ".html",
            Res = {test_case, Name, EventLog, Doc, HtmlLog, Result},
            split_cases(Cases, [Res | Acc], [EventLog|EventLogs])
    end;
split_cases([], Acc, EventLogs) ->
    {lists:reverse(Acc), EventLogs}.

split_doc([H|T] = Rest, AccDoc) ->
    case binary:split(H, <<": ">>) of
        [<<"doc", _/binary>>, Doc] ->
            split_doc(T, [Doc | AccDoc]);
        _ ->
            {lists:reverse(AccDoc), Rest}
    end.

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

safe_format(Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    safe_write(Fd, IoList).

double_format(Fd, Format, Args) ->
    IoList = io_lib:format(Format, Args),
    case Fd of
        undefined -> list_to_binary(IoList);
        _         -> double_write(Fd, IoList)
    end.

safe_write(OptFd, IoList) when is_list(IoList) ->
    safe_write(OptFd, list_to_binary(IoList));
safe_write(OptFd, Bin) when is_binary(Bin) ->
    case OptFd of
        undefined ->
            ok = io:format(Bin),
            Bin;
        Fd ->
            ok = file:write(Fd, Bin),
            Bin
    end.

double_write(Fd, IoList) when Fd =/= undefined ->
    Bin = safe_write(Fd, IoList),
    safe_write(undefined, Bin).

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
