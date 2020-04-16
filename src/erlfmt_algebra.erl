%% Copyright (c) Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(erlfmt_algebra).

-export([string_new/1, string_append/2, string_spaces/1, string_text/1, string_length/1]).
-export([lines_new/1, lines_combine/2, lines_flush/1, lines_render/1]).
-export([metric_new/1, metric_combine/2, metric_flush/1, metric_render/1, metric_dominates/2]).
-export([
    document_text/1,
    document_spaces/1,
    document_combine/2,
    document_flush/1,
    document_choice/2,
    document_single_line/1,
    document_prepend/2,
    document_render/2,
    document_reduce/2,
    document_fail/0
]).

-export_type([text/0, str/0, lines/0, metric/0, document/0, option/0]).

-type text() :: unicode:chardata().

-record(string, {length :: non_neg_integer(), text :: text()}).

-opaque str() :: #string{}.

-record(lines_combine, {left :: lines(), right :: lines()}).

-record(lines_flush, {lines :: lines()}).

-opaque lines() :: #string{} | #lines_combine{} | #lines_flush{}.

%% TODO: could we use a more efficient data structure here?
%% We'd benefit from efficient operations at both ends, could a queue be the answer?
-record(doc_seq, {seq :: nonempty_list(document())}).

-record(doc_flush, {doc :: document()}).

-record(doc_choice, {choices :: nonempty_list(document())}).

-record(doc_fail, {}).

-opaque document() :: #string{} | #doc_seq{} | #doc_flush{} | #doc_choice{} | #doc_fail{}.

%% Order of fields is important for comparisons!
-record(metric, {
    height :: non_neg_integer(),
    max_width :: non_neg_integer(),
    last_width :: non_neg_integer()
}).

-opaque metric() :: #metric{}.

-type option() :: {page_width, pos_integer()}.

-define(DEFAULT_PAGE_WIDTH, 80).

-spec string_new(text()) -> str().
string_new(Text) -> #string{length = string:length(Text), text = Text}.

-spec string_spaces(integer()) -> str().
string_spaces(Count) -> #string{length = Count, text = binary:copy(<<" ">>, Count)}.

-spec string_append(str(), str()) -> str().
string_append(Left, Right) ->
    #string{
        length = Left#string.length + Right#string.length,
        text = [Left#string.text | Right#string.text]
    }.

-spec string_text(str()) -> text().
string_text(#string{text = Text}) -> Text.

-spec string_length(str()) -> non_neg_integer().
string_length(#string{length = Length}) -> Length.

string_empty() -> #string{length = 0, text = ""}.

-spec lines_new(str()) -> lines().
lines_new(#string{} = String) -> String.

-spec lines_combine(lines(), lines()) -> lines().
lines_combine(#string{} = Left, #string{} = Right) ->
    string_append(Left, Right);
lines_combine(Left, Right) ->
    #lines_combine{left = Left, right = Right}.

-spec lines_flush(lines()) -> lines().
lines_flush(Lines) ->
    #lines_flush{lines = Lines}.

-spec lines_render(lines()) -> text().
lines_render(Lines) ->
    tl(strings_to_text(do_lines_render(Lines))).

strings_to_text([#string{text = Text} | Rest]) ->
    case string:trim(Text, leading, " ") of
        [] -> [$\n | strings_to_text(Rest)];
        _ -> [$\n, string:trim(Text, trailing, " ") | strings_to_text(Rest)]
    end;
strings_to_text([]) ->
    [].

%% TODO: there's probably a more efficient way to do this, possibly
%% by using a better data structure or making it tail-recursive
-spec do_lines_render(lines()) -> [str()].
do_lines_render(#string{} = Line) ->
    [Line];
do_lines_render(#lines_combine{left = Left, right = Right}) ->
    {LeftLast, LeftHeadRev} = split_last(do_lines_render(Left)),
    {RightFirst, RightTail} = split_first(do_lines_render(Right)),
    CombinedLine = string_append(LeftLast, RightFirst),
    IndentedLines = indent_lines(LeftLast#string.length, RightTail),
    lists:reverse(LeftHeadRev, [CombinedLine | IndentedLines]);
do_lines_render(#lines_flush{lines = Lines}) ->
    do_lines_render(Lines) ++ [string_empty()].

split_first([First | Rest]) -> {First, Rest}.

split_last([Line | Lines]) -> split_last(Lines, Line, []).

split_last([Line | Rest], Last, LinesRev) -> split_last(Rest, Line, [Last | LinesRev]);
split_last([], Last, LinesRev) -> {Last, LinesRev}.

indent_lines(0, Lines) -> Lines;
indent_lines(N, Lines) ->
    Offset = string_spaces(N),
    [string_append(Offset, Line) || Line <- Lines].

-spec metric_new(str()) -> metric().
metric_new(#string{length = Length}) ->
    #metric{height = 0, max_width = Length, last_width = Length}.

-spec metric_combine(metric(), metric()) -> metric().
metric_combine(Left, Right) ->
    #metric{
        height = Left#metric.height + Right#metric.height,
        max_width = max(Left#metric.max_width, Right#metric.max_width + Left#metric.last_width),
        last_width = Left#metric.last_width + Right#metric.last_width
    }.

-spec metric_flush(metric()) -> metric().
metric_flush(Metric) ->
    Metric#metric{height = Metric#metric.height + 1, last_width = 0}.

-spec metric_render(metric()) -> text().
metric_render(Metric) ->
    Line = lists:duplicate(Metric#metric.max_width, $x),
    LastLine = lists:duplicate(Metric#metric.last_width, $x),
    [lists:duplicate(Metric#metric.height, [Line, $\n]) | LastLine].

-spec metric_dominates(metric(), metric()) -> boolean().
metric_dominates(Metric0, Metric1) ->
    Metric0#metric.height =< Metric1#metric.height andalso
        Metric0#metric.last_width =< Metric1#metric.last_width andalso
        Metric0#metric.max_width =< Metric1#metric.max_width.

-spec document_text(text()) -> document().
document_text(Text) -> string_new(Text).

-spec document_spaces(integer()) -> document().
document_spaces(Count) -> string_spaces(Count).

-spec document_combine(document(), document()) -> document().
document_combine(#doc_fail{}, _) -> #doc_fail{};
document_combine(_, #doc_fail{}) -> #doc_fail{};
document_combine(#string{} = Left, #string{} = Right) ->
    string_append(Left, Right);
document_combine(#doc_seq{seq = Seq1}, #doc_seq{seq = Seq2}) ->
    #doc_seq{seq = Seq1 ++ Seq2};
document_combine(#doc_seq{seq = Seq}, Document) ->
    #doc_seq{seq = Seq ++ [Document]};
document_combine(Document, #doc_seq{seq = Seq}) ->
    #doc_seq{seq = [Document | Seq]};
document_combine(Document1, Document2) ->
    #doc_seq{seq = [Document1, Document2]}.

-spec document_flush(document()) -> document().
document_flush(Document) -> #doc_flush{doc = Document}.

-spec document_fail() -> document().
document_fail() -> #doc_fail{}.

-spec document_choice(document(), document()) -> document().
%% TODO: try to reduce the number of choices if both alternatives have the same
%% elements by folidng them together - e.g. same prefix/suffix for seq or both flush.
document_choice(#doc_fail{}, Document) -> Document;
document_choice(Document, #doc_fail{}) -> Document;
document_choice(#doc_choice{choices = Choices1}, #doc_choice{choices = Choices2}) ->
    #doc_choice{choices = Choices1 ++ Choices2};
document_choice(#doc_choice{choices = Choices}, Document) ->
    #doc_choice{choices = [Document | Choices]};
document_choice(Document, #doc_choice{choices = Choices}) ->
    #doc_choice{choices = [Document | Choices]};
document_choice(Document1, Document2) ->
    #doc_choice{choices = [Document1, Document2]}.

-spec document_single_line(document()) -> document().
document_single_line(#doc_flush{}) -> #doc_fail{};
document_single_line(#string{} = String) -> String;
document_single_line(#doc_fail{} = Fail) -> Fail;
document_single_line(#doc_choice{choices = Choices}) -> choice_single_line(Choices, []);
document_single_line(#doc_seq{seq = Seq}) -> seq_single_line(Seq, []).

choice_single_line([Choice | Choices], Acc) ->
    case document_single_line(Choice) of
        #doc_fail{} -> choice_single_line(Choices, Acc);
        Document -> choice_single_line(Choices, [Document | Acc])
    end;
choice_single_line([], []) -> #doc_fail{};
choice_single_line([], [SingleChoice]) -> SingleChoice;
choice_single_line([], Choices) -> #doc_choice{choices = Choices}.

seq_single_line([Doc0 | Docs], Acc) ->
    case document_single_line(Doc0) of
        #doc_fail{} -> #doc_fail{};
        Doc -> seq_single_line(Docs, [Doc | Acc])
    end;
seq_single_line([], Acc) -> #doc_seq{seq = lists:reverse(Acc)}.

-spec document_prepend(document(), document()) -> document().
document_prepend(#doc_fail{}, _) -> #doc_fail{};
document_prepend(_, #doc_fail{}) -> #doc_fail{};
document_prepend(#string{} = Left, #string{} = Right) ->
    string_append(Left, Right);
document_prepend(Document1, #string{} = Document2) ->
    #doc_seq{seq = [Document1, Document2]};
document_prepend(Document, #doc_seq{seq = [SeqHead | Seq]}) ->
    #doc_seq{seq = [document_prepend(Document, SeqHead) | Seq]};
document_prepend(Document, #doc_flush{doc = Inner}) ->
    #doc_flush{doc = document_prepend(Document, Inner)};
document_prepend(Document, #doc_choice{choices = Choices}) ->
    #doc_choice{choices = [document_prepend(Document, Choice) || Choice <- Choices]}.

-spec document_render(document(), [option()]) -> text().
document_render(Document, Options) ->
    PageWidth = proplists:get_value(page_width, Options, ?DEFAULT_PAGE_WIDTH),
    Layouts0 = document_interpret(Document, PageWidth),
    case reject_invalid(Layouts0, PageWidth, Options) of
        [] -> error(no_viable_layout);
        Layouts ->
            [{_Metric, Lines} | _] = lists:keysort(1, Layouts),
            lines_render(Lines)
    end.

%% Same as lists:foldr/3 except it doesn't need initial accumulator
%%   and just uses the last element of the list for that purpose.
-spec document_reduce(Reducer, [document(), ...]) -> document() when
    Reducer :: fun ((document(), document()) -> document()).
document_reduce(_Fun, [Doc]) -> Doc;
document_reduce(Fun, [Doc | Rest]) -> Fun(Doc, document_reduce(Fun, Rest)).

-spec document_interpret(document(), non_neg_integer()) -> [{metric(), lines()}].
document_interpret(#string{} = String, _PageWidth) ->
    [{metric_new(String), lines_new(String)}];
document_interpret(#doc_flush{doc = Document}, PageWidth) ->
    Layouts0 = document_interpret(Document, PageWidth),
    Layouts = [{metric_flush(Metric), lines_flush(Lines)} || {Metric, Lines} <- Layouts0],
    lists:foldl(fun pareto_frontier_add/2, [], Layouts);
document_interpret(#doc_fail{}, _PageWidth) ->
    [];
document_interpret(#doc_seq{seq = [Doc | Seq]}, PageWidth) ->
    interpret_seq(Seq, document_interpret(Doc, PageWidth), PageWidth);
document_interpret(#doc_choice{choices = [Choice | Choices]}, PageWidth) ->
    interpret_choice(Choices, document_interpret(Choice, PageWidth), PageWidth).

interpret_choice([Choice | Choices], Frontier0, PageWidth) ->
    Layouts = document_interpret(Choice, PageWidth),
    Frontier = lists:foldl(fun pareto_frontier_add/2, Frontier0, Layouts),
    interpret_choice(Choices, Frontier, PageWidth);
interpret_choice([], Frontier, _PageWidth) -> Frontier.

interpret_seq([Doc | Seq], Lefts, PageWidth) ->
    case document_interpret(Doc, PageWidth) of
        [] ->
            [];
        Rights ->
            CombinedFrontier = layout_combine_many(Lefts, Rights, PageWidth, [], []),
            interpret_seq(Seq, CombinedFrontier, PageWidth)
    end;
interpret_seq(_Seq, [], _PageWidth) -> [];
interpret_seq([], Acc, _PageWidth) -> Acc.

layout_combine_many([Left | Lefts], Rights, PageWidth, Frontier0, Unfit0) ->
    {Frontier, Unfit} = layout_combine_many1(Left, Rights, PageWidth, Frontier0, Unfit0),
    layout_combine_many(Lefts, Rights, PageWidth, Frontier, Unfit);
layout_combine_many([], _Rights, _PageWidth, [], Unfit) ->
    best_unfit(Unfit);
layout_combine_many([], _Rights, _PageWidth, Frontier, _Unfit) ->
    Frontier.

layout_combine_many1({LMetric, LLines} = Left, [{RMetric, RLines} | Rights], PageWidth, Frontier0, Unfit0) ->
    Metric = metric_combine(LMetric, RMetric),
    Layout = {Metric, lines_combine(LLines, RLines)},
    case Metric#metric.max_width =< PageWidth of
        true ->
            Frontier = pareto_frontier_add(Layout, Frontier0),
            %% We can discard unfit, since we know there's at least one fitting layout
            layout_combine_many1(Left, Rights, PageWidth, Frontier, []);
        false ->
            layout_combine_many1(Left, Rights, PageWidth, Frontier0, [Layout | Unfit0])
    end;
layout_combine_many1(_Left, [], _PageWidth, Frontier, Unfit) ->
    {Frontier, Unfit}.

%% TODO: if we kept some order in how we add new layouts, we could probably
%% avoid this re-filtering step.
pareto_frontier_add({Metric, _} = Layout, Layouts) ->
    case any_dominates(Metric, Layouts) of
        true -> Layouts;
        false -> [Layout | filter_dominated(Metric, Layouts)]
    end.

any_dominates(Metric, [Layout | Layouts]) ->
    metric_dominates(element(1, Layout), Metric) orelse any_dominates(Metric, Layouts);
any_dominates(_Metric, []) -> false.

filter_dominated(Metric, Layouts) ->
    [Layout || Layout <- Layouts, not metric_dominates(Metric, element(1, Layout))].

reject_invalid(Layouts, PageWidth, Options) ->
    AllowUnfit = proplists:get_value(allow_unfit, Options, true),
    case [Layout || {Metric, _} = Layout <- Layouts, Metric#metric.max_width =< PageWidth] of
        [] when AllowUnfit -> best_unfit(Layouts);
        [] -> [];
        Filtered -> Filtered
    end.

best_unfit([]) -> [];
best_unfit([First | Rest]) -> [best_unfit(Rest, First)].

best_unfit([{CandidateMetric, _} = Candidate | Rest], {BestMetric, _})
  when CandidateMetric#metric.max_width < BestMetric#metric.max_width ->
    best_unfit(Rest, Candidate);
best_unfit([_Candidate | Rest], Best) ->
    best_unfit(Rest, Best);
best_unfit([], Best) -> Best.
