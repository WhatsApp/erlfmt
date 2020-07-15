%% Copyright (c) 2012-2020 Plataformatec
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

%   A set of functions for creating and manipulating algebra
%   documents.

%   This module implements the functionality described in
%   ["Strictly Pretty" (2000) by Christian Lindig][0] with small
%   additions, like support for binary nodes and a break mode that
%   maximises use of horizontal space.

%   The functions `nest/2`, `space/2` and `line/2` help you put the
%   document together into a rigid structure. However, the document
%   algebra gets interesting when using functions like `break/3` and
%   `group/1`. A break inserts a break between two documents. A group
%   indicates a document that must fit the current line, otherwise
%   breaks are rendered as new lines.

%   ## Implementation details

%   The implementation of `Inspect.Algebra` is based on the Strictly Pretty
%   paper by [Lindig][0] which builds on top of previous pretty printing
%   algorithms but is tailored to strict languages, such as Erlang.
%   The core idea in the paper is the use of explicit document groups which
%   are rendered as flat (breaks as spaces) or as break (breaks as newlines).

%   This implementation provides two types of breaks: `strict` and `flex`.
%   When a group does not fit, all strict breaks are treated as newlines.
%   Flex breaks however are re-evaluated on every occurrence and may still
%   be rendered flat. See `break/1` and `flex_break/1` for more information.

%   This implementation also adds `force_breaks/0` and `next_break_fits/2` which
%   give more control over the document fitting.

%     [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

-module(erlfmt_algebra).

-dialyzer(no_improper_lists).

-define(newline, <<"\n">>).

-export_type([doc/0]).

-export([
    force_breaks/0,
    empty/0,
    string/1,
    concat/1,
    concat/2,
    concat/3,
    nest/2,
    nest/3,
    break/0,
    break/1,
    next_break_fits/1,
    next_break_fits/2,
    flex_break/0,
    flex_break/1,
    flex_break/2,
    flex_break/3,
    break/2,
    break/3,
    group/1,
    space/2,
    line/0,
    line/1,
    line/2,
    fold_doc/2,
    format/2,
    format/3,
    fits/4,
    indent/1
]).

% Functional interface to "doc" records

% doc_string represents Literal text, which is simply printed as is.
-record(doc_string, {
    string :: unicode:chardata(),
    length :: non_neg_integer()
}).

-record(doc_line, {
    count :: integer()
}).

-record(doc_cons, {
    left :: doc(),
    right :: doc()
}).

% In doc_nest, all breaks inside the field `doc` that are printed as newlines are followed by `indent` spaces.
% `always_or_break` was not part of the original paper.
% `always` means nesting always happen,
% `break` means nesting only happens inside a group that has been broken.
-record(doc_nest, {
    doc :: doc(),
    indent :: non_neg_integer(),
    always_or_break :: always | break
}).

% The decision for each group affects all the line breaks of a group at a whole but is made for subgroups individually.
% 1. Print every optional line break of the current group and all its subgroups as spaces. If
% the current group then fits completely into the remaining space of current line this is
% the layout of the group.
% 2. If the former fails every optional line break of the current group is printed as a newline.
% Subgroups and their line breaks, however, are considered individually as they are reached
% by the pretty printing process.
-record(doc_group, {
    group :: doc()
}).

-record(doc_break, {
    break :: binary(),
    flex_or_strict :: flex | strict
}).

-record(doc_fits, {
    group :: doc(),
    enabled_or_disabled :: enabled | disabled
}).

% The first six constructors are described in the original paper at "Figure 1: Six constructors for the doc data type".
% doc_break is added as part of the implementation in section 3.
% doc_fits and doc_force_breaks are newly added.
-type doc() ::
    binary() |
    doc_force_breaks |
    doc_nil |
    #doc_string{} |
    % doc_line should be thought of as a space character which may be replaced by a line break when necessary.
    #doc_line{} |
    #doc_cons{} |
    #doc_nest{} |
    #doc_group{} |
    #doc_break{} |
    #doc_fits{}.

-define(is_doc(Doc),
    ((Doc == doc_force_breaks) orelse
        (Doc == doc_nil) orelse
        is_binary(Doc) orelse
        is_record(Doc, doc_line) orelse
        is_record(Doc, doc_break) orelse
        is_record(Doc, doc_cons) orelse
        is_record(Doc, doc_fits) orelse
        is_record(Doc, doc_group) orelse
        is_record(Doc, doc_nest) orelse
        is_record(Doc, doc_string))
).

% empty is not printed at all, but it is essential to implement optional output: if ... then "output" else empty;
% empty is mapped to the empty string by the pretty printer.
-spec empty() -> doc().
empty() -> doc_nil.

% string documents are measured in terms of graphemes towards the document size.
-spec string(unicode:chardata()) -> doc().
string(String) ->
    #doc_string{string = String, length = string:length(String)}.

% Concatenates two document entities returning a new document.
-spec concat(doc(), doc()) -> doc().
concat(Left, Right) when is_binary(Left), is_binary(Right) ->
    #doc_string{string = [Left | Right], length = byte_size(Left) + byte_size(Right)};
concat(#doc_string{string = String, length = Length}, Right) when is_binary(Right) ->
    #doc_string{string = [String | Right], length = Length + byte_size(Right)};
concat(Left, #doc_string{string = String, length = Length}) when is_binary(Left) ->
    #doc_string{string = [Left | String], length = Length + byte_size(Left)};
concat(#doc_string{} = Left, #doc_string{} = Right) ->
    #doc_string{
        string = [Left#doc_string.string | Right#doc_string.string],
        length = Left#doc_string.length + Right#doc_string.length
    };
concat(Left, Right) when ?is_doc(Left), ?is_doc(Right) ->
    #doc_cons{left = Left, right = Right}.

% Concatenates a list of documents returning a new document.
-spec concat([doc()]) -> doc().
concat(Docs) when is_list(Docs) ->
    fold_doc(fun concat/2, Docs).

% Concatenates three document entities returning a new document.
-spec concat(doc(), doc(), doc()) -> doc().
concat(A, B, C) when ?is_doc(A), ?is_doc(B), ?is_doc(C) ->
    concat(A, concat(B, C)).

% Nests the given document at the given `level`.
-spec nest(doc(), non_neg_integer()) -> doc().
nest(Doc, Level) ->
    nest(Doc, Level, always).

-spec nest(doc(), non_neg_integer(), always | break) -> doc().
nest(Doc, 0, _Mode) when ?is_doc(Doc) ->
    Doc;
nest(Doc, Indent, always) when ?is_doc(Doc), is_integer(Indent) andalso Indent >= 0 ->
    #doc_nest{doc = Doc, indent = Indent, always_or_break = always};
nest(Doc, Indent, break) when ?is_doc(Doc), is_integer(Indent) andalso Indent >= 0 ->
    #doc_nest{doc = Doc, indent = Indent, always_or_break = break}.

% This break can be rendered as a linebreak or as the given `string`, depending on the `mode` or line limit of the chosen layout.
-spec break() -> doc().
break() ->
    break(<<" ">>).

-spec break(binary()) -> doc().
break(String) when is_binary(String) ->
    #doc_break{break = String, flex_or_strict = strict}.

%   Considers the next break as fit.

%   `mode` can be `:enabled` or `:disabled`. When `:enabled`,
%   it will consider the document as fit as soon as it finds
%   the next break, effectively cancelling the break. It will
%   also ignore any `force_breaks/0` in search of the next break.

%   When disabled, it behaves as usual and it will ignore
%   any further `next_break_fits/2` instruction.

%   ## Examples

%   This is used by Elixir's code formatter to avoid breaking
%   code at some specific locations. For example, consider this
%   code:

%       some_function_call(%{..., key: value, ...})

%   Now imagine that this code does not fit its line. The code
%   formatter introduces breaks inside `(` and `)` and inside
%   `%{` and `}`. Therefore the document would break as:

%       some_function_call(
%         %{
%           ...,
%           key: value,
%           ...
%         }
%       )

%   The formatter wraps the algebra document representing the
%   map in `next_break_fits/1` so the code is formatted as:

%       some_function_call(%{
%         ...,
%         key: value,
%         ...
%       })

-spec next_break_fits(doc()) -> doc().
next_break_fits(Doc) ->
    next_break_fits(Doc, enabled).

-spec next_break_fits(doc(), enabled | disabled) -> doc().
next_break_fits(Doc, Mode) when ?is_doc(Doc), Mode == enabled orelse Mode == disabled ->
    #doc_fits{group = Doc, enabled_or_disabled = Mode}.

% Forces the parent group and its parent groups to break.
-spec force_breaks() -> doc().
force_breaks() ->
    doc_force_breaks.

%   Returns a flex break document based on the given `string`.

%   A flex break still causes a group to break, like `break/1`,
%   but it is re-evaluated when the documented is rendered.

%   For example, take a group document represented as `[1, 2, 3]`
%   where the space after every comma is a break. When the document
%   above does not fit a single line, all breaks are enabled,
%   causing the document to be rendered as:

%       [1,
%        2,
%        3]

%   However, if flex breaks are used, then each break is re-evaluated
%   when rendered, so the document could be possible rendered as:

%       [1, 2,
%        3]

%   Hence the name "flex". they are more flexible when it comes
%   to the document fitting. On the other hand, they are more expensive
%   since each break needs to be re-evaluated.

-spec flex_break() -> doc().
flex_break() -> flex_break(<<" ">>).

-spec flex_break(binary()) -> doc().
flex_break(String) when is_binary(String) ->
    #doc_break{break = String, flex_or_strict = flex}.

%   Breaks two documents (`doc1` and `doc2`) inserting a
%   `flex_break/1` given by `break_string` between them.

-spec flex_break(doc(), doc()) -> doc().
flex_break(Doc1, Doc2) ->
    flex_break(Doc1, <<" ">>, Doc2).

-spec flex_break(doc(), binary(), doc()) -> doc().
flex_break(Doc1, BreakString, Doc2) when is_binary(BreakString) ->
    concat(Doc1, flex_break(BreakString), Doc2).

%   Breaks two documents (`doc1` and `doc2`) inserting the given
%   break `break_string` between them.

%   For more information on how the break is inserted, see `break/1`.

%   ## Examples

%       iex> doc = Inspect.Algebra.break("hello", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", " ", "world"]

%       iex> doc = Inspect.Algebra.break("hello", "\t", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", "\t", "world"]

-spec break(doc(), doc()) -> doc().
break(Doc1, Doc2) ->
    break(Doc1, <<" ">>, Doc2).

-spec break(doc(), binary(), doc()) -> doc().
break(Doc1, BreakString, Doc2) when is_binary(BreakString) ->
    concat(Doc1, break(BreakString), Doc2).

%   Returns a group containing the specified document `doc`.

%   Documents in a group are attempted to be rendered together
%   to the best of the renderer ability.

%   The group mode can also be set to `:inherit`, which means it
%   automatically breaks if the parent group has broken too.

%   ## Examples

%       iex> doc =
%       ...>   Inspect.Algebra.group(
%       ...>     Inspect.Algebra.concat(
%       ...>       Inspect.Algebra.group(
%       ...>         Inspect.Algebra.concat(
%       ...>           "Hello,",
%       ...>           Inspect.Algebra.concat(
%       ...>             Inspect.Algebra.break(),
%       ...>             "A"
%       ...>           )
%       ...>         )
%       ...>       ),
%       ...>       Inspect.Algebra.concat(
%       ...>         Inspect.Algebra.break(),
%       ...>         "B"
%       ...>       )
%       ...>     )
%       ...>   )
%       iex> Inspect.Algebra.format(doc, 80)
%       ["Hello,", " ", "A", " ", "B"]
%       iex> Inspect.Algebra.format(doc, 6)
%       ["Hello,", "\n", "A", "\n", "B"]

-spec group(doc()) -> doc().
group(Doc) ->
    #doc_group{group = Doc}.

%   Inserts a mandatory single space between two documents.

%   ## Examples

%       iex> doc = Inspect.Algebra.space("Hughes", "Wadler")
%       iex> Inspect.Algebra.format(doc, 5)
%       ["Hughes", " ", "Wadler"]

-spec space(doc(), doc()) -> doc().
space(Doc1, Doc2) ->
    concat(Doc1, <<" ">>, Doc2).

% A mandatory linebreak, but in the paper doc_line was described as optional? (is this mandatory or optional in this implementation)
% A group with linebreaks will fit if all lines in the group fit.
-spec line() -> doc().
line() -> #doc_line{count = 1}.

-spec line(pos_integer()) -> doc().
line(Count) when is_integer(Count), Count > 0 -> #doc_line{count = Count}.

% Inserts a mandatory linebreak between two documents.
-spec line(doc(), doc()) -> doc().
line(Doc1, Doc2) -> concat(Doc1, line(), Doc2).

%   Folds a list of documents into a document using the given folder function.
%   The list of documents is folded "from the right"; in that, this function is
%   similar to `List.foldr/3`, except that it doesn't expect an initial
%   accumulator and uses the last element of `docs` as the initial accumulator.
%   Example:
%   ```
%   Docs = ["A", "B", "C"],
%   FoldedDocs = fold_doc(fun(Doc, Acc) -> concat([Doc, "!", Acc]) end, Docs),
%   io:format("~p", [FoldedDocs]).
%   ```
%   ["A", "!", "B", "!", "C"]
-spec fold_doc(fun((doc(), doc()) -> doc()), [doc()]) -> doc().
fold_doc(_Fun, []) ->
    empty();
fold_doc(_Fun, [Doc]) ->
    Doc;
fold_doc(Fun, [Doc | Docs]) ->
    Fun(Doc, fold_doc(Fun, Docs)).

% Formats a given document for a given width.
% Takes the maximum width and a document to print as its arguments
% and returns an string representation of the best layout for the
% document to fit in the given width.
% The document starts flat (without breaks) until a group is found.
-spec format(doc(), non_neg_integer() | infinity) -> unicode:chardata().
format(Doc, Width) when ?is_doc(Doc) andalso (Width == infinity orelse Width >= 0) ->
    format(Width, 0, [{0, flat, Doc}]).

%   Type representing the document mode to be rendered
%
%     * flat - represents a document with breaks as flats (a break may fit, as it may break)
%     * break - represents a document with breaks as breaks (a break always fits, since it breaks)
%
%   The following modes are exclusive to fitting
%
%     * flat_no_break - represents a document with breaks as flat not allowed to enter in break mode
%     * break_no_flat - represents a document with breaks as breaks not allowed to enter in flat mode

-type mode() :: flat | flat_no_break | break | break_no_flat.

-spec fits(Width :: integer(), Column :: integer(), HasBreaks :: boolean(), Entries) ->
    boolean()
when
    Entries :: maybe_improper_list(
        {integer(), mode(), doc()},
        {tail, boolean(), Entries} | []
    ).
% We need at least a break to consider the document does not fit since a
% large document without breaks has no option but fitting its current line.
%
% In case we have groups and the group fits, we need to consider the group
% parent without the child breaks, hence {:tail, b?, t} below.

fits(Width, Column, HasBreaks, _) when Column > Width andalso HasBreaks ->
    false;
fits(_, _, _, []) ->
    true;
fits(Width, Column, _, {tail, HasBreaks, Entries}) ->
    fits(Width, Column, HasBreaks, Entries);
%   ## Flat no break

fits(Width, Column, HasBreaks, [{Indent, _, #doc_fits{group = X, enabled_or_disabled = disabled}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, flat_no_break, X} | T]);
fits(Width, Column, HasBreaks, [{Indent, flat_no_break, #doc_fits{group = X}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, flat_no_break, X} | T]);
%   ## Breaks no flat

fits(Width, Column, HasBreaks, [{Indent, _, #doc_fits{group = X, enabled_or_disabled = enabled}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, break_no_flat, X} | T]);
fits(Width, Column, HasBreaks, [{_Indent, break_no_flat, doc_force_breaks} | T]) ->
    fits(Width, Column, HasBreaks, T);
fits(_, _, _, [{_, break_no_flat, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break_no_flat, #doc_line{}} | _]) ->
    true;
%   ## Breaks

fits(_, _, _, [{_, break, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break, #doc_line{}} | _]) ->
    true;
fits(Width, Column, HasBreaks, [{Indent, break, #doc_group{group = X}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, flat, X} | {tail, HasBreaks, T}]);
%   ## Catch all

fits(Width, _, _, [{Indent, _, #doc_line{}} | T]) ->
    fits(Width, Indent, false, T);
fits(Width, Column, HasBreaks, [{_, _, doc_nil} | T]) ->
    fits(Width, Column, HasBreaks, T);
fits(Width, Column, HasBreaks, [{_, _, #doc_string{length = L}} | T]) ->
    fits(Width, Column + L, HasBreaks, T);
fits(Width, Column, HasBreaks, [{_, _, S} | T]) when is_binary(S) ->
    fits(Width, Column + byte_size(S), HasBreaks, T);
fits(_, _, _, [{_, _, doc_force_breaks} | _]) ->
    false;
fits(Width, Column, _, [{_, _, #doc_break{break = S}} | T]) ->
    fits(Width, Column + byte_size(S), true, T);
fits(Width, Column, HasBreaks, [{Indent, M, #doc_nest{doc = X, always_or_break = break}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, M, X} | T]);
fits(Width, Column, HasBreaks, [{Indent, M, #doc_nest{doc = X, indent = J}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent + J, M, X} | T]);
fits(Width, Column, HasBreaks, [{Indent, M, #doc_cons{left = X, right = Y}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, M, X}, {Indent, M, Y} | T]);
fits(Width, Column, HasBreaks, [{Indent, M, #doc_group{group = X}} | T]) ->
    fits(Width, Column, HasBreaks, [{Indent, M, X} | {tail, HasBreaks, T}]).

-spec format(integer() | infinity, integer(), [{integer(), mode(), doc()}]) -> [binary()].
format(_, _, []) ->
    [];
format(Width, Column, [{_, _, doc_nil} | T]) ->
    format(Width, Column, T);
format(Width, _, [{Indent, _, #doc_line{count = Count}} | T]) ->
    NewLines = binary:copy(<<"\n">>, Count - 1),
    [NewLines, indent(Indent) | format(Width, Indent, T)];
format(Width, Column, [{Indent, M, #doc_cons{left = X, right = Y}} | T]) ->
    format(Width, Column, [{Indent, M, X}, {Indent, M, Y} | T]);
format(Width, Column, [{_, _, #doc_string{string = S, length = L}} | T]) ->
    [S | format(Width, Column + L, T)];
format(Width, Column, [{_, _, S} | T]) when is_binary(S) ->
    [S | format(Width, Column + byte_size(S), T)];
format(Width, Column, [{_Indent, _M, doc_force_breaks} | T]) ->
    format(Width, Column, T);
format(Width, Column, [{Indent, M, #doc_fits{group = X}} | T]) ->
    format(Width, Column, [{Indent, M, X} | T]);
%   # Flex breaks are not conditional to the mode
format(Width, K0, [{Indent, M, #doc_break{break = S, flex_or_strict = flex}} | T]) ->
    Column = K0 + byte_size(S),
    case Width == infinity orelse M == flat orelse fits(Width, Column, true, T) of
        true -> [S | format(Width, Column, T)];
        false -> [indent(Indent) | format(Width, Indent, T)]
    end;
%   # Strict breaks are conditional to the mode
format(Width, Column, [{Indent, M, #doc_break{break = S, flex_or_strict = strict}} | T]) ->
    case M of
        break -> [indent(Indent) | format(Width, Indent, T)];
        _ -> [S | format(Width, Column + byte_size(S), T)]
    end;
%   # Nesting is conditional to the mode.
format(Width, Column, [{Indent, M, #doc_nest{doc = X, indent = J, always_or_break = Nest}} | T]) ->
    case Nest == always orelse (Nest == break andalso M == break) of
        true -> format(Width, Column, [{Indent + J, M, X} | T]);
        false -> format(Width, Column, [{Indent, M, X} | T])
    end;
%   # Groups must do the fitting decision.
format(Width, Column, [{Indent, _, #doc_group{group = X}} | T0]) ->
    {StringLength, T1} = peek_next_string_length(T0),
    case Width == infinity orelse fits(Width - StringLength, Column, false, [{Indent, flat, X}]) of
        true ->
            format(Width, Column, [{Indent, flat, X} | T1]);
        false ->
            T = force_next_flex_break(T1),
            format(Width, Column, [{Indent, break, X} | T])
    end.

peek_next_string_length([{Indent, M, #doc_cons{left = Left, right = Right}} | T]) ->
    peek_next_string_length([{Indent, M, Left}, {Indent, M, Right} | T]);
peek_next_string_length([{_, _, #doc_string{length = Length}} | _] = Stack) ->
    {Length, Stack};
peek_next_string_length([{_, _, Binary} | _] = Stack) when is_binary(Binary) ->
    {byte_size(Binary), Stack};
peek_next_string_length(Stack) ->
    {0, Stack}.

%% after a group breaks, we force next flex break to also break
force_next_flex_break([{Indent, M, #doc_break{flex_or_strict = flex} = Break} | T]) ->
    [{Indent, M, Break#doc_break{flex_or_strict = strict}} | T];
force_next_flex_break([{_, _, #doc_break{flex_or_strict = strict}} | _] = Stack) ->
    Stack;
force_next_flex_break([{Indent, M, #doc_cons{left = Left, right = Right}} | T]) ->
    force_next_flex_break([{Indent, M, Left}, {Indent, M, Right} | T]);
force_next_flex_break([Other | T]) ->
    [Other | force_next_flex_break(T)];
force_next_flex_break([]) ->
    [].

indent(0) ->
    ?newline;
indent(Indent) when is_integer(Indent) ->
    Spaces = binary:copy(<<" ">>, Indent),
    <<?newline/binary, Spaces/binary>>.
