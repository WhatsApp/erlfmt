%   A set of functions for creating and manipulating algebra
%   documents.

%   This module implements the functionality described in
%   ["Strictly Pretty" (2000) by Christian Lindig][0] with small
%   additions, like support for binary nodes and a break mode that
%   maximises use of horizontal space.

%       iex> Inspect.Algebra.empty()
%       :doc_nil

%       iex> "foo"
%       "foo"

%   With the functions in this module, we can concatenate different
%   elements together and render them:

%       iex> doc = Inspect.Algebra.concat(Inspect.Algebra.empty(), "foo")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["foo"]

%   The functions `nest/2`, `space/2` and `line/2` help you put the
%   document together into a rigid structure. However, the document
%   algebra gets interesting when using functions like `glue/3` and
%   `group/1`. A glue inserts a break between two documents. A group
%   indicates a document that must fit the current line, otherwise
%   breaks are rendered as new lines. Let's glue two docs together
%   with a break, group it and then render it:

%       iex> doc = Inspect.Algebra.glue("a", " ", "b")
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 80)
%       ["a", " ", "b"]

%   Notice the break was represented as is, because we haven't reached
%   a line limit. Once we do, it is replaced by a newline:

%       iex> doc = Inspect.Algebra.glue(String.duplicate("a", 20), " ", "b")
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 10)
%       ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

%   This module uses the byte size to compute how much space there is
%   left. If your document contains strings, then those need to be
%   wrapped in `string/1`, which then relies on `String.length/1` to
%   precompute the document size.

%   Finally, this module also contains Elixir related functions, a bit
%   tied to Elixir formatting, such as `to_doc/2`.

%   ## Implementation details

%   The implementation of `Inspect.Algebra` is based on the Strictly Pretty
%   paper by [Lindig][0] which builds on top of previous pretty printing
%   algorithms but is tailored to strict languages, such as Elixir.
%   The core idea in the paper is the use of explicit document groups which
%   are rendered as flat (breaks as spaces) or as break (breaks as newlines).

%   This implementation provides two types of breaks: `:strict` and `:flex`.
%   When a group does not fit, all strict breaks are treated as newlines.
%   Flex breaks however are re-evaluated on every occurrence and may still
%   be rendered flat. See `break/1` and `flex_break/1` for more information.

%   This implementation also adds `force_unfit/1` and `next_break_fits/2` which
%   give more control over the document fitting.

%     [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

-module(erlfmt_algebra2).

-define(newline, <<"\n">>).

-export_type([doc/0]).

-export([
    empty/0,
    string/1,
    concat/1, concat/2,
    nest/2, nest/3,
    break/0, break/1,
    collapse_lines/1,
    next_break_fits/1, next_break_fits/2,
    force_unfit/1,
    flex_break/0, flex_break/1,
    flex_glue/2, flex_glue/3,
    glue/2, glue/3,
    group/1, group/2,
    space/2,
    line/0, line/2,
    fold_doc/2,
    format/2, format/3,
    fits/4,
    collapse/4,
    apply_nesting/3,
    indent/1,
    container_doc/3, container_doc/4
]).

% Functional interface to "doc" records

-record(doc_string, {
    string :: doc(),
    length :: non_neg_integer()
}).

-record(doc_cons, {
    left :: doc(),
    right :: doc()
}).

-record(doc_nest, {
    doc :: doc(),
    indent :: cursor | reset | non_neg_integer(),
    always_or_break :: always | break
}).

-record(doc_break, {
    break :: binary(),
    flex_or_strict :: flex | strict
}).

-record(doc_group, {
    group :: doc(),
    inherit_or_self :: inherit | self
}).

-record(doc_fits, {
    group :: doc(),
    enabled_or_disabled :: enabled | disabled
}).

-record(doc_force, {
    group :: doc()
}).

-record(doc_collapse, {
    count :: pos_integer()
}).

-opaque doc() :: binary() 
    | doc_line
    | doc_nil
    | #doc_break{}
    | #doc_collapse{}
    | #doc_cons{}
    | #doc_fits{}
    | #doc_force{}
    | #doc_group{}
    | #doc_nest{}
    | #doc_string{}
    .

-define(docs, [
    doc_break,
    doc_collapse,
    doc_color,
    doc_cons,
    doc_fits,
    doc_force,
    doc_group,
    doc_nest,
    doc_string
]).

-define(is_doc(Doc),
    (
        is_binary(Doc) orelse
        (Doc == doc_nil) orelse
        (Doc == doc_line) orelse
        is_record(Doc, doc_break) orelse
        is_record(Doc, doc_collapse) orelse
        is_record(Doc, doc_cons) orelse
        is_record(Doc, doc_fits) orelse
        is_record(Doc, doc_force) orelse
        is_record(Doc, doc_group) orelse
        is_record(Doc, doc_nest) orelse
        is_record(Doc, doc_string)
    )
).

%   Wraps `collection` in `left` and `right` according to limit and contents.

%   It uses the given `left` and `right` documents as surrounding and the
%   separator document `separator` to separate items in `docs`. If all entries
%   in the collection are simple documents (texts or strings), then this function
%   attempts to put as much as possible on the same line. If they are not simple,
%   only one entry is shown per line if they do not fit.

%   The limit in the given `inspect_opts` is respected and when reached this
%   function stops processing and outputs `"..."` instead.

%   ## Options

%     * `:separator` - the separator used between each doc
%     * `:break` - If `:strict`, always break between each element. If `:flex`,
%       breaks only when necessary. If `:maybe`, chooses `:flex` only if all
%       elements are text-based, otherwise is `:strict`

%   ## Examples

%       iex> inspect_opts = %Inspect.Opts{limit: :infinity}
%       iex> fun = fn i, _opts -> to_string(i) end
%       iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun)
%       iex> Inspect.Algebra.format(doc, 5) |> IO.iodata_to_binary()
%       "[1,\n 2,\n 3,\n 4,\n 5]"

%       iex> inspect_opts = %Inspect.Opts{limit: 3}
%       iex> fun = fn i, _opts -> to_string(i) end
%       iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun)
%       iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary()
%       "[1, 2, 3, ...]"

%       iex> inspect_opts = %Inspect.Opts{limit: 3}
%       iex> fun = fn i, _opts -> to_string(i) end
%       iex> opts = [separator: "!"]
%       iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun, opts)
%       iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary()
%       "[1! 2! 3! ...]"

-spec container_doc(doc(), [doc()], doc()) -> doc().
container_doc(Left, Collection, Right) ->
    container_doc(Left, Collection, Right, #{}).

-spec container_doc(doc(), [doc()], doc(), map()) -> doc().
container_doc(Left, [], Right, Opts) when 
    ?is_doc(Left), ?is_doc(Right), is_map(Opts)  ->
        concat(Left, Right);
container_doc(Left, Collection, Right, Opts) when 
    ?is_doc(Left), is_list(Collection), ?is_doc(Right), is_map(Opts)  ->
    Break = maps:get(break, Opts, maybe),
    Separator = maps:get(separator, Opts, <<",">>),
    {Docs0, Simple} = container_each(Collection, [], Break == maybe),
    Flex = Simple orelse Break == flex,
    Docs = fold_doc(Docs0, fun(L, R) -> join(L, R, Flex, Separator) end),
    case Flex of
        % TODO: 1 and 2 should probably not be constants
        true -> group(concat(concat(Left, nest(Docs, 1)), Right));
        false -> group(glue(nest(glue(Left, <<"">>, Docs), 2), <<"">>, Right))
    end.

%   @spec container_doc(t, [any], t, Inspect.Opts.doc(), (term, Inspect.Opts.doc() -> t), keyword()) ::
%           t
%   def container_doc(left, collection, right, inspect_opts, fun, opts \\ [])
%       when is_doc(left) and is_list(collection) and is_doc(right) and is_function(fun, 2) and
%              is_list(opts) do
%     case collection do
%       [] ->
%         concat(left, right)

%       _ ->
%         break = Keyword.get(opts, :break, :maybe)
%         separator = Keyword.get(opts, :separator, @container_separator)

%         {docs, simple?} =
%           container_each(collection, inspect_opts.limit, inspect_opts, fun, [], break == :maybe)

%         flex? = simple? or break == :flex
%         docs = fold_doc(docs, &join(&1, &2, flex?, separator))

%         case flex? do
%           true -> group(concat(concat(left, nest(docs, 1)), right))
%           false -> group(glue(nest(glue(left, "", docs), 2), "", right))
%         end
%     end
%   end

container_each([], Acc, Simple) ->
    {lists:reverse(Acc), Simple};
container_each([Doc | Docs], Acc, Simple) when is_list(Docs) ->
    container_each(Docs, [Doc | Acc], Simple andalso simple(Doc));
container_each([Left | Right], Acc, Simple0) ->
    Simple = Simple0 and simple(Left) and simple(Right),
    Doc = join(Left, Right, Simple, <<" |">>),
    {lists:reverse([Doc | Acc]), Simple}.

%   defp container_each([], _limit, _opts, _fun, acc, simple?) do
%     {:lists.reverse(acc), simple?}
%   end

%   defp container_each(_, 0, _opts, _fun, acc, simple?) do
%     {:lists.reverse(["..." | acc]), simple?}
%   end

%   defp container_each([term | terms], limit, opts, fun, acc, simple?) when is_list(terms) do
%     limit = decrement(limit)
%     doc = fun.(term, %{opts | limit: limit})
%     container_each(terms, limit, opts, fun, [doc | acc], simple? and simple?(doc))
%   end

%   defp container_each([left | right], limit, opts, fun, acc, simple?) do
%     limit = decrement(limit)
%     left = fun.(left, %{opts | limit: limit})
%     right = fun.(right, %{opts | limit: limit})
%     simple? = simple? and simple?(left) and simple?(right)

%     doc = join(left, right, simple?, @tail_separator)
%     {:lists.reverse([doc | acc]), simple?}
%   end

%   defp decrement(:infinity), do: :infinity
%   defp decrement(counter), do: counter - 1

join(Left, doc_nil, _, _) -> Left;
join(doc_nil, Right, _, _) -> Right;
join(Left, Right, true, Sep) -> flex_glue(concat(Left, Sep), Right);
join(Left, Right, false, Sep) -> glue(concat(Left, Sep), Right).

%   defp join(:doc_nil, :doc_nil, _, _), do: :doc_nil
%   defp join(left, :doc_nil, _, _), do: left
%   defp join(:doc_nil, right, _, _), do: right
%   defp join(left, right, true, sep), do: flex_glue(concat(left, sep), right)
%   defp join(left, right, false, sep), do: glue(concat(left, sep), right)

simple(#doc_cons{left = Left, right = Right}) -> simple(Left) andalso simple(Right);
simple(#doc_string{}) -> true;
simple(doc_nil) -> true;
simple(Other) -> is_binary(Other).

%   defp simple?(doc_cons(left, right)), do: simple?(left) and simple?(right)
%   defp simple?(doc_string(_, _)), do: true
%   defp simple?(:doc_nil), do: true
%   defp simple?(other), do: is_binary(other)

%%%%%%%%%%%%%%%

%   # Algebra API

%   @doc """
%   Returns a document entity used to represent nothingness.

%   ## Examples

%       iex> Inspect.Algebra.empty()
%       :doc_nil

%   """

-spec empty() -> doc().
empty() -> doc_nil.

%   Creates a document represented by string.

%   While `Inspect.Algebra` accepts binaries as documents,
%   those are counted by binary size. On the other hand,
%   `string` documents are measured in terms of graphemes
%   towards the document size.

%   ## Examples

%   The following document has 10 bytes and therefore it
%   does not format to width 9 without breaks:

%       iex> doc = Inspect.Algebra.glue("olá", " ", "mundo")
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 9)
%       ["olá", "\n", "mundo"]

%   However, if we use `string`, then the string length is
%   used, instead of byte size, correctly fitting:

%       iex> string = Inspect.Algebra.string("olá")
%       iex> doc = Inspect.Algebra.glue(string, " ", "mundo")
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 9)
%       ["olá", " ", "mundo"]

-spec string(binary()) -> doc().
string(String) when is_binary(String) ->
    #doc_string{string = String, length = string:length(String)}.

%   Concatenates two document entities returning a new document.

%   ## Examples

%       iex> doc = Inspect.Algebra.concat("hello", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", "world"]

-spec concat(doc(), doc()) -> doc().
concat(Left, Right) when ?is_doc(Left), ?is_doc(Right) ->
    #doc_cons{left = Left, right = Right}.

%   Concatenates a list of documents returning a new document.

%   ## Examples

%       iex> doc = Inspect.Algebra.concat(["a", "b", "c"])
%       iex> Inspect.Algebra.format(doc, 80)
%       ["a", "b", "c"]

-spec concat([doc()]) -> doc().
concat(Docs) when is_list(Docs) ->
    fold_doc(Docs, fun concat/2).

%   Nests the given document at the given `level`.

%   If `level` is an integer, that's the indentation appended
%   to line breaks whenever they occur. If the level is `:cursor`,
%   the current position of the "cursor" in the document becomes
%   the nesting. If the level is `:reset`, it is set back to 0.

%   `mode` can be `:always`, which means nesting always happen,
%   or `:break`, which means nesting only happens inside a group
%   that has been broken.

%   ## Examples

%       iex> doc = Inspect.Algebra.nest(Inspect.Algebra.glue("hello", "world"), 5)
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 5)
%       ["hello", "\n     ", "world"]

-define(is_indent(Indent), (
    Indent == reset orelse
    Indent == cursor orelse
    (is_integer(Indent) andalso Indent >= 0)
)).

-spec nest(doc(), non_neg_integer() | cursor | reset) -> doc().
nest(Doc, Level) ->
    nest(Doc, Level, always).

-spec nest(doc(), non_neg_integer() | cursor | reset, always | break) -> doc().
nest(Doc, 0, _Mode) when ?is_doc(Doc) ->
    Doc;
nest(Doc, Level, always) when ?is_doc(Doc), ?is_indent(Level)  ->
    #doc_nest{doc = Doc, indent = Level, always_or_break = always};
nest(Doc, Level, break) when ?is_doc(Doc), ?is_indent(Level)  ->
    #doc_nest{doc = Doc, indent = Level, always_or_break = break}.

%   Returns a break document based on the given `string`.

%   This break can be rendered as a linebreak or as the given `string`,
%   depending on the `mode` of the chosen layout.

%   ## Examples

%   Let's create a document by concatenating two strings with a break between
%   them:

%       iex> doc = Inspect.Algebra.concat(["a", Inspect.Algebra.break("\t"), "b"])
%       iex> Inspect.Algebra.format(doc, 80)
%       ["a", "\t", "b"]

%   Notice the break was represented with the given string, because we didn't
%   reach a line limit. Once we do, it is replaced by a newline:

%       iex> break = Inspect.Algebra.break("\t")
%       iex> doc = Inspect.Algebra.concat([String.duplicate("a", 20), break, "b"])
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> Inspect.Algebra.format(doc, 10)
%       ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

-spec break() -> doc().
break() ->
    break(<<" ">>).

-spec break(binary()) -> doc().
break(String) when is_binary(String) ->
    #doc_break{break = String, flex_or_strict = strict}.

%   Collapse any new lines and whitespace following this
%   node, emitting up to `max` new lines.
  
-spec collapse_lines(pos_integer()) -> doc().
collapse_lines(Max) when is_integer(Max) andalso Max > 0 ->
    #doc_collapse{count = Max}.

%   Considers the next break as fit.

%   `mode` can be `:enabled` or `:disabled`. When `:enabled`,
%   it will consider the document as fit as soon as it finds
%   the next break, effectively cancelling the break. It will
%   also ignore any `force_unfit/1` in search of the next break.

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

-define(is_next_break_mode(Mode), (
    Mode == enabled orelse
    Mode == disabled
)).

-spec next_break_fits(doc()) -> doc().
next_break_fits(Doc) ->
    next_break_fits(Doc, enabled).

-spec next_break_fits(doc(), enabled | disabled) -> doc().
next_break_fits(Doc, Mode) when ?is_doc(Doc) and ?is_next_break_mode(Mode) ->
    #doc_fits{group = Doc, enabled_or_disabled = Mode}.

%   Forces the current group to be unfit.

-spec force_unfit(doc()) -> doc().
force_unfit(Doc) when ?is_doc(Doc) ->
    #doc_force{group = Doc}.

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

%   This function is used by `container_doc/6` and friends to the
%   maximum number of entries on the same line.

-spec flex_break() -> doc().
flex_break() -> flex_break(<<" ">>).

-spec flex_break(binary()) -> doc().
flex_break(String) when is_binary(String) ->
    #doc_break{break = String, flex_or_strict = flex}.

%   Glues two documents (`doc1` and `doc2`) inserting a
%   `flex_break/1` given by `break_string` between them.

%   This function is used by `container_doc/6` and friends
%   to the maximum number of entries on the same line.

-spec flex_glue(doc(), doc()) -> doc().
flex_glue(Doc1, Doc2) ->
    flex_glue(Doc1, <<" ">>, Doc2).

-spec flex_glue(doc(), binary(), doc()) -> doc().
flex_glue(Doc1, BreakString, Doc2) when is_binary(BreakString) ->
    concat(Doc1, concat(flex_break(BreakString), Doc2)).

%   Glues two documents (`doc1` and `doc2`) inserting the given
%   break `break_string` between them.

%   For more information on how the break is inserted, see `break/1`.

%   ## Examples

%       iex> doc = Inspect.Algebra.glue("hello", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", " ", "world"]

%       iex> doc = Inspect.Algebra.glue("hello", "\t", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", "\t", "world"]

-spec glue(doc(), doc()) -> doc().
glue(Doc1, Doc2) ->
    glue(Doc1, <<" ">>, Doc2).

-spec glue(doc(), binary(), doc()) -> doc().
glue(Doc1, BreakString, Doc2) when is_binary(BreakString) ->
    concat(Doc1, concat(break(BreakString), Doc2)).

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
    group(Doc, self).

-spec group(doc(), self | inherit) -> doc().
group(Doc, Mode) when ?is_doc(Doc), Mode == self orelse Mode == inherit ->
    #doc_group{group = Doc, inherit_or_self = Mode}.

%   Inserts a mandatory single space between two documents.

%   ## Examples

%       iex> doc = Inspect.Algebra.space("Hughes", "Wadler")
%       iex> Inspect.Algebra.format(doc, 5)
%       ["Hughes", " ", "Wadler"]

-spec space(doc(), doc()) -> doc().
space(Doc1, Doc2) ->
    concat(Doc1, concat(<<" ">>, Doc2)).

%   A mandatory linebreak.

%   A group with linebreaks will fit if all lines in the group fit.

%   ## Examples

%       iex> doc =
%       ...>   Inspect.Algebra.concat(
%       ...>     Inspect.Algebra.concat(
%       ...>       "Hughes",
%       ...>       Inspect.Algebra.line()
%       ...>     ),
%       ...>     "Wadler"
%       ...>   )
%       iex> Inspect.Algebra.format(doc, 80)
%       ["Hughes", "\n", "Wadler"]

-spec line() -> doc().
line() -> doc_line.

%   Inserts a mandatory linebreak between two documents.

%   See `line/0`.

%   ## Examples

%       iex> doc = Inspect.Algebra.line("Hughes", "Wadler")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["Hughes", "\n", "Wadler"]

-spec line(doc(), doc()) -> doc().
line(Doc1, Doc2) -> concat(Doc1, concat(line(), Doc2)).

%   Folds a list of documents into a document using the given folder function.

%   The list of documents is folded "from the right"; in that, this function is
%   similar to `List.foldr/3`, except that it doesn't expect an initial
%   accumulator and uses the last element of `docs` as the initial accumulator.

%   ## Examples

%       iex> docs = ["A", "B", "C"]
%       iex> docs =
%       ...>   Inspect.Algebra.fold_doc(docs, fn doc, acc ->
%       ...>     Inspect.Algebra.concat([doc, "!", acc])
%       ...>   end)
%       iex> Inspect.Algebra.format(docs, 80)
%       ["A", "!", "B", "!", "C"]

-spec fold_doc([doc()], fun((doc(), doc()) -> doc())) -> doc().
fold_doc([], _Fun) ->
    empty();
fold_doc([Doc], _Fun) ->
    Doc;
fold_doc([Doc | Docs], Fun) ->
    Fun(Doc, fold_doc(Docs, Fun)).

%   Formats a given document for a given width.

%   Takes the maximum width and a document to print as its arguments
%   and returns an IO data representation of the best layout for the
%   document to fit in the given width.

%   The document starts flat (without breaks) until a group is found.

%   ## Examples

%       iex> doc = Inspect.Algebra.glue("hello", " ", "world")
%       iex> doc = Inspect.Algebra.group(doc)
%       iex> doc |> Inspect.Algebra.format(30) |> IO.iodata_to_binary()
%       "hello world"
%       iex> doc |> Inspect.Algebra.format(10) |> IO.iodata_to_binary()
%       "hello\nworld"

-spec format(doc(), non_neg_integer() | infinity) -> iodata().
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

-spec fits(integer(), integer(), boolean(), Entries) -> boolean()
    when Entries :: maybe_improper_list({integer(), mode(), doc()}, {tail, boolean(), Entries} | []).

% We need at least a break to consider the document does not fit since a
% large document without breaks has no option but fitting its current line.
%
% In case we have groups and the group fits, we need to consider the group
% parent without the child breaks, hence {:tail, b?, t} below.

fits(Width, K, B, _) when K > Width andalso B -> false;
fits(_, _, _, []) -> true;
fits(Width, K, _, {tail, B, Doc}) -> fits(Width, K, B, Doc);

%   ## Flat no break

fits(Width, K, B, [{I, _, #doc_fits{group = X, enabled_or_disabled = disabled}} | T]) ->
    fits(Width, K, B, [{I, flat_no_break, X} | T]);
fits(Width, K, B, [{I, flat_no_break, #doc_fits{group = X}} | T]) ->
    fits(Width, K, B, [{I, flat_no_break, X} | T]);

%   ## Breaks no flat

fits(Width, K, B, [{I, _, #doc_fits{group = X, enabled_or_disabled = enabled}} | T]) ->
    fits(Width, K, B, [{I, break_no_flat, X} | T]);
fits(Width, K, B, [{I, break_no_flat, #doc_force{group = X}} | T]) ->
    fits(Width, K, B, [{I, break_no_flat, X} | T]);
fits(_, _, _, [{_, break_no_flat, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break_no_flat, doc_line} | _]) ->
    true;

%   ## Breaks

fits(_, _, _, [{_, break, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break, doc_line} | _]) ->
    true;
fits(Width, K, B, [{I, break, #doc_group{group = X}} | T]) ->
    fits(Width, K, B, [{I, flat, X} | {tail, B, T}]);

%   ## Catch all

fits(Width, _, _, [{I, _, doc_line} | T]) ->
    fits(Width, I, false, T);
fits(Width, K, B, [{_, _, doc_nil} | T]) ->
    fits(Width, K, B, T);
fits(Width, _, B, [{I, _, #doc_collapse{}} | T]) ->
    fits(Width, I, B, T);
fits(Width, K, B, [{_, _, #doc_string{length = L}} | T]) ->
    fits(Width, K + L, B, T);
fits(Width, K, B, [{_, _, S} | T]) when is_binary(S) ->
    fits(Width, K + byte_size(S), B, T);
fits(_, _, _, [{_, _, #doc_force{}} | _]) ->
    false;
fits(Width, K, _, [{_, _, #doc_break{break = S}} | T]) ->
    fits(Width, K + byte_size(S), true, T);
fits(Width, K, B, [{I, M, #doc_nest{doc = X, always_or_break = break}} | T]) ->
    fits(Width, K, B, [{I, M, X} | T]);
fits(Width, K, B, [{I, M, #doc_nest{doc = X, indent = J}} | T]) ->
    fits(Width, K, B, [{apply_nesting(I, K, J), M, X} | T]);
fits(Width, K, B, [{I, M, #doc_cons{left = X, right = Y}} | T]) ->
    fits(Width, K, B, [{I, M, X}, {I, M, Y} | T]);
fits(Width, K, B, [{I, M, #doc_group{group = X}} | T]) ->
    fits(Width, K, B, [{I, M, X} | {tail, B, T}]).

-spec format(integer() | infinity, integer(), [{integer(), mode(), doc()}]) -> [binary()].
format(_, _, []) ->
    [];
format(Width, K, [{_, _, doc_nil} | T]) ->
    format(Width, K, T);
format(Width, _, [{I, _, doc_line} | T]) ->
    [indent(I) | format(Width, I, T)];
format(Width, K, [{I, M, #doc_cons{left = X, right = Y}} | T]) ->
    format(Width, K, [{I, M, X}, {I, M, Y} | T]);
format(Width, K, [{_, _, #doc_string{string = S, length = L}} | T]) ->
    [S | format(Width, K + L, T)];
format(Width, K, [{_, _, S} | T]) when is_binary(S) ->
    [S | format(Width, K + byte_size(S), T)];
format(Width, K, [{I, M, #doc_force{group = X}} | T]) ->
    format(Width, K, [{I, M, X} | T]);
format(Width, K, [{I, M, #doc_fits{group = X}} | T]) ->
    format(Width, K, [{I, M, X} | T]);
format(Width, _, [{I, _, #doc_collapse{count = Max}} | T]) ->
    collapse(format(Width, I, T), Max, 0, I);

%   # Flex breaks are not conditional to the mode
format(Width, K0, [{I, M, #doc_break{break = S, flex_or_strict = flex}} | T]) ->
    K = K0 + byte_size(S),
    case Width == infinity orelse M == flat orelse fits(Width, K, true, T) of
        true -> [S | format(Width, K, T)];
        false -> [indent(I) | format(Width, I, T)]
    end;

%   # Strict breaks are conditional to the mode
format(Width, K, [{I, M, #doc_break{break = S, flex_or_strict = strict}} | T]) ->
    case M of
        break -> [indent(I) | format(Width, I, T)];
        _ -> [S | format(Width, K + byte_size(S), T)]
    end;

%   # Nesting is conditional to the mode.
format(Width, K, [{I, M, #doc_nest{doc = X, indent = J, always_or_break = Nest}} | T]) ->
    case Nest == always orelse (Nest == break andalso M == break) of
        true -> format(Width, K, [{apply_nesting(I, K, J), M, X} | T]);
        false -> format(Width, K, [{I, M, X} | T])
    end;

%   # Groups must do the fitting decision.
format(Width, K, [{I, break, #doc_group{group = X, inherit_or_self = inherit}} | T]) ->
    format(Width, K, [{I, break, X} | T]);

format(Width, K, [{I, _, #doc_group{group = X}} | T]) ->
    case Width == infinity orelse fits(Width, K, false, [{I, flat, X}]) of
        true -> format(Width, K, [{I, flat, X} | T]);
        false -> format(Width, K, [{I, break, X} | T])
    end.

collapse([<<"\n", _/binary>> | T], Max, Count, I) ->
    collapse(T, Max, Count + 1, I);

collapse([<<"">> | T], Max, Count, I) ->
    collapse(T, Max, Count, I);

collapse(T, Max, Count, I) ->
    NewLines = binary:copy(<<"\n">>, min(Max, Count)),
    Spaces = binary:copy(<<" ">>, I),
    [<<NewLines/binary, Spaces/binary>> | T].

apply_nesting(_, K, cursor) -> K;
apply_nesting(_, _, reset) -> 0;
apply_nesting(I, _, J) -> I + J.

indent(0) -> ?newline;
indent(I) when is_integer(I) ->
    Spaces = binary:copy(<<" ">>, I),
    <<?newline/binary,Spaces/binary>>.
