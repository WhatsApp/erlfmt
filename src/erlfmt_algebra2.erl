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

-define(container_separator, <<",">>).
-define(tail_separator, <<" |">>).
-define(newline, <<"\n">>).

% Functional interface to "doc" records

-record(doc_string, {
    string :: t(),
    length :: non_neg_integer()
}).

-record(doc_cons, {
    left :: t(),
    right :: t()
}).

-record(doc_nest, {
    doc :: t(),
    indent :: cursor | reset | non_neg_integer(),
    always_or_break :: always | break
}).

-record(doc_break, {
    break :: binary(),
    mode :: flex | strict
}).

-record(doc_group, {
    group :: t(),
    mode :: inherit | self
}).

-record(doc_fits, {
    group :: t(),
    mode :: enabled | disabled
}).

-record(doc_force, {
    group :: t()
}).

-record(doc_collapse, {
    count :: pos_integer()
}).

-opaque t() :: binary() 
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


%%%%%%%%%%%%%%%%%% TODO
%   @spec container_doc(t, [any], t, Inspect.Opts.t(), (term, Inspect.Opts.t() -> t), keyword()) ::
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

%   defp join(:doc_nil, :doc_nil, _, _), do: :doc_nil
%   defp join(left, :doc_nil, _, _), do: left
%   defp join(:doc_nil, right, _, _), do: right
%   defp join(left, right, true, sep), do: flex_glue(concat(left, sep), right)
%   defp join(left, right, false, sep), do: glue(concat(left, sep), right)

%   defp simple?(doc_cons(left, right)), do: simple?(left) and simple?(right)
%   defp simple?(doc_color(doc, _)), do: simple?(doc)
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

-spec empty() -> t().
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

-spec string(binary()) -> t().
string(String) when is_binary(String) ->
    #doc_string{string = String, length = string:length(String)}.

%   Concatenates two document entities returning a new document.

%   ## Examples

%       iex> doc = Inspect.Algebra.concat("hello", "world")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["hello", "world"]

-spec concat(t(), t()) -> t().
concat(Left, Right) when ?is_doc(Left), ?is_doc(Right) ->
    #doc_cons{left = Left, right = Right}.

%   Concatenates a list of documents returning a new document.

%   ## Examples

%       iex> doc = Inspect.Algebra.concat(["a", "b", "c"])
%       iex> Inspect.Algebra.format(doc, 80)
%       ["a", "b", "c"]

-spec concat([t()]) -> t().
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

-define(is_nest_mode(Mode), (
    (Mode == always) orelse
    (Mode == break)
)).

-define(is_indent(Indent), (
    Indent == reset orelse
    Indent == always orelse
    (is_integer(Indent) andalso Indent >= 0)
)).

-spec nest(t(), non_neg_integer() | cursor | reset) -> t().
nest(Doc, Level) ->
    nest(Doc, Level, always).

-spec nest(t(), non_neg_integer() | cursor | reset, always | break) -> t().
nest(Doc, 0, Mode) when ?is_doc(Doc), ?is_nest_mode(Mode) ->
    Doc;
nest(Doc, Level, Mode) when ?is_doc(Doc), ?is_indent(Level), ?is_nest_mode(Mode)  ->
    #doc_nest{doc = Doc, indent = Level, always_or_break = mode}.

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

-spec break() -> t().
break() ->
    #doc_break{break = <<"">>, mode = strict}.

-spec break(binary()) -> t().
break(String) when is_binary(String) ->
    #doc_break{break = String, mode = strict}.

%   Collapse any new lines and whitespace following this
%   node, emitting up to `max` new lines.
  
-spec collapse_lines(pos_integer()) -> t().
collapse_lines(Max) when is_integer(Max) and Max > 0 ->
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

-spec next_break_fits(t()) -> t().
next_break_fits(Doc) ->
    next_break_fits(Doc, enabled).

-spec next_break_fits(t(), enabled | disabled) -> t().
next_break_fits(Doc, Mode) when ?is_doc(Doc) and ?is_next_break_mode(Mode) ->
    #doc_fits{group = Doc, mode = Mode}.

%   Forces the current group to be unfit.

-spec force_unfit(t()) -> t().
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

-spec flex_break() -> t().
flex_break() -> flex_break(<<" ">>).

-spec flex_break(binary()) -> t().
flex_break(String) when is_binary(String) ->
    #doc_break{break = String, mode = flex}.

%   Glues two documents (`doc1` and `doc2`) inserting a
%   `flex_break/1` given by `break_string` between them.

%   This function is used by `container_doc/6` and friends
%   to the maximum number of entries on the same line.

-spec flex_glue(t(), t()) -> t().
flex_glue(Doc1, Doc2) ->
    flex_glue(Doc1, <<" ">>, Doc2).

-spec flex_glue(t(), binary(), t()) -> t().
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

-spec glue(t(), t()) -> t().
glue(Doc1, Doc2) ->
    glue(Doc1, <<" ">>, Doc2).

-spec glue(t(), binary(), t()) -> t().
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

-spec group(t()) -> t().
group(Doc) ->
    group(Doc, self).

-spec group(t(), self | inherit) -> t().
group(Doc, Mode) when ?is_doc(Doc), Mode == self orelse Mode == inherit ->
    #doc_group{group = Doc, mode = Mode}.

%   Inserts a mandatory single space between two documents.

%   ## Examples

%       iex> doc = Inspect.Algebra.space("Hughes", "Wadler")
%       iex> Inspect.Algebra.format(doc, 5)
%       ["Hughes", " ", "Wadler"]

-spec space(t(), t()) -> t().
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

-spec line() -> t().
line() -> doc_line.

%   Inserts a mandatory linebreak between two documents.

%   See `line/0`.

%   ## Examples

%       iex> doc = Inspect.Algebra.line("Hughes", "Wadler")
%       iex> Inspect.Algebra.format(doc, 80)
%       ["Hughes", "\n", "Wadler"]

-spec line(t(), t()) -> t().
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

-spec fold_doc([t()], fun((t(), t()) -> t())) -> t().
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

-spec format(t(), non_neg_integer() | infinity) -> iodata().
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
    when Entries :: maybe_improper_list({integer(), mode(), t()}, {tail, boolean(), Entries} | []).

% We need at least a break to consider the document does not fit since a
% large document without breaks has no option but fitting its current line.
%
% In case we have groups and the group fits, we need to consider the group
% parent without the child breaks, hence {:tail, b?, t} below.

fits(W, K, B, _) when K > W andalso B -> false;
fits(_, _, _, []) -> true;
fits(W, K, _, {tail, B, T}) -> fits(W, K, B, T);

%   defp fits?(w, k, b?, _) when k > w and b?, do: false
%   defp fits?(_, _, _, []), do: true
%   defp fits?(w, k, _, {:tail, b?, t}), do: fits?(w, k, b?, t)

%   ## Flat no break

fits(W, K, B, [{I, _, #doc_fits{group = X, mode = disabled}} | T]) ->
    fits(W, K, B, [{I, flat_no_break, X} | T]);

%   defp fits?(w, k, b?, [{i, _, doc_fits(x, :disabled)} | t]),
%     do: fits?(w, k, b?, [{i, :flat_no_break, x} | t])

fits(W, K, B, [{I, flat_no_break, #doc_fits{group = X}} | T]) ->
    fits(W, K, B, [{I, flat_no_break, X} | T]);

%   defp fits?(w, k, b?, [{i, :flat_no_break, doc_fits(x, _)} | t]),
%     do: fits?(w, k, b?, [{i, :flat_no_break, x} | t])

%   ## Breaks no flat

fits(W, K, B, [{I, _, #doc_fits{group = X, mode = enabled}} | T]) ->
    fits(W, K, B, [{I, break_no_flat, X} | T]);

%   defp fits?(w, k, b?, [{i, _, doc_fits(x, :enabled)} | t]),
%     do: fits?(w, k, b?, [{i, :break_no_flat, x} | t])

fits(W, K, B, [{I, break_no_flat, #doc_force{group = X}} | T]) ->
    fits(W, K, B, [{I, break_no_flat, X} | T]);

%   defp fits?(w, k, b?, [{i, :break_no_flat, doc_force(x)} | t]),
%     do: fits?(w, k, b?, [{i, :break_no_flat, x} | t])

fits(_, _, _, [{_, break_no_flat, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break_no_flat, doc_line} | _]) ->
    true;

%   defp fits?(_, _, _, [{_, :break_no_flat, doc_break(_, _)} | _]), do: true
%   defp fits?(_, _, _, [{_, :break_no_flat, :doc_line} | _]), do: true

%   ## Breaks

fits(_, _, _, [{_, break, #doc_break{}} | _]) ->
    true;
fits(_, _, _, [{_, break, doc_line} | _]) ->
    true;

%   defp fits?(_, _, _, [{_, :break, doc_break(_, _)} | _]), do: true
%   defp fits?(_, _, _, [{_, :break, :doc_line} | _]), do: true

fits(W, K, B, [{I, break, #doc_group{group = X}} | T]) ->
    fits(W, K, B, [{I, flat, X} | {tail, B, T}]);

%   defp fits?(w, k, b?, [{i, :break, doc_group(x, _)} | t]),
%     do: fits?(w, k, b?, [{i, :flat, x} | {:tail, b?, t}])

%   ## Catch all

fits(W, _, _, [{I, _, doc_line} | T]) ->
    fits(W, I, false, T);
%   defp fits?(w, _, _, [{i, _, :doc_line} | t]), do: fits?(w, i, false, t)
fits(W, K, B, [{_, _, doc_nil} | T]) ->
    fits(W, K, B, T);
%   defp fits?(w, k, b?, [{_, _, :doc_nil} | t]), do: fits?(w, k, b?, t)
fits(W, _, B, [{I, _, #doc_collapse{}} | T]) ->
    fits(W, I, B, T);
%   defp fits?(w, _, b?, [{i, _, doc_collapse(_)} | t]), do: fits?(w, i, b?, t)
fits(W, K, B, [{_, _, #doc_string{length = L}} | T]) ->
    fits(W, K + L, B, T);
%   defp fits?(w, k, b?, [{_, _, doc_string(_, l)} | t]), do: fits?(w, k + l, b?, t)
fits(W, K, B, [{_, _, S} | T]) when is_binary(S) ->
    fits(W, K + byte_size(S), B, T);
%   defp fits?(w, k, b?, [{_, _, s} | t]) when is_binary(s), do: fits?(w, k + byte_size(s), b?, t)
fits(_, _, _, [{_, _, #doc_force{}} | _]) ->
    false;
%   defp fits?(_, _, _, [{_, _, doc_force(_)} | _]), do: false
fits(W, K, _, [{_, _, #doc_break{break = S}} | T]) ->
    fits(W, K + byte_size(S), true, T);
%   defp fits?(w, k, _, [{_, _, doc_break(s, _)} | t]), do: fits?(w, k + byte_size(s), true, t)
fits(W, K, B, [{I, M, #doc_nest{doc = X, always_or_break = break}} | T]) ->
    fits(W, K, B, [{I, M, X} | T]);
%   defp fits?(w, k, b?, [{i, m, doc_nest(x, _, :break)} | t]), do: fits?(w, k, b?, [{i, m, x} | t])
fits(W, K, B, [{I, M, #doc_nest{doc = X, indent = J}} | T]) ->
    fits(W, K, B, [{apply_nesting(I, K, J), M, X} | T]);
%   defp fits?(w, k, b?, [{i, m, doc_nest(x, j, _)} | t]),
%     do: fits?(w, k, b?, [{apply_nesting(i, k, j), m, x} | t])
fits(W, K, B, [{I, M, #doc_cons{left = X, right = Y}} | T]) ->
    fits(W, K, B, [{I, M, X}, {I, M, Y} | T]);
%   defp fits?(w, k, b?, [{i, m, doc_cons(x, y)} | t]),
%     do: fits?(w, k, b?, [{i, m, x}, {i, m, y} | t])
fits(W, K, B, [{I, M, #doc_group{group = X}} | T]) ->
    fits(W, K, B, [{I, M, X} | {tail, B, T}]).

-spec format(integer() | infinity, integer(), [{integer(), mode(), t()}]) -> [binary()].
%   @spec format(integer | :infinity, integer, [{integer, mode, t}]) :: [binary]
format(_, _, []) -> 
    [];
%   defp format(_, _, []), do: []
format(W, K, [{_, _, doc_nil} | T]) ->
    format(W, K, T);
%   defp format(w, k, [{_, _, :doc_nil} | t]), do: format(w, k, t)
format(W, _, [{I, _, doc_line} | T]) ->
    [indent(I) | format(W, I, T)];
%   defp format(w, _, [{i, _, :doc_line} | t]), do: [indent(i) | format(w, i, t)]
format(W, K, [{I, M, #doc_cons{left = X, right = Y}} | T]) ->
    format(W, K, [{I, M, X}, {I, M, Y} | T]);
%   defp format(w, k, [{i, m, doc_cons(x, y)} | t]), do: format(w, k, [{i, m, x}, {i, m, y} | t])
format(W, K, [{_, _, #doc_string{string = S, length = L}} | T]) ->
    [S | format(W, K + L, T)];
%   defp format(w, k, [{_, _, doc_string(s, l)} | t]), do: [s | format(w, k + l, t)]
format(W, K, [{_, _, S} | T]) when is_binary(S) ->
    [S | format(W, K + byte_size(S), T)];
%   defp format(w, k, [{_, _, s} | t]) when is_binary(s), do: [s | format(w, k + byte_size(s), t)]
format(W, K, [{I, M, #doc_force{group = X}} | T]) ->
    format(W, K, [{I, M, X} | T]);
%   defp format(w, k, [{i, m, doc_force(x)} | t]), do: format(w, k, [{i, m, x} | t])
format(W, K, [{I, M, #doc_fits{group = X}} | T]) ->
    format(W, K, [{I, M, X} | T]);
%   defp format(w, k, [{i, m, doc_fits(x, _)} | t]), do: format(w, k, [{i, m, x} | t])
format(W, _, [{I, _, #doc_collapse{count = Max}} | T]) ->
    collapse(format(W, I, T), Max, 0, I);
%   defp format(w, _, [{i, _, doc_collapse(max)} | t]), do: collapse(format(w, i, t), max, 0, i)

%   # Flex breaks are not conditional to the mode
format(W, K0, [{I, M, #doc_break{break = S, mode = flex}} | T]) ->
    K = K0 + byte_size(S),
    case W == infinity orelse M == flat orelse fits(W, K, true, T) of
        true -> [S | format(W, K, T)];
        false -> [indent(I) | format(W, I, T)]
    end;

%   defp format(w, k, [{i, m, doc_break(s, :flex)} | t]) do
%     k = k + byte_size(s)

%     if w == :infinity or m == :flat or fits?(w, k, true, t) do
%       [s | format(w, k, t)]
%     else
%       [indent(i) | format(w, i, t)]
%     end
%   end

%   # Strict breaks are conditional to the mode
format(W, K, [{I, M, #doc_break{break = S, mode = strict}} | T]) ->
    case M of
        break -> [indent(I) | format(W, I, T)];
        _ -> [S | format(W, K + byte_size(S), T)]
    end;

%   defp format(w, k, [{i, mode, doc_break(s, :strict)} | t]) do
%     if mode == :break do
%       [indent(i) | format(w, i, t)]
%     else
%       [s | format(w, k + byte_size(s), t)]
%     end
%   end

%   # Nesting is conditional to the mode.
format(W, K, [{I, M, #doc_nest{doc = X, indent = J, always_or_break = Nest}} | T]) ->
    case Nest == always orelse (Nest == break andalso M == break) of
        true -> format(W, K, [{apply_nesting(I, K, J), M, X} | T]);
        false -> format(W, K, [{I, M, X} | T])
    end;
%   defp format(w, k, [{i, mode, doc_nest(x, j, nest)} | t]) do
%     if nest == :always or (nest == :break and mode == :break) do
%       format(w, k, [{apply_nesting(i, k, j), mode, x} | t])
%     else
%       format(w, k, [{i, mode, x} | t])
%     end
%   end

%   # Groups must do the fitting decision.
format(W, K, [{I, break, #doc_group{group = X, mode = inherit}} | T]) ->
    format(W, K, [{I, break, X} | T]);

%   defp format(w, k, [{i, :break, doc_group(x, :inherit)} | t]) do
%     format(w, k, [{i, :break, x} | t])
%   end

format(W, K, [{I, _, #doc_group{group = X}} | T]) ->
    case W == infinity orelse fits(W, K, false, [{I, flat, X}]) of
        true -> format(W, K, [{I, flat, X} | T]);
        false -> format(W, K, [{I, break, X} | T])
    end.
%   defp format(w, k, [{i, _, doc_group(x, _)} | t]) do
%     if w == :infinity or fits?(w, k, false, [{i, :flat, x}]) do
%       format(w, k, [{i, :flat, x} | t])
%     else
%       format(w, k, [{i, :break, x} | t])
%     end
%   end

collapse([<<"\n", _/binary>> | T], Max, Count, I) ->
    collapse(T, Max, Count + 1, I);

%   defp collapse(["\n" <> _ | t], max, count, i) do
%     collapse(t, max, count + 1, i)
%   end

collapse([<<"">> | T], Max, Count, I) ->
    collapse(T, Max, Count, I);
%   defp collapse(["" | t], max, count, i) do
%     collapse(t, max, count, i)
%   end

collapse(T, Max, Count, I) ->
    NewLines = binary:copy(<<"\n">>, min(Max, Count)),
    Spaces = binary:copy(<<" ">>, I),
    [<<NewLines/binary, Spaces/binary>> | T].
%   defp collapse(t, max, count, i) do
%     [:binary.copy("\n", min(max, count)) <> :binary.copy(" ", i) | t]
%   end

apply_nesting(_, K, cursor) -> K;
apply_nesting(_, _, reset) -> 0;
apply_nesting(I, _, J) -> I + J.
%   defp apply_nesting(_, k, :cursor), do: k
%   defp apply_nesting(_, _, :reset), do: 0
%   defp apply_nesting(i, _, j), do: i + j

indent(0) -> ?newline;
indent(I) -> 
    Spaces = binary:copy(" ", i),
    <<?newline/binary,Spaces/binary>>.

%   defp indent(0), do: @newline
%   defp indent(i), do: @newline <> :binary.copy(" ", i)