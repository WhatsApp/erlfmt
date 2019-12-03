# `erlfmt_algebra`

The algebra module contains APIs for composing pretty-printing documents at multiple different levels.
The API and algorithm implemented by this module are based on the approach outlined in the paper
["A pretty but not greedy printer" by Jean-Philippe Bernardy](https://jyp.github.io/pdf/Prettiest.pdf).

## `str()` type

Abstracts over charlists offering fast append and length operations.

## `lines()` type

Defines the basic algebra for building and combining documents. It supports horizontal composition
through the `lines_combine` function. Vertical composition can be achieved by composing the
`lines_flush` and `lines_combine` functions. Those two combinators allow expressing any layout.

The `combine` operator works by positionling the right operand to the bottom and right of the
left operand, for example:

    xxxxxxxx      yyyyyyyy     xxxxxxxx
    xxx       <>  yyyy      =  xxx
    xxxxxxx                    xxxxxxx
    xxxxx                      xxxxxyyyyyyyy
                                    yyyy

## `metric()` type

Metric abstracts over concrete layout shapes to keep track of only 3 measures - the height,
the maximum width and the width of the last line of the layout. With those 3 measures we can
order layouts as to how desirable they are - out goal is to produce the smallest layout possible.
The `metric_dominates/2` function defines a partial order over layouts that will later allow us to
eliminate some layouts from computation to limit the complexity of the algorithm.

The metric type defines the same operators as the lines type. Building on the previous example,
doing the same composition with metrics would yield:

    xxxxxxxx      xxxxxxxx     xxxxxxxxxxxxx
    xxxxxxxx  <>  xxxx      =  xxxxxxxxxxxxx
    xxxxxxxx                   xxxxxxxxxxxxx
    xxxxx                      xxxxxxxxxxxxx
                               xxxxxxxxx

## `document()` type

This is the main API for composing flexible layouts and pretty-printing them. It offers the same
`combine` and `flush` API that lower layers offer, but it adds a `choice` operator that allows
choosing between two layouts depending on which one is "better" - for example laying out function
arguments either horizontally or vertically.

Conceptually, the `document()` type can be thought as a list of all possible `line()` layouts
produced from the composition. Doing this naively, though, would lead to a exponential explosion
of layouts to consider. To avoid this, at each step we also track `metric()` related to a particular
layout, which is used in two ways:

  * We can discard layouts that won't fit on the page - since composing it with anything will also
    yield a layout that won't fit on the page;
  * We can discard layouts that are dominated by another layout - since composing a smaller or shorter
    layout with another will always yield a smaller or shorter layout overall.

This means that to consider the set of all possible layouts, it's enough to consider only the set of
non-dominated layouts, known as "pareto frontier".

### Choosing between multiple layouts

There are 2 cases where one layout has to be chosen from many where the partial order over metrics
can't be used alone:

  * When none of the layouts matches, but we still want to print something, we need to pick
    "the best looser". This is done by picking the layout with the minimal width.
  * At the very end where multiple layouts are possible. All the possible layouts are compared
    by height, max width and last line width (in that order) and the smallest one is picked.

### Additional API

  * `document_single_line` takes a document as input and retuns only single-line documents.
    Since this can return documents that would produce no layouts, its use has to always be
    paired with some alternative.
