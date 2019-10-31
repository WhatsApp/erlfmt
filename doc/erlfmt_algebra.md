# `erlfmt_algebra`

The algebra module contains APIs for composing pretty-printing documents at multiple different levels.

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
