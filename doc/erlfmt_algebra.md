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

    xxxxxxxx      yyyy         xxxxxxxx
    xxx       <>  yyyyyyyy  =  xxx
    xxxxxxx                    xxxxxxx
    xxxxx                      xxxxxyyyy
                                    yyyyyyyy

