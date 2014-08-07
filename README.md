Haskell-Chart-Parser
====================

A simple chart parsing library for Haskell. The parser is generic over what
it parses into, so it can produce parse trees, evaluations, anything.

Chart Parsing
-------------

Chart parsing consists of iteratively adding more and more information
to a parsing chart until the chart is "completed". Chart parsing is
ideal for parsing ambiguous grammars, as it can produce all possible
parses.

A chart is a graph where the edges are labeled by either unparsed tokens
or by parsed items. The nodes of the chart represent the inter-token
positions in the input, and an edge labeled l from node i to node j
represents that the input from position i to j can be parsed as l.

For example, consider the BNF grammar for ambiguous arithmetic expressions

    <exp> ::= "x" | "y" | "z" | <exp> "+" <exp> | <exp> "-" <exp>

Prior to adding parse edges, the expression

    x + y - z

corresponds to the chart

!["x + y - z" before parsing](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart-before-parsing.jpg)

We parse the expression by progressively adding labels to the chart
according to the grammar. For instance, we can add a label from 0 to 1
because the token `x` can be parsed as an `<exp>`:
    
!["x + y - z" after adding an edge](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart-after-adding-edge.jpg)

After adding some more edges, we will have recognized that both spans
`x + y` and `y - z` can be parsed as `<exp>`s:
    
!["x + y - z" with edges spanning "x + y" and "y + z"](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart-ambiguous-edges.jpg)

This sort of ambiguity is ok in a chart parser. In this particular chart,
we're labeling with just non-terminal symbols (specifically, just `<exp>`),
so the end result will not distinguish the two parses, but a richer label type
could be chosen, for instance, an AST.

When we're done adding edges to the chart, it looks like this:

!["x + y - z" after parsing](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart-after-parsing.jpg)

Because there's an edge from the beginning of the chart to the end of the chart,
we know that the whole of the input parses. In this case, it's an `<exp>`.

In general, chart parsers can add edges to the chart however they like.
The chart parser in this library, however, uses a left-corner technique
to parse the input one word at a time. We start out in a parse state with
an empty chart, and a list of tokens waiting to be added to the chart.

!["x + y - z" initial parse state](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart2-initial-state.jpg)

We then read a token by adding an edge to the graph labeled by that token.

!["x + y - z" after adding "x"](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart2-add-x.jpg)

We then add to the chart all the edges we can that end after the newly added
token. In this library, this is called saturating the chart.

!["x + y - z" after saturating the first time](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart2-saturation-1.jpg)

Saturating like this ensures that the chart has as much information as
possible after reading each token, so that parsing happens incrementally.
After reading a few more tokens, the chart looks like this:
    
!["x + y - z" after reading some more tokens](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart2-saturation-2.jpg)

And finally we end up with the final chart we got before. The evolution
of the parse state looks like this:

!["x + y + z" parse animation](http://www.github.com/psygnisfive/Haskell-Chart-Parser/img/chart-animation.gif)