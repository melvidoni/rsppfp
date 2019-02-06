## RSPPFP 1.0.4
Stable release with minor bug fixes. Released February 2019.

### Minor Bug Fixes and Improvements
 - Fixed bugs for missing nodes on both transformation algorithms.
 - The numeric or integer attributes on graphs with multiple attributes are no longer converted to characters. They are parsed to the generic numeric format.
 - Minor wording changes on warnings and error messages.
 - Updates to the `get_shortest_path()` function, to improve its functionality.

## RSPPFP 1.0.3
Stable release of advanced implementation of rsppfp, with minor bug fixes. Released November 2018.

### Minor Bug Fixes and Improvements
 - Additional checks have been added to Hsu's and Villeneuve's transformation. The algorithms now stop if certain conditions are not met, such as: columns' names, use of non-existent nodes in forbidden paths, and subpaths inclusions for Villeneuve's algorithms.
 - Additional handling of the weight has been added to the convenience function `get_shortest_path()`. The user is now required to explicit the `weight` attribute name. If it does not exist, a standard weight of 1 is added to the graph.
 - All examples and documentation now feature data frames created with `data.frame()`, instead of using `structure()` The importance of using `stringAsFactors = FALSE` has been highlighted.
 - Cluster closing has been moved to `on.exit()`, so that if the function terminates early, all the connections will still be closed.
 - The benchmark on the website now showcases the results for all the evaluated density types.


## RSPPFP 1.0.2
Stable release of advanced implementation of rsppfp, with minor bug fixes. Released October 2018.

### Minor Bug Fixes and Improvements
 - Fixed Hsu's transformation: when having more than three columns (`from` and `to`, and an undefined number of attributes) the resulting `gStar` has the attributes in the correct order.
 - Improved Hsu's transformation: previous to working, it deletes empty rows from the set of forbidden paths. Empty rows are those in which all of the cells contain either only blank spaces or `NA` values.


## RSPPFP 1.0.0
First stable release of advanced implementation of rsppfp, released on June 2018.

### Major Changes
First stable release of advanced implementation of rsppfp! Includes the following functions:
  - Transformation algorithms (Villeneuve & Desaulniers, and Hsu's et al.).
  - Basic parsing functions: graph-to-digraph, and vpath translation.
  - Integration functions: equivalent nodes selection, and igraph's integration.