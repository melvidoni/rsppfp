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