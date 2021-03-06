#' Package: rsppfp
#'
#' Transformation algorithms to translate the SPPFP (Shortest Path Problem with Forbidden
#'    Paths) to a traditional shortest-path problem that includes the forbidden paths.
#'
#' The SPPFP is a variant of the traditional shortest path problem, in which no solution
#'    can include any path listed on a known set of forbidden paths. The current approach
#'    to solve this is to translate the existing graph, and its set of forbidden paths,
#'    to a graph in which no path will include any forbidden sequence.
#' This package provides straightforward parallel processing capabilities, as well as
#'    translation functions to use the algorithms on undirected graphs. It is highly
#'    compatible with other network research packages, as it only uses native R data types.
#'
#' @docType package
#' @name rsppfp
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
globalVariables(c("from", "to", "node1", "rowid", "node3", "var", "startNode", "k", "bn", "nn", "i"))