#' @title Parser for G* nodes paths.
#'
#' @description Translates a sequence of nodes from a G* graph, generated with any of the available
#' transformations, to a sequence of nodes in terms of the original G.
#'
#' @family Parsers
#'
#' @param vpath A vector of character type, representing a path as a sequence of nodes. The nodes are
#'    supposed to belong to an original graph G, but be written in terms of G*.
#'
#' @return A new vector of character type, representing the same path as \code{vpath} but with the nodes
#'    names translated to the original graph G's names.
#'
#' @examples
#' # Obtain the vpath from any way, an algorithm or random walk.
#' # Call the parsing function
#' translated_vpath <- parse_vpath( c("s|u", "s|u|v", "u|v|y", "t") )
#'
#' # Print the result
#' translated_vpath
#'
#' [1]
#' "u" "v" "y" "t"
#'
#'
parse_vpath <- function(vpath) {
  # Create al alternative vector of the same length
  altVPath <- vector(mode = "character", 0)

  # Now for each element on the original vector
  for(n in vpath) {
    # If this is a new node, split by pipe and record the last element
    # Otherwise, just keep the name of the node as-is
    altVPath <- c(altVPath, ifelse(grepl("\\|", n), gsub(".*\\|(.*)", "\\1", n), n))
  }

  # Return the new path
  return(altVPath)
}



#' @title Alias function for foreach's dopar
#'
#' @description Provides an alias for dopar, so it can be used inside this package.
#'
`%dopar%` <- foreach::`%dopar%`



#' @title Undirected Graph Translator
#'
#' @description The SPPFP transformation functions only work with digraphs -i.e. directed graphs. Because in a digraph
#'    arcs can only be traveled in one direction, from the origin node to the destination arc, if undirected graphs are
#'    used as-is, the resultng G* will not be accurate. Therefore, this functions translates an undirected graph to a
#'    digraph by duplicating each arc and swapping the duplicate's \code{from} and \code{to} nodes.
#'
#' @family Parsers
#'
#' @param graph An undirected graph written as a data frame, in which each rows represent an arc. The columns
#'    must be named \code{from} and \code{to}, and can be of any data type. Each row can have additional
#'    attributes, and no cells can be \code{NULL} or \code{NA}.
#' @param cores This algorithm can be run using R's parallel processing functions. This variable represents
#'    the number of processing cores you want to assign for the transformation. The default value is one single
#'    core. It is suggested to not assign all of your available cores to the function.
#'
#' @return A new graph, with the same columns and data types of the original graph. This new graph is twice as
#'    big as the original, as new arcs are added to represent that each arc can be traveled in both directions.
#'
#' @examples
#' # Obtain the graph from any way
#' undirected_graph <- read.csv(...)
#'
#' # Translate it
#' digraph <- direct_graph(undirected_graph)
#'
#'
direct_graph <- function(graph, cores = 1L) {
  # Get the number of columns
  ncol <- ncol(graph)

  # Set up the parallel
  cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cluster)

  # Loop through the original graph
  directedGraph <- foreach::foreach(row = 1:nrow(graph), .combine = rbind) %dopar% {
    # Create the new rows
    tempDG <- graph[0,]

    # Check the number of columns
    if(ncol > 2) {
      # If this has attributes, add them
      tempDG[nrow(tempDG) + 1, ] <- list(graph[row, 2], graph[row, 1], graph[row, 3:ncol(graph)])
    }
    # Otherwise add only from/to
    else tempDG[nrow(tempDG) + 1, ] <- list(graph[row, 2], graph[row, 1])

    # Return the value
    tempDG
  }

  # Stop Cluster
  parallel::stopCluster(cluster)

  # Return the value
  rbind(graph, directedGraph)
}
