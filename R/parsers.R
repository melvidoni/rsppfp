#' @export
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



#' @export
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
#' @importFrom foreach %dopar% 
#' @importFrom magrittr %>%
#' 
#' @examples
#' # Obtain the graph from any way
#' graph <- structure(list(from = c("s", "s", "s", "u", "u", "w", "w", "x", "x", "v", "v", "y", "y"), 
#'                        to = c("u", "w", "x", "w", "v", "v", "y", "w", "y", "y", "t", "t", "u"),
#'                        cost = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)),
#'                    .Names = c("from", "to", "cost"), class = "data.frame", row.names = c(NA, -13L))
#' graph                    
#'
#' # Translate it
#' digraph <- direct_graph(graph)
#' digraph
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







#' @export
#' @title Parser for G* nodes.
#'
#' @description A original node N_i can appear on a transformed G* as different nodes. This is the result of
#'    the creation of nodes in the transformation processes. Therefore, it is possible that the original node N
#'    does not exists on G*, or that multiple N_i* exist. Hence, as all new nodes are generated using a specific
#'    structure for the name -compiling all previous nodes names, split by pipe-, this function allows searching
#'    for all the N_i* nodes that are equivalente to N_i. This can be used to find shortest paths to all of them.
#'
#' @family Parsers
#'
#' @param g A graph in data frame format, translated using one of the available functions.
#' @param originalNode The name of the original node from G, that needs to be searched within G*. It is preferable
#'    to use a character format, but this can also be of any simple type. No lists or vectors are allowed.
#'
#' @return A new vector of character type, whose elements are all the N_i* equivalent to the original N node. This
#'    also includes the original node.
#'
#' @examples
#' # Given a specific gStar graph:
#' gStar <- structure(list(from = c("u|v", "s|u|v", "s|u", "s", "s", "u", "w", "w", "x", "x", "v", 
#'                                  "v", "y", "y", "s", "s|u", "u", "u|v"), 
#'                         to = c("t", "u|v|y", "w", "w", "x", "w", "v", "y", "w", "y", "y", "t", 
#'                                "t", "u", "s|u", "s|u|v", "u|v", "u|v|y"), 
#'                    weight = c(12L, 3L, 5L, 9L, 7L, 5L, 11L, 10L, 1L, 2L, 3L, 12L, 13L, 0L, 8L, 4L, 4L, 3L)), 
#'                    class = "data.frame", row.names = c(NA, -18L), .Names = c("from", "to", "weight"))
#' gStar
#' 
#' # Obtain all the nodes equivalent to N_i = "v"
#' get_all_nodes(gStar, "v")                                                   
#'
#'
get_all_nodes <- function(g, originalNode) {
  # Get the list of nodes
  nodes <- unique(c(g$from, g$to))

  # Get all the nodes that end on that original node
  nodes[sapply(paste0("*\\s|", originalNode, "$"), function(y) grepl(y, nodes))]
}
