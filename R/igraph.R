#' @export
#' @title igraph Shortest Path
#'
#' @description A original node N_i can appear on a transformed gStar as different N_i* equivalent
#'    nodes. Therefore, this becomes a limitation when searching for a shortest path inside gStar.
#'    As a result: all N_i* need to be considered as possible destination nodes when looking for
#'    the shortest path. This function is a wrapper for this behavior, providing a straightforward
#'    implementation using igraph capabilities. However, it aims to provide
#'    guidance on how to build a similar algorithm for different path-finding algorithms.
#'    
#'    It is important to mention that new nodes are only considered as destination nodes, and they
#'    are not search for origin nodes. This is because N* nodes can only be reached after traveling
#'    through gStar nodes. For example, a node \code{"f|e|r"} is actually indicating that \code{"r"}
#'    has been reached after traveling through the nodes \code{"f"} and \code{"e"}.
#' 
#' @family igraph Integration
#'
#' @param g A gStar digraph in data frame format, translated using one of the available functions.
#'   The weight or cost attribute of each arc of the graph must be stored in a specific column
#'   named \code{weight}.
#' @param origin The name of the starting node from G for the path. It must be written as it
#'    appears in G, and it is preferable to use a character format, but this can also be of
#'    any simple type. No lists or vectors are allowed.
#' @param dest The name of the destination node from G for the path. It must be written as it
#'    appears in G, and it is preferable to use a character format, but this can also be of any
#'    simple type. No lists or vectors are allowed.  
#' @param weightColName The name of the weight column used in the dataframe. If the column does not
#'    exist, a name _must_ be provided so that a new weight column is generated.     
#'
#' @return The shortest path from \code{origin} node to \code{dest} node, calculated in G*, to
#'    include the forbidden paths. It uses igraph's functionalities.
#'    
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph shortest_paths
#' @importFrom igraph E
#' @importFrom igraph edge_attr
#' @importFrom foreach %do%
#' @importFrom foreach foreach    
#'
#' @examples
#' # Given a specific gStar graph:
#' gStar <- data.frame(from = c("u|v", "s|u|v", "s|u", "s", "s", "u", "w", "w", "x", "x", 
#'                              "v", "v", "y", "y", "s", "s|u", "u", "u|v"),
#'                     to = c("t", "u|v|y", "w", "w", "x", "w", "v", "y", "w", "y", "y", "t", 
#'                             "t", "u", "s|u", "s|u|v", "u|v", "u|v|y"), 
#'                    weight = c(12L, 3L, 5L, 9L, 7L, 5L, 11L, 10L, 1L, 2L, 3L, 12L, 13L, 0L,
#'                               8L, 4L, 4L, 3L), 
#'                    stringsAsFactors = FALSE)
#' gStar
#' 
#' # Obtain the shortest path
#' get_shortest_path(gStar, "s", "v", "weight")                                                 
#'
#'
get_shortest_path <- function(g, origin, dest, weightColName = NULL) {
  #If there is no weight column specified, assume equal weights
  if(is.null(weightColName)) {
    g$weight <- 1
    weightColName <- "weight"
  # If the column could not be found...
  } else if(!weightColName %in% colnames(g)) {
    #Show an error
    stop(weightColName, " is not a variable in `g`.")
  }
  
  # Convert the graph
  g.i <- graph_from_data_frame(g)
  
  # Get all nodes where for the destination is the destination
  destEq <- get_all_nodes(g, dest)
  
  # Find shortest paths from `origin` to all N* corresponding to `dest`
  # - suppress warning if not all destinations reachable
  sp <- suppressWarnings(shortest_paths(g.i, from = origin, to = destEq,
                                        weights = edge_attr(g.i, weightColName),
                                        output = "both"))
  
  # Filter out zero-length paths (return if nothing left)
  zero_length <- lengths(sp$epath) == 0
  if (all(zero_length)) {
    warning("There is no path from ", origin, " to ", dest, ".\n")
    return (character(0))
  } else {
    sp <- lapply(sp, function(element) element[!zero_length])
  }
  
  # Find shortest of remaining paths
  dist <- vapply(sp$epath, 
                 function(path) sum(edge_attr(g.i, weightColName, path)),
                 numeric(1)) 
  shortestPath <- sp$vpath[[which.min(dist)]]
  
  # Convert path with nodes from N* to path with nodes from N
  return( parse_vpath(names(shortestPath)) )
}