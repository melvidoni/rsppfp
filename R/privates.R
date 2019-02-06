#' @title Villeneuve's Combination Function
#'
#' @keywords internal
#' 
#' @description This function combines the outputs produced on the parallel loop of
#'    the Villeneuve and Desaulnier's algorithm implementations.
#'    It cannot be used for other purposes.
#'
#' @family Private Functions
#' 
#' @details Private function that cannot be used by the package's end-users.
#'
#' @param x Dataframes to be merged using \code{rbind}.
#'
#'
.comb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}




#' @title Additional Attributes Getter
#'
#' @keywords internal
#' 
#' @description Given a graph G, and two nodes \code{from} and \code{to}. The function searches for
#'    the arc that goes in the direction "from-to", and returns a list of its attributes,
#'    without the original nodes.
#'
#' @details Private function that cannot be used by the package's end-users.
#'
#' @family Private Functions
#'
#' @param g The original graph from which the attributes need to be extracted. This cannot be
#'    a G*, and it must have at least one attribute, besides the \code{from} and \code{to} columns.
#' @param f The name of the node that needs to be the origin of the arc that will be searched for.
#' @param t The name of the node that needs to be de destination of the arc that will be searched for.
#'
#' @return A list of the attributes for the corresponding arc. Since this function is called 
#'    from a controlled space, it assumes that the arc always exists.
#'
.get_arc_attributes <- function(g, f, t) {
  ss <- subset(g, from == f & to == t)[1,3:length(colnames(g))] %>% as.data.frame()

  for(i in 3:ncol(g)) {
    if(is.numeric(g[,i]) | is.integer(g[,i]))
      ss[,(i-2)] <- as.numeric(ss[,(i-2)])
  }
  
  return(ss)
}




#' @title Node Inclusion Checker
#'
#' @keywords internal
#'
#' @description Given a graph \code{g}, and a data frame of forbidden paths \code{f}, the function
#'    checks that all nodes used on \code{f} are also present on the graph.
#'
#' @details Private function that cannot be used by the package's end-users.
#'
#' @family Private Functions
#'
#' @param g The original graph from which the attributes need to be extracted. This cannot be a G*,
#'    and it must have at least one attribute, besides the \code{from} and \code{to} columns.
#' @param f The set of forbidden paths, written as a data frame. Each row represents a path
#'    as a sequence of nodes. Each row may be of different size, filling the empty cells with
#'    \code{NA}. All nodes involved must be part of \code{g}, and no forbidden path can be of
#'    size 2. This is because the latter is thought as an arc that should not exist in
#'    the first place.
#'
#' @return \code{TRUE} if all nodes in \code{f} are present in the graph. Otherwise,
#'    it returns \code{FALSE}.
#'
.nodesExists <- function(g, f) {
  # Get all unique values
  uniqueG <- c(g$from, g$to) %>% unique()
  uniqueFP <- unlist(f) %>% unique()
  uniqueFP <- uniqueFP[!is.na(uniqueFP)]
  
  # Return the result of comparing if all values are present
  return(all(uniqueFP %in% uniqueG, TRUE))
}



#' @title Subpaths Inclusion Checker
#'
#' @keywords internal
#'
#' @description Given a data frame of forbidden paths \code{f}, the function checks that no 
#'    subpath of any path is included in any other path. It only checks for subpaths of length 
#'    3, as it is the minimum combination that can be repeated
#'
#' @details Private function that cannot be used by the package's end-users.
#'
#' @import dplyr
#' @importFrom tidyr gather
#' 
#' @family Private Functions
#'
#' @param f The set of forbidden paths, written as a data frame. Each row represents a path
#'    as a sequence of nodes. Each row may be of different size, filling the empty cells with
#'    \code{NA}. All nodes involved must be part of \code{g}, and no forbidden path can be of
#'    size 2. This is because the latter is thought as an arc that should not exist in the
#'    first place.
#'
#' @return \code{TRUE} if there is at least one subpath included in another forbidden path,
#'    \code{FALSE} otherwise.
#'
.hasSubpaths <- function(f) {
  # Copy and rename columns
  subpaths <- f
  colnames(subpaths) <- sapply(1:ncol(f), function(x) paste0("V", x))
  subpaths$rowid <- seq.int(nrow(f))
  
  # Reconstruct the paths
  subpaths <- subpaths %>%
    tidyr::gather(key = "var", value = "node1", dplyr::starts_with("V")) %>%
    dplyr::filter( !is.na(node1) ) %>%
    dplyr::group_by(rowid) %>% dplyr::arrange(rowid) %>%
    dplyr::mutate( node2 = lead(node1, n = 1), node3 = lead(node1, n = 2) ) %>%
    dplyr::filter(!is.na(node3)) %>% dplyr::select(-var)

  # Add an additional value
  subpaths$dup <- FALSE
  subpaths <- as.data.frame(subpaths)
  
  # Now go through each row
  for(i in 1:nrow(subpaths)) {
    # Check if it exists in another row
    subpaths[i, "dup"] <- apply(f[-subpaths[i, "rowid"], ], 1, function(x) {
      stringr::str_detect(paste(x, collapse=""), paste(subpaths[i, 2:4], collapse = ""))
    }) %>% any()
  }
  rm(i)

  # Return if there are any duplicates
  return(any(subpaths$dup))
}


