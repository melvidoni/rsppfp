#' @title Villeneuve's Combination Function
#'
#' @description This function combines the outputs produced on the parallel loop of the Villeneuve and
#'    Desaulnier's algorithm implementations. It cannot be used for other purposes.
#'
#' @details Private function that cannot be used by the package's end-users.
#'
#' @family Private Functions
#'
.comb <- function(x, ...) {
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}




#' @title Additional Attributes Getter
#'
#' @description Given a graph G, and two nodes \code{from} and \code{to}. The function searches for the arc that
#'    goes in the direction "from-to", and returns a list of its attributes, without the original nodes.
#'
#' @details Private function that cannot be used by the package's end-users.
#'
#' @family Private Functions
#'
#' @param g The original graph from which the attributes need to be extracted. This cannot be a G*, and it must
#'    have at least one attribute, besides the \code{from} and \code{to} columns.
#' @param from The name of the node that needs to be the origin of the arc that will be searched for.
#' @param to The name of the node that needs to be de destination of the arc that will be searched for.
#'
#' @return A list of the attributes for the corresponding arc. Since this function is called from a controlled
#'    space, it assumes that the arc always exists.
#'
.get_arc_attributes <- function(g, f, t) {
  subset(g, from == f & to == t)[1,3:length(colnames(g))]
}