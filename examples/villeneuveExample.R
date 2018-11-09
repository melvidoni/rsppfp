# Import the packages and libraries
lapply(c("rsppfp", "igraph"), require, character.only = TRUE)


# LOADING DATA AND TRANSFORMING THE GRAPH
# ------------------------------------------------------------------------------------------------------
# Load the sample graph
graph <- data.frame(from = c("s", "s", "s", "u", "u", "w", "w", "x", "x", "v", "v", "y", "y"),
                    to = c("u", "w", "x", "w", "v", "v", "y", "w", "y", "y", "t", "t", "u"),
                    cost = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                    stringsAsFactors = FALSE)
# Load the forbidden paths:
#   F = { f1, f2 } where f1 = {s, u, v, t} and f2 = {u, v, y, u}
fpaths <- data.frame(V1 = c("s", "u"), V2 = c("u", "v"), V3 = c("v", "y"), V4 = c("t", "u"),
                     stringsAsFactors = FALSE)


# Run the algorithm and transform the graph
gStar <- modify_graph_vd(graph, fpaths, 3L)



# USING IGRAPH WITH RSPPFP's GSTAR
# ------------------------------------------------------------------------------------------------------
# Plot the graph using igraph's functionalities
gStar.igraph <- graph_from_data_frame(gStar)
tkplot( graph_from_data_frame(graph) )
tkplot( gStar.igraph )

# Obtain a shortest path
shortestPath <- V(gStar.igraph)$name[ shortest_paths(gStar.igraph, from = "u", 
                                                     to = "t", mode = "out", output = "vpath")$vpath[[1]] ]



# TRANSLATING THE VPATH
# ------------------------------------------------------------------------------------------------------
# The obtained vpath is written in terms of gStar, so we might need to translate it
shortestPath.translated <- parse_vpath(shortestPath)

# Print the outcome
shortestPath.translated
