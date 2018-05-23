# Import the packages and libraries
lapply(c("rsppfp", "igraph"), require, character.only = TRUE)


# LOADING DATA AND TRANSFORMING THE GRAPH
# ------------------------------------------------------------------------------------------------------
# Load the sample graph
graph <- structure(list(from = c("s", "s", "s", "u", "u", "w", "w", "x", "x", "v", "v", "y", "y"),
                        to = c("u", "w", "x", "w", "v", "v", "y", "w", "y", "y", "t", "t", "u"),
                        cost = c(1L, 4L, 1L, 2L, 7L, 1L, 2L, 5L, 1L, 4L, 1L, 3L, 9L)),
                   .Names = c("from", "to", "cost"), class = "data.frame", row.names = c(NA, -13L))

# Load the forbidden paths:
#   F = { f1, f2, f3, f4 } where f1 = {u, v, y, u}, f2 = {u, w, y, u}, f3 = {w, v, y} and f4 = {x, w, v, y, t}
fpaths <- structure(list(V1 = c("u", "u", "w", "x"), V2 = c("v", "w", "v","w"), V3 = c("y", "y", "y", "v"),
                         V4 = c("u", "u", "", "y"), V5 = c("", "", "", "t")),
                    .Names = c("V1", "V2", "V3", "V4", "V5"), class = "data.frame", row.names = c(NA, -4L))

# Run the algorithm and transform the graph
gStar <- modify_graph_hsu(graph, fpaths, 3L)



# USING IGRAPH WITH RSPPFP's GSTAR
# ------------------------------------------------------------------------------------------------------
# Plot the graph using igraph's functionalities
gStar.igraph <- graph_from_data_frame(gStar)
tkplot( graph_from_data_frame(graph) )
tkplot( gStar.igraph )

# Obtain a shortest path
shortestPath <- V(gStar.igraph)$name[ shortest_paths(gStar.igraph, from = "u", to = "t", mode = "out", output = "vpath")$vpath[[1]] ]



# TRANSLATING THE VPATH
# ------------------------------------------------------------------------------------------------------
# The obtained vpath is written in terms of gStar, so we might need to translate it
shortestPath.translated <- parse_vpath(shortestPath)

# Print the outcome
shortestPath.translated
