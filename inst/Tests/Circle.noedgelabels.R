
# Test with 6 edges, no causal structure:

source("startup.R")

Z <- DynamicGraph(V.Names, V.Types, From, To, texts = c("Gryf", "gaf"),
                  object = Object, UserMenus = Menus, w = 4,
                  namesOnEdges = FALSE, 
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
