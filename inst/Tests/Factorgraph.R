
# Test with factor graph:

source("startup.R")

Factors <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))
Z <- DynamicGraph(V.Names, V.Types, from = NULL, to = NULL, viewType = "Factor",
                  factors = Factors, object = Object, UserMenus = Menus,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
