
# Test with factor graph:

source("startup.R")

Factors <- list(V.Names[c(1, 2, 3, 4)], 
                V.Names[c(3, 4, 5)], 
                c(V.Names[c(4, 5, 6)], "Gryf"))
Z <- DynamicGraph(V.Names, V.Types, from = NULL, to = NULL,
                  factors = Factors, object = Object, UserMenus = Menus,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
