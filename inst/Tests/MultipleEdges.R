
# Test with 3 edges, each with more than two vertices:

source("startup.R")

EdgeList <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))

Z <- DynamicGraph(V.Names, V.Types, edge.list = EdgeList, object = Object,
                  UserMenus = Menus)
