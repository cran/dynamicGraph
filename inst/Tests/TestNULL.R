
# Test with no edges, NULL object:

source("startup.R")

From <- NULL
To   <- NULL

Z <- DynamicGraph(V.Names, V.Types, From, To, object = NULL, UserMenus = Menus)
