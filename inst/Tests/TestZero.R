
# Test with no edges:

source("startup.R")

From <- NULL
To   <- NULL

Z <- DynamicGraph(V.Names, V.Types, From, To, object = Object, UserMenus = Menus)
