
# Test, oriented (cyclic) edges, without causal structure:

source("startup.R")

Z <- DynamicGraph(V.Names, V.Types, From, To, oriented = TRUE, 
                  object = Object, UserMenus = Menus)
