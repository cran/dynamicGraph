
# Test with 6 edges, no causal structure:

source("startup.R")

Z <- DynamicGraph(V.Names, V.Types, From, To, 
                  texts = c("-50--50",
                            "  0-  0", " 50- 50",
                            "100-100", "150-150",
                            "200-200", "250-250",
                            "300-300", "350-350",
                            "400-400", "450-450"), diagonal = TRUE,
                  object = Object, UserMenus = Menus, w = 4,
#                 debug.strata = debug.strata, debug.edges = debug.edges, 
#                 debug.position = debug.position, debug.update = debug.update)
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
