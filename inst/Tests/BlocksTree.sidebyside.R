
# Test with block recursive model:

source("startup.R")

Block.tree <- list(Vertices = c(27), label = "W",
                   # horizontal = FALSE,
                   X = list(Vertices = c(1, 2, 3, 4),
                            A = list(Vertices = c(5, 6, 7),
                                     horizontal = FALSE,
                                     d = list(Vertices = c(11, 12)),
                                     e = list(Vertices = c(13))),
                            B = list(Vertices = c(8, 9),
                                     f = list(Vertices = c(14),
                                              horizontal = FALSE),
                                     g = list(Vertices = c(15, 16),
                                              horizontal = FALSE),
                                     h = list(Vertices = c(17, 18, 19),
                                              horizontal = FALSE),
                                     k = list(Vertices = c(20, 21, 22, 23),
                                              horizontal = FALSE)),
                            C = list(Vertices = c(10),
                                     i = list(Vertices = c(24),
                                              j = list(Vertices = c(25, 26)) )
                                     )
                              ))

From <- c(27, 27, 27, 27, 
           1, 5, 6, 2, 2, 3, 3,  4, 12, 13, 14, 16, 19, 23,
           5,  6,  7,  8,  8,  8,  9, 17, 18,  9, 20, 21, 22, 10, 24, 24)
To   <- c( 1,  2,  3,  4, 
           5, 6, 7, 8, 8, 9, 9, 10, 14, 14, 16, 19, 23, 25,
          11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)

v <- unlist(Block.tree)
v <- v[v!=0]
v <- v[v!="FALSE"]
v <- v[v!="TRUE"]
v <- v[v!="W"]
V.Names <- paste("V", 1:length(v), sep = "")
rm(v)
V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, block.tree = Block.tree,
                  object = Object,
                  width = 600, height = 600, drawblocks = TRUE,
                  UserMenus = Menus, overlaying = FALSE,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
