
# Test with block recursive model:

source("startup.0.R")

Block.tree <- 
 list(label = "H", Vertices = c("H"),
      D = list(D = list(Vertices = c("D"),
                        B = list(Vertices = c("B"))),
               A = list(Vertices = c("A"))
               ),
      common.children = 
      list(TB = list(Vertices = c("T3")),
           TA = list(Vertices = c("T1", "T2"))
           )
      )

From <- c("H", "D", "D", "A",  "A",  "B"   )
To   <- c("D", "A", "B", "T1", "T2", "T3"  )

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)

From <- match(From, V.Names)
To   <- match(To, V.Names)
V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, block.tree = Block.tree,
                  object = Object,
                  width = 600, height = 600, drawblocks = TRUE,
                  UserMenus = Menus, overlaying = TRUE,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update)
