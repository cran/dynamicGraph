
# Test with block recursive model:

source("startup.0.R")

Block.tree <- 
 list(g = 0, F = 0.13, horizontal = TRUE, label = "Pedegree.gF",
      P.G.Father = list(Vertices = c("P.G.Father")),
      P.G.Mother = list(Vertices = c("P.G.Mother")),
      M.G.Father = list(Vertices = c("M.G.Father")),
      M.G.Mother = list(Vertices = c("M.G.Mother")),
      common.children = 
      list(g = 0, F = 0.17, horizontal = TRUE,
           Father = list(Vertices = c("Father")),
           Mother = list(Vertices = c("Mother")),
           common.children = 
           list(g = 0, F = 0.25, horizontal = TRUE,
                Male = list(Vertices = c("Male")),
                Female = list(Vertices = c("Female")),
                common.children = 
                list(Vertices = c("Marriage"), g = 3,
                     Son = 
                     list(Vertices = c("Son"), g = 3,
                          P.G.Son = list(Vertices = c("P.G.Son"), g = 0),
                          P.G.Dat = list(Vertices = c("P.G.Dat"), g = 0)),
                     Dat = 
                     list(Vertices = c("Dat"), g = 3,
                          M.G.Son = list(Vertices = c("M.G.Son"), g = 0),
                          M.G.Dat = list(Vertices = c("M.G.Dat"), g = 0))
                     )
               )
          )
     )

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)

FromTo <- matrix(c("P.G.Father", "Father",
                   "P.G.Mother", "Father",
                   "M.G.Father", "Mother",
                   "M.G.Mother", "Mother",
                   "Father",     "Male",
                   "Mother",     "Male",
                   "Male",       "Marriage",
                   "Female",     "Marriage",
                   "Marriage",   "Son",
                   "Marriage",   "Dat",
                   "Son",        "P.G.Son",
                   "Son",        "P.G.Dat",
                   "Dat",        "M.G.Son",
                   "Dat",        "M.G.Dat"), byrow = T, ncol = 2)
From <- match(FromTo[,1], V.Names)
To   <- match(FromTo[,2], V.Names)

V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, 
                  block.tree = Block.tree, object = Object,
                  width = 600, height = 600, 
                  drawblocks = TRUE, drawBlockFrame = TRUE, 
                  overlaying = TRUE, blockColors = blockColors.blue,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  UserMenus = Menus, title = "Pedegree.gF")
