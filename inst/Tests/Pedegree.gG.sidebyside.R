
# Test with block recursive model, absolute space for blocks:

source("startup.0.R")

Block.tree <- 
 list(g = 0, G = 11, label = "Pedegree.gG.sidebyside",
      P.G.Father = list(g = 2, Vertices = c("P.G.Father")),
      P.G.Mother = list(g = 2, Vertices = c("P.G.Mother")),
      M.G.Father = list(g = 2, Vertices = c("M.G.Father")),
      M.G.Mother = list(g = 2, Vertices = c("M.G.Mother")),
      common.children = 
      list(g = 0, G = 11,
           Father = list(g = 2, Vertices = c("Father")),
           Mother = list(g = 2, Vertices = c("Mother")),
           common.children = 
           list(g = 0, G = 11,
                Male = list(g = 2, Vertices = c("Male")),
                Female = list(g = 2, Vertices = c("Female")),
                common.children = 
                list(g = 4, Vertices = c("Marriage"), 
                     Son = 
                     list(g = 4, Vertices = c("Son"), 
                        P.G.Son = list(Vertices = c("P.G.Son")),
                        P.G.Dat = list(Vertices = c("P.G.Dat"))),
                     Dat = 
                     list(g = 4, Vertices = c("Dat"), 
                          M.G.Son = list(Vertices = c("M.G.Son")),
                          M.G.Dat = list(Vertices = c("M.G.Dat")))
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
                  overlaying = FALSE, blockColors = rev(blockColors.blue),
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  UserMenus = Menus, title = "Pedegree.gG.sidebyside")
