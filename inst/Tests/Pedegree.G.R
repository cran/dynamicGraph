
# Test with block recursive model, absolute space for blocks:

source("startup.0.R")

Block.tree <-
  list(g = 0, G = 54, label = "Pedegree.G",
       Male.Side = 
       list(g = 0, G = 33,
            Father = 
            list(g = 0, G = 12,
                 P.G.Father = list(Vertices = c("P.G.Father.1")),
                 P.G.Mother = list(Vertices = c("P.G.Mother.1")),
                 common.children = list(g = 0, label = "Father.1",
                                        Vertices = c("Father.1"))),
            Mother = 
            list(g = 0, G = 12,
                 M.G.Father = list(Vertices = c("M.G.Father.1")),
                 M.G.Mother = list(Vertices = c("M.G.Mother.1")),
                 common.children = list(g = 0, label = "Mother.1",
                                        Vertices = c("Mother.1"))),
            common.children = list(g = 2, Vertices = c("Male"))),
  Female.Side = list(g = 0, G = 12,
    P.G.Father = list(Vertices = c("P.G.Father.2")),
    P.G.Mother = list(Vertices = c("P.G.Mother.2")),
    M.G.Father = list(Vertices = c("M.G.Father.2")),
    M.G.Mother = list(Vertices = c("M.G.Mother.2")),
    common.children = list(g = 0, G = 12, label = "Female",
      Father = list(Vertices = c("Father.2")),
      Mother = list(Vertices = c("Mother.2")),
      common.children = list(g = 2, Vertices = c("Female")))),
  common.children = list(Vertices = c("Marriage"), g = 3, label = "Children",
    Son = list(Vertices = c("Son"), g = 3, 
       P.G.Son = list(Vertices = c("P.G.Son"), g = 2),
       P.G.Dat = list(Vertices = c("P.G.Dat"), g = 1)),
    Dat = list(Vertices = c("Dat"), g = 2,
       M.G.Son = list(Vertices = c("M.G.Son")),
       M.G.Dat = list(Vertices = c("M.G.Dat")))
    )
)

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)

FromTo <- matrix(c("P.G.Father.1", "Father.1",
                   "P.G.Mother.1", "Father.1",
                   "M.G.Father.1", "Mother.1",
                   "M.G.Mother.1", "Mother.1",
                   "Father.1",     "Male",
                   "Mother.1",     "Male",
                   "P.G.Father.2", "Father.2",
                   "P.G.Mother.2", "Father.2",
                   "M.G.Father.2", "Mother.2",
                   "M.G.Mother.2", "Mother.2",
                   "Father.2",     "Female",
                   "Mother.2",     "Female",
                   "Male",         "Marriage",
                   "Female",       "Marriage",
                   "Marriage",     "Son",
                   "Marriage",     "Dat",
                   "Son",          "P.G.Son",
                   "Son",          "P.G.Dat",
                   "Dat",          "M.G.Son",
                   "Dat",          "M.G.Dat"), byrow = T, ncol = 2)

From <- match(FromTo[,1], V.Names)
To   <- match(FromTo[,2], V.Names)

V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, 
                  block.tree = Block.tree, object = Object,
                  width = 600, height = 600, 
                  drawblocks = TRUE, drawBlockFrame = TRUE, 
                  overlaying = TRUE, blockColors = blockColors.violet,
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  UserMenus = Menus, title = "Pedegree.G")
