
# Test with block recursive model, Pedegree.Triple.R":

source("startup.0.R")

Block.tree <-
  list(g = 0, G = 54, label = "Pedegree.Triple.R",

       Female.1 = 
       list(g = 0, G = 33,
            Paternal = 
            list(g = 0, G = 12,
                 Paternal.G.F = list(Vertices = c("P.G.Father.1")),
                 Paternal.G.M = list(Vertices = c("P.G.Mother.1")),
                 common.children = list(g = 0, label = "Father",
                                        Vertices = c("Father.1"))),
            Maternal = 
            list(g = 0, G = 12,
                 Maternal.G.F = list(Vertices = c("M.G.Father.1")),
                 Maternal.G.M = list(Vertices = c("M.G.Mother.1")),
                 common.children = list(g = 0, label = "Mother",
                                        Vertices = c("Mother.1"))),
            common.children = list(g = 2, Vertices = c("Female.1"))),

       Male = 
       list(g = 0, G = 33,
            Paternal = 
            list(g = 0, G = 12,
                 Paternal.G.F = list(Vertices = c("P.G.Father")),
                 Paternal.G.M = list(Vertices = c("P.G.Mother")),
                 common.children = list(g = 0, label = "Father",
                                        Vertices = c("Father"))),
            Maternal = 
            list(g = 0, G = 12,
                 Maternal.G.F = list(Vertices = c("M.G.Father")),
                 Maternal.G.M = list(Vertices = c("M.G.Mother")),
                 common.children = list(g = 0, label = "Mother",
                                        Vertices = c("Mother"))),
            common.children = list(g = 2, Vertices = c("Male"))),

       Female.2 = 
       list(g = 0, G = 33,
            Paternal = 
            list(g = 0, G = 12,
                 Paternal.G.F = list(Vertices = c("P.G.Father.2")),
                 Paternal.G.M = list(Vertices = c("P.G.Mother.2")),
                 common.children = list(g = 0, label = "Father",
                                        Vertices = c("Father.2"))),
            Maternal = 
            list(g = 0, G = 12,
                 Maternal.G.F = list(Vertices = c("M.G.Father.2")),
                 Maternal.G.M = list(Vertices = c("M.G.Mother.2")),
                 common.children = list(g = 0, label = "Mother",
                                        Vertices = c("Mother.2"))),
            common.children = list(g = 2, Vertices = c("Female.2"))),

  common.children = list(Vertices = c("Marriage.1", "Marriage.2"), g = 3, 
    Son.1 = list(Vertices = c("Son.1"), g = 3, 
       P.G.Son.1 = list(Vertices = c("P.G.Son.1")),
       P.G.Dat.1 = list(Vertices = c("P.G.Dat.1"))),
    Dat.1 = list(Vertices = c("Dat.1"), g = 3,
       M.G.Son.1 = list(Vertices = c("M.G.Son.1")),
       M.G.Dat.1 = list(Vertices = c("M.G.Dat.1"))),
    Son.2 = list(Vertices = c("Son.2"), g = 3, 
       P.G.Son.2 = list(Vertices = c("P.G.Son.2")),
       P.G.Dat.2 = list(Vertices = c("P.G.Dat.2"))),
    Dat.2 = list(Vertices = c("Dat.2"), g = 3,
       M.G.Son.2 = list(Vertices = c("M.G.Son.2")),
       M.G.Dat.2 = list(Vertices = c("M.G.Dat.2")))
    )
)

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)                   

FromTo <- matrix(c("P.G.Father",   "Father",
                   "P.G.Mother",   "Father",
                   "M.G.Father",   "Mother",
                   "M.G.Mother",   "Mother",
                   "Father",       "Male",
                   "Mother",       "Male",
                   "P.G.Father.1", "Father.1",
                   "P.G.Mother.1", "Father.1",
                   "M.G.Father.1", "Mother.1",
                   "M.G.Mother.1", "Mother.1",
                   "Father.1",     "Female.1",
                   "Mother.1",     "Female.1",
                   "P.G.Father.2", "Father.2",
                   "P.G.Mother.2", "Father.2",
                   "M.G.Father.2", "Mother.2",
                   "M.G.Mother.2", "Mother.2",
                   "Father.2",     "Female.2",
                   "Mother.2",     "Female.2",
                   "Male",         "Marriage.1",
                   "Female.1",     "Marriage.1",
                   "Male",         "Marriage.2",
                   "Female.2",     "Marriage.2",
                   "Marriage.1",   "Son.1",
                   "Marriage.1",   "Dat.1",
                   "Marriage.2",   "Son.2",
                   "Marriage.2",   "Dat.2",
                   "Son.1",        "P.G.Son.1",
                   "Son.1",        "P.G.Dat.1",
                   "Dat.1",        "M.G.Son.1",
                   "Dat.1",        "M.G.Dat.1",
                   "Son.2",        "P.G.Son.2",
                   "Son.2",        "P.G.Dat.2",
                   "Dat.2",        "M.G.Son.2",
                   "Dat.2",        "M.G.Dat.2"), byrow = T, ncol = 2)

From <- match(FromTo[,1], V.Names)
To   <- match(FromTo[,2], V.Names)

V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, 
                  block.tree = Block.tree, object = Object,
                  width = 600, height = 600, 
                  drawblocks = TRUE, drawBlockFrame = FALSE, 
                  drawBlockBackground = TRUE,
                  overlaying = TRUE, blockColors = rev(blockColors.blue),
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  UserMenus = Menus, title = "Pedegree.Triple")
