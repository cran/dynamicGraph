
# A big tree with 49 vertices, in 4 "copies" of ancestor trees
# and 3 "copies" of offspring trees, Pedegree.functions:

source("startup.0.R")

ancestors <- function(name, i)
 list(g = 0, G = 31,
      Paternal = 
      list(g = 0, G = 11,
           Paternal.G.F = list(Vertices = paste(c("P.G.Father"), i, sep =".")),
           Paternal.G.M = list(Vertices = paste(c("P.G.Mother"), i, sep =".")),
           common.children = list(g = 0, label = "Father",
                                  Vertices = paste(c("Father"), i, sep ="."))),
      Maternal = 
      list(g = 0, G = 11,
           Maternal.G.F = list(Vertices = paste(c("M.G.Father"), i, sep =".")),
           Maternal.G.M = list(Vertices = paste(c("M.G.Mother"), i, sep =".")),
           common.children = list(g = 0, label = "Mother",
                                  Vertices = paste(c("Mother"), i, sep ="."))),
      common.children = list(g = 1, Vertices = name))

ancestors.edges <- function(i, name, offspring = "Marriage", j = c(1, 2))
 rbind(matrix(paste(c("P.G.Father", "Father",
                      "P.G.Mother", "Father",
                      "M.G.Father", "Mother",
                      "M.G.Mother", "Mother",
                      "Father",     name,
                      "Mother",     name),
              i, sep = "."), byrow = T, ncol = 2),
      matrix(c(rep(paste(name, i, sep = "."), length(j)), 
                   paste(offspring, j, sep = ".")), ncol = 2))

descendants <- function(i, offspring = "Marriage")
  list(Vertices = paste(offspring, i, sep = "."), g = 1, 
       Son = list(Vertices = paste(c("Son"), i, sep = "."), g = 1, 
           P.G.Son = list(Vertices = paste(c("P.G.Son"), i, sep =".")),
           P.G.Dat = list(Vertices = paste(c("P.G.Dat"), i, sep ="."))),
       Dat = list(Vertices = paste(c("Dat"), i, sep = "."), g = 1,
          M.G.Son = list(Vertices = paste(c("M.G.Son"), i, sep =".")),
          M.G.Dat = list(Vertices = paste(c("M.G.Dat"), i, sep ="."))))

descendants.edges <- function(i, offspring = "Marriage")
       matrix(paste(c(offspring, "Son",
                      offspring, "Dat",
                      "Son",     "P.G.Son",
                      "Son",     "P.G.Dat",
                      "Dat",     "M.G.Son",
                      "Dat",     "M.G.Dat"),
              i, sep = "."), byrow = T, ncol = 2)

Block.tree <-
  list(g = 0, G = 51, label = "Pedegree.functions",
       Female.1 = ancestors("Female.1", 1),
       Male.2   = ancestors("Male.2", 2),
       Female.3 = ancestors("Female.3", 3),
       Male.4   = ancestors("Male.4", 4),
       common.children = list(g = 0, label = "Offspring",
                              Marriage.1 = descendants(1),
                              Marriage.2 = descendants(2),
                              Marriage.3 = descendants(3))
    )

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)

FromTo <- rbind(ancestors.edges(1, "Female", j = 1),
                ancestors.edges(2, "Male"  , j = c(1, 2)),
                ancestors.edges(3, "Female", j = c(2, 3)),
                ancestors.edges(4, "Male"  , j = 3),
                descendants.edges(1),
                descendants.edges(2),
                descendants.edges(3))

From <- match(FromTo[,1], V.Names)
To   <- match(FromTo[,2], V.Names)

V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, 
                  block.tree = Block.tree, object = Object,
                  width = 600, height = 600, w = 2,
                  drawblocks = TRUE, drawBlockFrame = TRUE, 
                  overlaying = TRUE, # blockColors = rev(blockColors.blue),
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  UserMenus = Menus, title = "Pedegree.functions")
