
require(tcltk)

require(dynamicGraph)

require(CoCoCg)

source("usermenus.R")

rats.data <- read.table("rats.txt", header = TRUE)

rats.data; dimnames(rats.data); class(rats.data); dim(rats.data)

library(CoCoCg)

rats <- makeCoCoCg(title = "Source: Morrison (1976). Multivariate Statistical Methods, McGraw-Hill. a: Sex; b: Treatment; x: Wt loss 1; y: Wt loss 2")

result <- enterDataFrame(rats.data, to.factor = 1:2)

ratsMainEffects <- makeModel(".;")
ratsMainFull <- makeModel("*;")

dynamic.Graph(ratsMainEffects, UserMenus = Menus, 
              vertexColor = "blue", title = "ratsMainEffects")
dynamic.Graph(ratsMainFull, UserMenus = Menus, 
              vertexColor = "black", title = "ratsMainFull")
