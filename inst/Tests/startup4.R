
require(tcltk)

source("source.R")

Object <- newDefaultModelObject("AnModelObject")

debug.strata   <- FALSE
debug.edges    <- FALSE
debug.position <- FALSE
debug.update   <- FALSE

V.Types <- c("Discrete", "Discrete", "Continuous", "Continuous")

V.Names <- c("Sex", "Age", "Eye", "Hair")
V.Names <- paste(V.Names, 1:4, sep ="/")

From <- c(1, 2, 3, 4)
To   <- c(2, 3, 4, 1)
