
# Test with block recursive model:

source("startup.R")

source("colorlists.R")

Blocks <- list(Basic = c(2, 1), Intermediate = c(5, 4, 3),  Now = c(6))

V.Names <- paste(V.Names, c(1, 1, 2, 2, 2, 3), sep =":")

Z <- DynamicGraph(V.Names, V.Types, From, To, blocks = Blocks, object = Object,
                  width = 600, height = 600, drawblocks = TRUE, 
                  # drawBlockBackground = FALSE,
                  blockColors = blockColors.blue,
                  nested.blocks = TRUE, UserMenus = Menus)
