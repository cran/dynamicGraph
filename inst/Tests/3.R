
# Test of "link" and "add":

source("startup.R")

From <- c(1, 2, 3)
To   <- c(2, 3, 1)

Z <- DynamicGraph(V.Names[1:3], V.Types[1:3], From, To, object = Object, 
                  debug.edges = FALSE, UserMenus = Menus, returnLink = TRUE,
		  title = "Z", width = 150, height = 200, margin = 200)

From <- c(2, 3)
To   <- c(3, 1)

W <- DynamicGraph(from = From, to = To, 
                  object = Object, debug.edges = FALSE, 
                  UserMenus = Menus, returnLink = TRUE,
		  width = 150, height = 200, margin = 200, 
                  title = "W", frameModels = Z, addModel = TRUE)

V <- DynamicGraph(from = From, to = To, 
                  object = Object, debug.edges = FALSE, 
                  UserMenus = Menus, returnLink = TRUE,
		  width = 150, height = 200, margin = 200, 
                  frameModels = W, frameViews = W@models[[1]], 
                  title = "V", addView = TRUE)

From <- 1
To   <- 2

U <- DynamicGraph(from = From, to = To, 
                  object = Object, debug.edges = FALSE, 
                  UserMenus = Menus, returnLink = TRUE,
		  width = 150, height = 200, margin = 200, 
                  title = "U", frameModels = Z, 
                  frameViews = Z@models[[1]], 
                  graphWindow =  Z@models[[1]]@graphs[[1]],
                  addModel = TRUE, overwrite = TRUE)
