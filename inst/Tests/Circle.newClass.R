
# Test with new vertex class:

source("startup.0.R")

setClass("NewVertexProto", contains = "VertexProto")

myVertexClasses <- rbind(validVertexClasses(), 
                         c("NewVertex", "NewVertexProto"))

setMethod("draw", "NewVertexProto",
          function(object, canvas, position,
                   x = position[1], y = position[2], stratum = 0,
                   w = 2, color = "green", background = "white")
          {
            s <- w * sqrt(4 / pi) / 2
            p1 <- tkcreate(canvas, "oval",
                           x - s - s, y - s,
                           x + s - s, y + s, 
                           fill = color(object))
            p2 <- tkcreate(canvas, "oval",
                           x - s + s, y - s,
                           x + s + s, y + s, 
                           fill = color(object))
            p3 <- tkcreate(canvas, "oval",
                           x - s, y - s - s,
                           x + s, y + s - s, 
                           fill = color(object))
            p4 <- tkcreate(canvas, "poly", 
                           x - 1.5 * s, y + 3 * s,
                           x + 1.5 * s, y + 3 * s, 
                           x, y, 
                           fill = color(object))
            return(list(dynamic = list(p1, p2, p3, p4), fixed = NULL)) })

setMethod("addToPopups", "NewVertexProto",
          function(object, type, nodePopupMenu, ...)
          {
               tkadd(nodePopupMenu, "command",
                     label = paste(" --- This is a my new vertex!"),
                     command = function() { print(name(object))})
          })

# Why are these 2 * 7 methods not avaliable from "VertexProto" ?

setMethod("color", "NewVertexProto",
          function(object) object@color)
setReplaceMethod("color", "NewVertexProto",
                 function(x, value) {x@color <- value; x} )

setMethod("label", "NewVertexProto",
          function(object) object@label)
setReplaceMethod("label", "NewVertexProto",
                 function(x, value) {x@label <- value; x} )

setMethod("labelPosition", "NewVertexProto",
          function(object) object@label.position)
setReplaceMethod("labelPosition", "NewVertexProto",
                 function(x, value) {x@label.position <- value; x} )

setMethod("name", "NewVertexProto",
          function(object) object@name)
setReplaceMethod("name", "NewVertexProto",
                 function(x, value) {x@name <- value; x} )

setMethod("index", "NewVertexProto",
          function(object) object@index)
setReplaceMethod("index", "NewVertexProto",
                 function(x, value) {x@index <- value; x} )

setMethod("position", "NewVertexProto", 
          function(object) object@position)
setReplaceMethod("position", "NewVertexProto",
                 function(x, value) {x@position <- value; x} )

setMethod("stratum", "NewVertexProto",
          function(object) object@stratum)
setReplaceMethod("stratum", "NewVertexProto",
                 function(x, value) {x@stratum <- value; x} )


V.Types <- rep("NewVertex", 6)

V.Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Names <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

Z <- DynamicGraph(V.Names, V.Types, From, To, texts = c("Gryf", "gaf"),
                  object = Object, UserMenus = Menus, 
                  updateEdgeLabels = FALSE,
                  edgeColor = "green", vertexColor = "blue", 
                  debug.strata = debug.strata, debug.edges = debug.edges, 
                  debug.position = debug.position, debug.update = debug.update,
                  vertexClasses = myVertexClasses)
