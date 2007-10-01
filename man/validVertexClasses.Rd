\name{validVertexClasses}

\alias{vertexClasses}
\alias{validVertexClasses}

\title{Valid vertex classes}

\description{Return matrix with labels of valid vertex classes and the valid vertex classes}

\usage{validVertexClasses()}

\details{The argument \code{vertexClasses} to the functions
    \code{\link{dynamicGraphMain}} and \code{\link{DynamicGraph}}, and to
    \code{\link{dg.Vertex-class}} and \code{\link{returnVertexList}}
    is by default the returned value of this
    function. If new vertex classes are created then \code{vertexClasses}
    should be set to a value with this returned value extended appropriate. }
\value{Matrix of text strings with labels (used in dialog windows)
       of valid vertex classes
       and the valid vertex classes (used to create the vertices).}

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{validEdgeClasses}}. }

\examples{
require(tcltk)

# Test with new vertex class (demo(Circle.newVertex)):

setClass("NewVertex", contains = c("dg.Node", "dg.Vertex"),
         representation(my.text   = "character",
                        my.number = "numeric"), 
         prototype(my.text    = "",
                   my.number  = 2))

myVertexClasses <- rbind(validVertexClasses(), 
                         NewVertex = c("NewVertex", "NewVertex"))

setMethod("draw", "NewVertex",
          function(object, canvas, position,
                   x = position[1], y = position[2], stratum = 0,
                   w = 2, color = "green", background = "white")
          {
            s <- w * sqrt(4 / pi) / 2
            p1 <- tkcreate(canvas, "oval",
                           x - s - s, y - s,
                           x + s - s, y + s,
                           fill = color(object), activefill = "IndianRed")
            p2 <- tkcreate(canvas, "oval",
                           x - s + s, y - s,
                           x + s + s, y + s, 
                           fill = color(object), activefill = "IndianRed")
            p3 <- tkcreate(canvas, "oval",
                           x - s, y - s - s,
                           x + s, y + s - s, 
                           fill = color(object), activefill = "IndianRed")
            p4 <- tkcreate(canvas, "poly", 
                           x - 1.5 * s, y + 3 * s,
                           x + 1.5 * s, y + 3 * s, 
                           x, y, 
                           fill = color(object), activefill = "SteelBlue")
            return(list(dynamic = list(p1, p2, p3, p4), fixed = NULL)) })

setMethod("addToPopups", "NewVertex",
          function(object, type, nodePopupMenu, i,
			   updateArguments, Args, ...)
          {
               tkadd(nodePopupMenu, "command",
                     label = paste(" --- This is a my new vertex!"),
                     command = function() { print(name(object))})
          })

if (!isGeneric("my.text")) {
  if (is.function("my.text"))
    fun <- my.text
  else
    fun <- function(object) standardGeneric("my.text")
  setGeneric("my.text", fun)
}
setGeneric("my.text<-",
           function(x, value) standardGeneric("my.text<-"))

setMethod("my.text", "NewVertex",
          function(object) object@my.text)
setReplaceMethod("my.text", "NewVertex",
                 function(x, value) {x@my.text <- value; x} )

if (!isGeneric("my.number")) {
  if (is.function("my.number"))
    fun <- my.number
  else
    fun <- function(object) standardGeneric("my.number")
  setGeneric("my.number", fun)
}
setGeneric("my.number<-",
           function(x, value) standardGeneric("my.number<-"))

setMethod("my.number", "NewVertex",
          function(object) object@my.number)
setReplaceMethod("my.number", "NewVertex",
                 function(x, value) {x@my.number <- value; x} )


V.Types <- c(rep("NewVertex", 3), "Discrete", "Ordinal", "Continuous")
V.Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Labels <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

control <- dg.control(updateEdgeLabels = FALSE,
                      edgeColor = "green", vertexColor = "blue",
                      vertexClasses = myVertexClasses)

simpleGraph.Z.nV <- new("dg.simple.graph", vertex.names = V.Names, 
                        types = V.Types, labels = V.Labels,
                        from = From, to = To, texts = c("Gryf", "gaf"))

graph.Z.nV <- simpleGraphToGraph(simpleGraph.Z.nV, control = control)

Object <- NULL

Z.nV <- dg(graph.Z.nV, modelObject = Object, control = control, title = "Z")

}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
