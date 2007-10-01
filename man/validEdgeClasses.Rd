\name{validEdgeClasses}

\alias{edgeClasses}
\alias{validEdgeClasses}

\title{Valid edge classes}

\description{Return matrix with labels of valid edge classes
and the valid edge classes}

\usage{validEdgeClasses()}

\details{The argument \code{edgeClasses} to the functions
    \code{\link{dynamicGraphMain}} and \code{\link{DynamicGraph}}, and to
    \code{\link{dg.VertexEdge-class}} and \code{\link{returnEdgeList}}
    is by default the returned value of this
    function. If new edge classes are created then \code{edgeClasses}
    should be set to a value with this returned value extended appropriate. }

\value{Matrix of text strings with labels (used in dialog windows) of valid 
       edge classes and the valid edge classes (used to create the edges).}

\author{Jens Henrik Badsberg}

\note{The "draw" method for an edge should return a list with the items 
      "lines", "tags", "from", "to", label" and "label.position".
      "lines" is the "tk"-objects for line objects between pairs of vertices,
      with coordinates at the vertices.
      "tags" is the "tk" -objects for objects between pairs of vertices,
      with coordinates at the middle of the two vertices.
 }

\seealso{ \code{\link{validVertexClasses}}. }

\examples{
require(tcltk)

# Test with new edge class (demo(Circle.newEdge)):

setClass("NewEdge", contains = c("dg.VertexEdge", "dg.Edge", "dg.Node"))

myEdgeClasses <- rbind(validEdgeClasses(), 
                       NewEdge = c("NewEdge", "NewEdge"))

setMethod("draw", "NewEdge",
          function(object, canvas, position,
                   x = lapply(position, function(e) e[1]),
                   y = lapply(position, function(e) e[2]),
                   stratum = as.vector(rep(0, length(position)),
                                       mode = "list"), 
                   w = 2, color = "green", background = "white", 
                   font.edge.label = "8x16")
          {
            f <- function(i, j) {
              dash <- "."
              arrowhead <- "both"
              l <- function(xi, yi, xj, yj)
                tkcreate(canvas, "line", xi, yi, xj, yj, width = w, 
                         arrow = arrowhead, dash = dash,
                         # arrowshape = as.list(c(2, 5, 3) * w),
                         fill = color(object), activefill = "DarkSlateGray")
              lines <- list(l(x[[i]], y[[i]], x[[j]], y[[j]]))
              label.position <- (position[[i]] + position[[j]]) / 2
              pos <- label.position + rep(0, length(label.position))
              label <- tkcreate(canvas, "text", pos[1], pos[2],
                                text = object@label, anchor = "nw", 
				font = "8x16", activefill = "DarkSlateGray")
              tags <- NULL
              x. <- mean(unlist(x))
              y. <- mean(unlist(y))
              s <- 4 * w * sqrt(4 / pi)
              p <- tkcreate(canvas, "rectangle",
			    x. - s, y. - s, x. + s, y. + s, 
                            fill = color(object), activefill = "SeaGreen")
              tags <- list(p)
              return(list(lines = lines, tags = tags,
                          from = object@vertex.indices[i],
                          to = object@vertex.indices[j],
                          label = label, label.position = label.position))
            }
            result <- NULL
            edge <- object@vertex.indices
            m <- length(edge)
            for (j in seq(along = edge))
              if (j < length(edge))
                for (k in (j+1):length(edge))
                    result <- append(result, list(f(j, k)))
            return(result) 
  })

setMethod("addToPopups", "NewEdge",
          function(object, type, nodePopupMenu, i,
			   updateArguments, Args, ...)
          {
               tkadd(nodePopupMenu, "command",
                     label = paste(" --- This is a my new vertex!"),
                     command = function() { print(name(object))})
          })



V.Types <- c("Discrete", "Ordinal", "Discrete",
             "Continuous", "Discrete", "Continuous")
V.Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Labels <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6, 3)
To   <- c(2, 3, 4, 5, 6, 1, 6)

control <- dg.control(updateEdgeLabels = FALSE,
                      edgeColor = "green", vertexColor = "blue",
                      edgeClasses = myEdgeClasses)

simpleGraph.Z.nE <- new("dg.simple.graph", vertex.names = V.Names, 
                        types = V.Types, labels = V.Labels,
                        from = From, to = To,
                        edge.types = c("NewEdge",
                                       "VertexEdge",
                                       "Dashed",
                                       "Dotted",
                                       "DoubleArrow",
                                       "DoubleConnected",
                                       "TripleConnected"), 
                        texts = c("Gryf", "gaf"))

graph.Z.nE <- simpleGraphToGraph(simpleGraph.Z.nE, control = control)

Object <- NULL

Z.nE <- dg(graph.Z.nE, modelObject = Object, control = control, title = "Z")

}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
