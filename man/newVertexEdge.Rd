\name{newVertexEdge}
\alias{EdgeProto}
\alias{VertexEdgeProto}
\alias{newVertexEdge}
\alias{labelOfEdge}
\alias{widthOfEdge}
\alias{labelOfEdge<-}
\alias{widthOfEdge<-}
\alias{edgeLabel}
\alias{edgeWidth}
\alias{edgeLabel<-}
\alias{edgeWidth<-}
\alias{nodeIndicesOfEdge}
\alias{nodeIndicesOfEdge<-}
\alias{nodeTypesOfEdge}
% \alias{nodeTypesOfEdge<-}
\alias{width}
\alias{width<-}
\alias{oriented}
\alias{oriented<-}
\title{The edge object}
\description{Returns an object of the class \code{EdgeProto}
  for an edge between vertices. }
\usage{
newVertexEdge(vertex.indices, vertices = NULL, width = 2,
              color = "DarkOliveGreen", oriented = FALSE, type = NULL,
              label = ifelse(is.null(vertices), "",
                             paste(Labels(vertices), collapse = "~")))
}
\arguments{
  \item{vertex.indices}{ Integer vector of the \code{vertex.indices} 
      of the vertices of the edge. These are the indices of the
      vertices in the list of vertices. }
  \item{vertices}{ All the \code{vertices} of the edge, and only the
     vertices of the edge. Currently only used for setting the default label. }
  \item{width}{ Numeric, for the \code{width} of the edge. }
  \item{color}{ Text string for the \code{color} of the edge. }
  \item{oriented}{ Logical, TRUE if the edge is \code{oriented},
      also when no blocks are used in \code{\link{dynamicGraphMain}}. }
  \item{type}{ Text string for the \code{type} of the edge. 
      Currently not used. 
      Intended to set which class the edge should be of
      containing the  class \code{EdgeProto}.}
  \item{label}{ Text string for the \code{label} of the edge.
                By default set by concatenating the labels of the vertices. }
}
\details{
    Edges are not constrained to have two vertices.
}
\value{A object of class \code{EdgeProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{
    The \code{EdgeProto} class has
    beside the methods of \code{\link{VertexProto}}
    the methods \code{\link{oriented}} and \code{\link{oriented<-}}.

    The methods
    \code{\link{labelOfEdge}}, \code{\link{widthOfEdge}},
    \code{\link{labelOfEdge<-}}, \code{\link{widthOfEdge<-}}
    are available because of some problems with the methods
    \code{\link{label}}, \code{\link{width}},
    \code{\link{label<-}} and \code{\link{width<-}} 
    for edges and \code{NAMESPACES}.
    These problems might be solved, and the methods
    \code{...OfEdge} will later be removed.

    The method \code{\link{nodeIndicesOfEdge}} will extract the indices of the
    vertices of the edge, and the method \code{\link{nodeTypesOfEdge}} will
    extract the types
    ("super classes": \code{vertex}, \code{factor} or \code{block})
    of the vertices (nodes) of an edge.
    The method \code{\link{draw}} is used to draw the edge,
    and items are added to the pop up menu of an edge
    by the method \code{\link{addToPopups}}.

    Some of these methods also applies for
    block edges (\code{BlockEdgeProto})
    and factor edges (\code{FactorEdgeProto}).
}
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
e <- newVertexEdge(c(1, 2, 3), vertices = vertices[1:3])

str(e)

color(e)
labelOfEdge(e)
label(e)
labelPosition(e)
widthOfEdge(e)
width(e)
oriented(e)
nodeIndicesOfEdge(e)
nodeTypesOfEdge(e)

color(e) <- "Black"
labelOfEdge(e) <- "v1-v2"
label(e) <- "1-2"
labelPosition(e) <- c(10, 20, 30)
widthOfEdge(e) <- 0
width(e) <- 1
oriented(e) <- TRUE
nodeIndicesOfEdge(e) <- c(1, 2)

str(e)
}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
