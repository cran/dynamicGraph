\name{returnEdgeList}
\alias{vertexEdgeList}
\alias{returnEdgeList}
\alias{NodeTypes}
% \alias{NodeTypes<-}
\alias{NodeIndices}
% \alias{NodeIndices<-}
\alias{Widths}
\alias{Widths<-}
\alias{Oriented}
\alias{Oriented<-}

\alias{NodeIndices,list-method}
\alias{NodeTypes,list-method}
\alias{Widths,list-method}
\alias{Widths<-,list-method}
\alias{Oriented,list-method}
\alias{Oriented<-,list-method}

\title{ The edge list }
\description{
  Return a list of edges, each of class containing \code{VertexEdgeProto}.
}
\usage{
returnEdgeList(edge.list, vertices, width = 2, color = "DarkSlateGrey",
               oriented = FALSE, type = NULL)
}
\arguments{
  \item{edge.list}{ A list of vectors identifying the edges.
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or 
                    a vector of text strings with the names of the vertices.}
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{VertexProto}.
     \code{vertices} are used to set the initial labels of the edges.}
  \item{width}{ A single numeric with the initial \code{width} of the edges. }
  \item{color}{ A single text string giving the \code{color} of the edges. }
  \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{type}{ A single text string giving the \code{type} of the edges. 
     The label \code{type} of an edge class is currently not used, but
     intended to identify which class the edges should be of containing
     the \code{VertexEdgeProto}.} 
}
% \details{}
\value{
     A list of edges, each of class containing \code{VertexEdgeProto}.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{
    Beside the methods of the vertex list, \code{\link{vertexList}},
    (except \code{\link{Positions}}, \code{\link{Indices}}
    and \code{\link{Strata}}) the edge list also has the methods
    \code{\link{NodeTypes}},
    % \code{\link{NodeTypes<-}},
    \code{\link{NodeIndices}},
    % \code{\link{NodeIndices<-}},
    \code{\link{Widths}},
    \code{\link{Widths<-}},
    \code{\link{Oriented}} and
    \code{\link{Oriented<-}}. 
}
\seealso{\code{\link{vertexList}} and \code{\link{newVertexEdge}}.}
\examples{

from <- c("contry", "contry", "race", "race",       "sex",  "sex")
to   <- c(   "sex",   "race", "hair",  "eye", "education",  "age")
vertexnames <- unique(sort(c(from, to)))
vertices <- returnVertexList(vertexnames)
# from <- match(from, vertexnames)
# to <- match(to, vertexnames)
edge.list <- vector("list", length(to))
for (j in seq(along = to)) edge.list[[j]] <- c(from[j], to[j])
edges <- returnEdgeList(edge.list, vertices, color = "red", oriented = TRUE)

Names(edges)
Colors(edges)
Labels(edges)
LabelPositions(edges)
# Positions(edges)
# Strata(edges)
# Indices(edges)
str(NodeTypes(edges))
str(NodeIndices(edges))
Widths(edges)
Oriented(edges)
Widths(edges) <- rep(1, 7)
Widths(edges) <- rep(1, 6)
Widths(edges)

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
