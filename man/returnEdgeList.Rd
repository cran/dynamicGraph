\name{returnEdgeList}

\alias{vertexEdgeList}
\alias{returnEdgeList}

\alias{dg.VertexEdgeList-class}

% \alias{NodeTypes}
% % \alias{NodeTypes<-}
% \alias{NodeIndices}
% % \alias{NodeIndices<-}
% \alias{Widths}
% \alias{Widths<-}
% \alias{Dashes}
% \alias{Dashes<-}
% \alias{Oriented}
% \alias{Oriented<-}

% \alias{NodeIndices,dg.list-method}
% \alias{NodeTypes,dg.list-method}
% \alias{Widths,dg.list-method}
% \alias{Widths<-,dg.list-method}
% \alias{Dashes,dg.list-method}
% \alias{Dashes<-,dg.list-method}
% \alias{Oriented,dg.list-method}
% \alias{Oriented<-,dg.list-method}

\title{Class "dg.VertexEdgeList": The edge list }
\description{
 Return a list of edges, each of class containing \code{dg.VertexEdge}.

 Objects can be created by calls of the form \code{new("dg.EdgeList", ...)}.
}
\usage{
returnEdgeList(edge.list, vertices, width = 2, color = "DarkSlateGrey", N = 3, 
               oriented = FALSE, types = NULL, edgeClasses = validEdgeClasses())
}
\arguments{
  \item{edge.list}{ A list of vectors identifying the edges.
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or 
                    a vector of text strings with the names of the vertices.}
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{dg.Vertex}.
     \code{vertices} are used to set the initial labels of the edges.}
  \item{width}{ A single numeric with the initial \code{width} of the edges. }
  \item{color}{ A single text string giving the \code{color} of the edges. }
  \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{types}{ A vector of text strings giving the \code{types} of the edges,
     identify which classes the edges should be of,
     containing the \code{dg.VertexEdge}.
     } 
  \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
  \item{edgeClasses}{ Returned value from \code{\link{validEdgeClasses}},
                      or extension of this matrix. }
}

\value{
     A list of edges, each of class containing \code{dg.VertexEdge}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
Class \code{"dg.EdgeList"}, directly.
Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.
Class \code{"dg.list"}, by class \code{"dg.EdgeList"}.
Class \code{"list"}, by class \code{"dg.EdgeList"}.
}

\section{Methods}{
No methods defined with class "dg.VertexEdgeList" in the signature.
}

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
    \code{\link{Dashes}},
    \code{\link{Dashes<-}},
    \code{\link{Oriented}}, and
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
Dashes(edges)
Widths(edges)
Oriented(edges)
Widths(edges) <- rep(1, 7)
Widths(edges) <- rep(1, 6)
Widths(edges)
asDataFrame(edges)
}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
