\name{returnExtraEdgeList}

\alias{extraEdgeList}
\alias{returnExtraEdgeList}

\alias{dg.ExtraEdgeList-class}

\title{Class "dg.ExtraEdgeList": The extra edge list }

\description{
 Return a list of extra edges, each of class \code{dg.ExtraEdge}.

 Objects can be created by calls of the form
 \code{new("dg.ExtraEdgeList", ...)}.
}

\usage{
returnExtraEdgeList(edge.list, vertices, extravertices = NULL,
                    width = 2, color = "DarkSlateGrey", type = NULL)
}

\arguments{
  \item{edge.list}{ A list of vectors identifying the edges (between vertices).
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge,
		    positive for vertices, negative for extra vertices, or
                    a vector of text strings with the names of the vertices. }
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{dg.Vertex}. }
  \item{extravertices}{ The list of \code{extravertices}, 
      each of a class containing \code{dg.ExtraVertex}. }
  \item{width}{ A numeric with the initial \code{width} of the extra edges. }
  \item{color}{ A text string giving the initial \code{color}
        of the extra edges. }
  % \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{type}{ A text string giving the \code{type} of the extra edges. }
}

\value{
    A list of edges, each of class \code{dg.ExtraEdge}.
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
No methods defined with class "dg.ExtraEdgeList" in the signature.
}

\author{Jens Henrik Badsberg}

\note{ 
    The methods of the edge list, \code{\link{returnEdgeList}},
    also applies for extra edge lists.

    The application is similar to \code{\link{returnBlockEdgeList}},
    see example here.
 }

\seealso{
\code{\link{returnFactorEdgeList}}
 and \code{\link{returnBlockEdgeList}}. }

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
