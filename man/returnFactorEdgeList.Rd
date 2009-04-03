\name{returnFactorEdgeList}

\alias{factorEdgeList}
\alias{returnFactorEdgeList}

\alias{dg.FactorEdgeList-class}
\alias{initialize,dg.FactorEdgeList-method}

\title{Class dg.FactorEdgeList: The factor edge list }

\description{
 Return a list of factor edges, each of class \code{dg.FactorEdge}.

 Objects can be created by calls of the form
 \code{new("dg.FactorEdgeList", ...)}.
}

\usage{
returnFactorEdgeList(edge.list, vertices, factorvertices = NULL,
                     width = 2, color = "DarkSlateGrey", N = 3, 
                     % oriented = FALSE,
                     type = NULL)
}

\arguments{
  \item{edge.list}{ A list of vectors identifying the edges (between vertices).
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or
                    a vector of text strings with the names of the vertices. }
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{dg.Vertex}. }
  \item{factorvertices}{ The list of \code{factorvertices}, 
      each of a class containing \code{dg.FactorVertex}. }
  \item{width}{ A numeric with the initial \code{width} of the factor edges. }
  \item{color}{ A text string giving the initial \code{color}
        of the factor edges. }
  % \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
  \item{type}{ A text string giving the \code{type} of the factor edges. }
}

\value{
    A list of edges, each of class \code{dg.FactorEdge}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
% Class \code{"dg.EdgeList"}, directly.
% Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.
% Class \code{"dg.list"}, by class \code{"dg.EdgeList"}.
% Class \code{"list"}, by class \code{"dg.EdgeList"}.

% Class \code{"list"}, from data part.
% Class \code{"dg.list"}, directly.
% Class \code{"dg.EdgeList"}, directly.
% Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.

Class \code{"dg.EdgeList"}, directly.
Class \code{"dg.list"}, directly.
Class \code{"list"}, from data part.
Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.
% Class \code{"vector"}, by class \code{"dg.EdgeList"}.
% Class \code{"vector"}, by class \code{"dg.EdgeList"}.
Class \code{"vector"}, by class \code{"dg.EdgeList"}.
Class \code{"vector"}, by class \code{"dg.list"}.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "dg.FactorEdgeList")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\note{ 
    The methods of the edge list, \code{\link{returnEdgeList}},
    also applies for factor edge lists.

    No example is given here since the factor edge list usually
    will be returned by the function 
    \code{\link{returnFactorVerticesAndEdges}}.
    The application is similar to \code{\link{returnBlockEdgeList}},
    see example here.

 }

\seealso{
\code{\link{returnFactorVerticesAndEdges}}
 and \code{\link{returnBlockEdgeList}}. }

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
