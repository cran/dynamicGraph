\name{VertexEdgeProto-class}
\docType{class}
\alias{VertexEdgeProto-class}
\alias{nodeTypesOfEdge,VertexEdgeProto-method}
\alias{oriented<-,VertexEdgeProto-method}
\alias{oriented,VertexEdgeProto-method}

\title{Class "VertexEdgeProto" }
\description{  The class for edges between vertices. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VertexEdgeProto", ...)}.
Objects has the methods for extracting and setting the slots for edges,
and the method for drawing the edge.
}
\section{Slots}{
  \describe{
    \item{\code{oriented}:}{Object of class \code{"logical"}.
       If TRUE, then the edge is \code{oriented},
       and thus drawn by an \code{arrow}, also when no blocks are pressent. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}, see \code{"EdgeProto"}. }
    \item{\code{width}:}{Object of class \code{"numeric"}, see \code{"EdgeProto"}. }
    \item{\code{color}:}{Object of class \code{"character"}, see \code{"EdgeProto"}. }
    \item{\code{label}:}{Object of class \code{"character"}, see \code{"EdgeProto"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, see \code{"EdgeProto"}. }
  }
}
\section{Extends}{
Class \code{"EdgeProto"}, directly.
Class \code{"NodeProto"}, by class \code{"EdgeProto"}.
}
\section{Methods}{
  \describe{
    \item{nodeTypesOfEdge}{\code{signature(object = "VertexEdgeProto")}: ... }
    \item{oriented<-}{\code{signature(x = "VertexEdgeProto")}: ... }
    \item{oriented}{\code{signature(object = "VertexEdgeProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newVertexEdge}},
\code{\link{returnEdgeList}},
\code{\link{EdgeProto-class}}.
}
% \examples{ }
\keyword{classes}
