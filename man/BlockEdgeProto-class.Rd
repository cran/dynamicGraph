\name{BlockEdgeProto-class}
\docType{class}
\alias{BlockEdgeProto-class}
\alias{nodeTypesOfEdge,BlockEdgeProto-method}
\alias{oriented<-,BlockEdgeProto-method}
\alias{oriented,BlockEdgeProto-method}

\title{Class "BlockEdgeProto" }
\description{  The class for edges between blocks and for edges between vertices and blocks. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BlockEdgeProto", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{oriented}:}{Object of class \code{"logical"}, see \code{"VertexEdgeProto"}. }
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
    \item{nodeTypesOfEdge}{\code{signature(object = "BlockEdgeProto")}:
       Extract the types ("super classes": \code{"Vertex"} or \code{"Block"})
       of the vertices (nodes) of the edge. }
    \item{oriented<-}{\code{signature(x = "BlockEdgeProto")}: ... }
    \item{oriented}{\code{signature(object = "BlockEdgeProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newBlockEdge}},
\code{\link{returnBlockEdgeList}},
\code{\link{EdgeProto-class}}.
}
% \examples{ }
\keyword{classes}
