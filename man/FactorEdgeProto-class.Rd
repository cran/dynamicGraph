\name{FactorEdgeProto-class}
\docType{class}
\alias{FactorEdgeProto-class}
\alias{nodeTypesOfEdge,FactorEdgeProto-method}

\title{Class "FactorEdgeProto" }
\description{  The class for the bipartite graph edges between vertices and factors. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FactorEdgeProto", ...)}.
}
\section{Slots}{
  \describe{
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
    \item{nodeTypesOfEdge}{\code{signature(object = "FactorEdgeProto")}:
      Extract the types ("super classes": \code{"Vertex"} or \code{"Factor"})
      of the vertices (nodes) of the edge. }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newFactorEdge}},
\code{\link{returnFactorEdgeList}},
\code{\link{EdgeProto-class}}.
}
% \examples{ }
\keyword{classes}
