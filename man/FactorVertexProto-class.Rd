\name{FactorVertexProto-class}
\docType{class}
\alias{FactorVertexProto-class}
\alias{nodeIndices<-,FactorVertexProto-method}
\alias{nodeIndices,FactorVertexProto-method}
\alias{index<-,FactorVertexProto-method}
\alias{index,FactorVertexProto-method}

\title{Class "FactorVertexProto" }
\description{  A skeleton class for the classes of factor vertices. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FactorVertexProto", ...)}.
}
\section{Slots}{
  \describe{
%    \item{\code{test.position}:}{Object of class \code{"numeric"}:
%       The \code{position} of a test statistic for the factor. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}:
       The \code{vertex.indices} of the vertices of the factor. }
    \item{\code{name}:}{Object of class \code{"character"}, see \code{"VertexProto"}. }
    \item{\code{index}:}{Object of class \code{"numeric"}, see \code{"VertexProto"}. }
    \item{\code{position}:}{Object of class \code{"numeric"}, see \code{"VertexProto"}. }
    \item{\code{blockindex}:}{Object of class \code{"numeric"}, see \code{"VertexProto"}. }
    \item{\code{stratum}:}{Object of class \code{"numeric"}, see \code{"VertexProto"}. }
    \item{\code{color}:}{Object of class \code{"character"}, see \code{"VertexProto"}. }
    \item{\code{label}:}{Object of class \code{"character"}, see \code{"VertexProto"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, see \code{"VertexProto"}. }
  }
}
\section{Extends}{
Class \code{"VertexProto"}, directly.
Class \code{"NodeProto"}, by class \code{"VertexProto"}.
}
\section{Methods}{
  \describe{
    \item{nodeIndices<-}{\code{signature(x = "FactorVertexProto")}: ... }
    \item{nodeIndices}{\code{signature(object = "FactorVertexProto")}: ... }
    \item{index<-}{\code{signature(x = "FactorVertexProto")}: ... }
    \item{index}{\code{signature(object = "FactorVertexProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newFactor}},
\code{\link{returnFactorVerticesAndEdges}},
\code{\link{VertexProto-class}},
\code{\link{NodeProto-class}}.
}
% \examples{ }
\keyword{classes}
