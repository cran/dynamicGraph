\name{LinearGeneratorProto-class}
\docType{class}
\alias{LinearGeneratorProto-class}
\alias{draw,LinearGeneratorProto-method}

\title{Class "LinearGeneratorProto" ~~~ }
\description{  The class for factor vertices for linear terms. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("LinearGeneratorProto", ...)}.
}
\section{Slots}{
  \describe{
%    \item{\code{test.position}:}{Object of class \code{"numeric"}, see\code{"FactorVertexProto"}. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}, see\code{"FactorVertexProto"}. }
    \item{\code{name}:}{Object of class \code{"character"}, see\code{"VertexProto"}. }
    \item{\code{index}:}{Object of class \code{"numeric"}, see\code{"VertexProto"}. }
    \item{\code{position}:}{Object of class \code{"numeric"}, see\code{"VertexProto"}. }
    \item{\code{stratum}:}{Object of class \code{"numeric"}, see\code{"VertexProto"}. }
    \item{\code{color}:}{Object of class \code{"character"}, see\code{"VertexProto"}. }
    \item{\code{label}:}{Object of class \code{"character"}, see\code{"VertexProto"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, see\code{"VertexProto"}. }
  }
}
\section{Extends}{
Class \code{"FactorVertexProto"}, directly.
Class \code{"VertexProto"}, by class \code{"FactorVertexProto"}.
Class \code{"NodeProto"}, by class \code{"FactorVertexProto"}.
}
\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "LinearGeneratorProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newFactor}},
\code{\link{returnFactorVerticesAndEdges}},
\code{\link{FactorVertexProto-class}},
\code{\link{VertexProto-class}},
\code{\link{NodeProto-class}}.
}
% \examples{ }
\keyword{classes}
