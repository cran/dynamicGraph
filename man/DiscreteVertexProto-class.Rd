\name{DiscreteVertexProto-class}
\docType{class}
\alias{DiscreteVertexProto-class}
\alias{draw,DiscreteVertexProto-method}

\title{Class "DiscreteVertexProto" }
\description{  The class for vertices for discrete variables. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DiscreteVertexProto", ...)}.
Objects has the methods for extracting and setting the slots for vertices,
and the method for drawing the vertex.
}
\section{Slots}{
  \describe{
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
    \item{draw}{\code{signature(object = "DiscreteVertexProto")}: Method for drawing the vertex.
    The symbol will be a \code{'d'ot} for \code{'d'iscrete} variable. }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newVertex}},
\code{\link{returnVertexList}},
\code{\link{VertexProto-class}}.
}
% \examples{ }
\keyword{classes}
