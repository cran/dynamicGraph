\name{OrdinalVertexProto-class}
\docType{class}
\alias{OrdinalVertexProto-class}
\alias{draw,OrdinalVertexProto-method}

\title{Class "OrdinalVertexProto" ~~~ }
\description{  The class for vertices for ordinal variables. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OrdinalVertexProto", ...)}.
Objects has the methods for extracting and setting the slots for vertices,
and the method for drawing the vertex.
}
\section{Slots}{
  \describe{
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
Class \code{"VertexProto"}, directly.
Class \code{"NodeProto"}, by class \code{"VertexProto"}.
}
\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "OrdinalVertexProto")}: Method for drawing the vertex.
    The symbol will be a \code{filled square} for \code{ordinal} variable. }
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
