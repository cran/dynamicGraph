\name{VertexProto-class}
\docType{class}
\alias{VertexProto-class}
\alias{ancestors<-,VertexProto-method}
\alias{ancestors,VertexProto-method}
\alias{descendants<-,VertexProto-method}
\alias{descendants,VertexProto-method}
\alias{index<-,VertexProto-method}
\alias{index,VertexProto-method}
\alias{name<-,VertexProto-method}
\alias{name,VertexProto-method}
\alias{position<-,VertexProto-method}
\alias{position,VertexProto-method}
\alias{stratum<-,VertexProto-method}
\alias{stratum,VertexProto-method}
% \alias{vertexIndices<-,VertexProto-method}
% \alias{vertexIndices,VertexProto-method}
\alias{visible<-,VertexProto-method}
\alias{visible,VertexProto-method}

\title{Class "VertexProto" ~~~ }
\description{  A skeleton class for the classes of vertices. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VertexProto", ...)}.
Objects has the methods for extracting and setting the slots for vertices.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} 
       with the \code{name} of the vertex. 
       Should be a valid name on variables for your model object.}
    \item{\code{index}:}{Object of class \code{"numeric"} 
       with the \code{index} of the vertex,
       the position of the vertex in a vertex list. }
    \item{\code{position}:}{Object of class \code{"numeric"} 
       with the \code{position} of the vertex.
       Vertices in the same dynamic graph should
       have the same number of coordinates. }
    \item{\code{stratum}:}{Object of class \code{"numeric"} 
       with the \code{stratum} of the vertex.}
    \item{\code{color}:}{Object of class \code{"character"}
       with the \code{color} of the vertex. }
    \item{\code{label}:}{Object of class \code{"character"}
       with the \code{label} of the vertex. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}
       with the \code{label.position} of the vertex.
       Labels of vertices in the same dynamic graph
       should have the same number of coordinates. }
  }
}
\section{Extends}{
Class \code{"NodeProto"}, directly.
}
\section{Methods}{
  \describe{
    \item{ancestors<-}{\code{signature(x = "VertexProto")}: ... }
    \item{ancestors}{\code{signature(object = "VertexProto")}: ... }
    \item{descendants<-}{\code{signature(x = "VertexProto")}: ... }
    \item{descendants}{\code{signature(object = "VertexProto")}: ... }
    \item{index<-}{\code{signature(x = "VertexProto")}: ... }
    \item{index}{\code{signature(object = "VertexProto")}: ... }
    \item{name<-}{\code{signature(x = "VertexProto")}: ... }
    \item{name}{\code{signature(object = "VertexProto")}: ... }
    \item{position<-}{\code{signature(x = "VertexProto")}: ... }
    \item{position}{\code{signature(object = "VertexProto")}: ... }
    \item{stratum<-}{\code{signature(x = "VertexProto")}: ... }
    \item{stratum}{\code{signature(object = "VertexProto")}: ... }
%     \item{vertexIndices<-}{\code{signature(x = "VertexProto")}: ... }
%     \item{vertexIndices}{\code{signature(object = "VertexProto")}: ... }
    \item{visible<-}{\code{signature(x = "VertexProto")}: ... }
    \item{visible}{\code{signature(object = "VertexProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newVertex}},
\code{\link{returnVertexList}},
\code{\link{NodeProto-class}}.
}
% \examples{ }
\keyword{classes}
