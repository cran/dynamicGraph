\name{EdgeProto-class}
\docType{class}
\alias{EdgeProto-class}
\alias{draw,EdgeProto-method}
\alias{edgeLabel<-,EdgeProto-method}
\alias{edgeLabel,EdgeProto-method}
\alias{edgeWidth<-,EdgeProto-method}
\alias{edgeWidth,EdgeProto-method}
\alias{labelOfEdge<-,EdgeProto-method}
\alias{labelOfEdge,EdgeProto-method}
\alias{name,EdgeProto-method}
\alias{nodeIndicesOfEdge<-,EdgeProto-method}
\alias{nodeIndicesOfEdge,EdgeProto-method}
% \alias{vertexIndices<-,EdgeProto-method}
% \alias{vertexIndices,EdgeProto-method}
\alias{width<-,EdgeProto-method}
\alias{width,EdgeProto-method}
\alias{widthOfEdge<-,EdgeProto-method}
\alias{widthOfEdge,EdgeProto-method}

\title{Class "EdgeProto" ~~~ }
\description{  A skeleton class for the classes of edges. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EdgeProto", ...)}.
Objects has the methods for extracting and setting the slots for edges,
and the method for drawing the edge.
}
\section{Slots}{
  \describe{
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}:
       The \code{vertex.indices} of the vertices of the edge. }
    \item{\code{width}:}{Object of class \code{"numeric"}:
       The \code{width} of the edge. }
    \item{\code{color}:}{Object of class \code{"character"}:
       The \code{color} of the edge. }
    \item{\code{label}:}{Object of class \code{"character"}:
       The \code{label} of the edge. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}:
       The \code{label.position} of the edge. }
  }
}
\section{Extends}{
Class \code{"NodeProto"}, directly.
}
\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "EdgeProto")}: ... }
    \item{edgeLabel<-}{\code{signature(x = "EdgeProto")}:    Same as \code{labelOfEdge<-}. }
    \item{edgeLabel}{\code{signature(object = "EdgeProto")}: Same as \code{labelOfEdge}.   }
    \item{edgeWidth<-}{\code{signature(x = "EdgeProto")}:    Same as \code{widthOfEdge<-}. }
    \item{edgeWidth}{\code{signature(object = "EdgeProto")}: Same as \code{widthOfEdge}.   }
    \item{labelOfEdge<-}{\code{signature(x = "EdgeProto")}: 
       Same as the method \code{label<-}. 
       Set the \code{label} of the edge. }
    \item{labelOfEdge}{\code{signature(object = "EdgeProto")}: 
       Same as the method \code{label}. 
       Return the \code{label} of the edge. }
    \item{name}{\code{signature(object = "EdgeProto")}: 
       Return the \code{name}, equal to the \code{label}, of the edge. }
    \item{nodeIndicesOfEdge<-}{\code{signature(x = "EdgeProto")}:
       Set the \code{indices} of the vertices of the edge. }
    \item{nodeIndicesOfEdge}{\code{signature(object = "EdgeProto")}:
       Return the \code{indices} of the vertices of the edge. }
%     \item{vertexIndices<-}{\code{signature(x = "EdgeProto")}: ... }
%     \item{vertexIndices}{\code{signature(object = "EdgeProto")}: ... }
    \item{width<-}{\code{signature(x = "EdgeProto")}:
       Set the \code{width} of the edge. }
    \item{width}{\code{signature(object = "EdgeProto")}:
       Return the \code{width} of the edge. }
    \item{widthOfEdge<-}{\code{signature(x = "EdgeProto")}: 
       Same as the method \code{width<-}. 
       Set the \code{width} of the edge. }
    \item{widthOfEdge}{\code{signature(object = "EdgeProto")}: 
       Same as the method \code{width}. 
       Return the \code{width} of the edge. }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{VertexEdgeProto-class}},
\code{\link{BlockEdgeProto-class}},
\code{\link{FactorEdgeProto-class}}.
}
% \examples{ }
\keyword{classes}
