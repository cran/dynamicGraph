\name{CanvasProto-class}
\docType{class}
\alias{CanvasProto-class}

\title{Class "CanvasProto" }
\description{  The class for objects unique to each dynamic graph. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CanvasProto", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{top}:}{Object of class \code{"tkwin"}:
       The \code{tkwin}-top of the graph window. }
    \item{\code{canvas}:}{Object of class \code{"tkwin"}:
       The \code{canvas}-top of the graph window. }
    \item{\code{tags}:}{Object of class \code{"list"}:
       The tags of the objects drawn in the graph window. }
    \item{\code{id}:}{Object of class \code{"numeric"}:
       Integer, increased for each redraw of the graph window. }
    \item{\code{object}:}{Object of class \code{"list"}:
       The object of the graph. }
    \item{\code{visibleVertices}:}{Object of class \code{"numeric"}:
       Numeric vector of the indices of the vertices of
       \code{vertexList} plotted in the graph window. }
    \item{\code{extraVertices}:}{Object of class \code{"list"}:
       The \code{extraList} of the graph in the window. }
    \item{\code{graphEdges}:}{Object of class \code{"list"}:
       The \code{edgeList} of the graph in the window. }
    \item{\code{blockEdges}:}{Object of class \code{"list"}:
       The \code{blockEdgeList} of the graph in the window. }
    \item{\code{factorVertices}:}{Object of class \code{"list"}:
       The \code{factorVertexList} of the graph in the window. }
    \item{\code{factorEdges}:}{Object of class \code{"list"}:
       The \code{factorEdgeList} of the graph in the window. }
  }
}
\section{Methods}{
No methods defined with class "CanvasProto" in the signature.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{dynamicGraphMain}},
\code{\link{GraphLatticeProto-class}}.
}
% \examples{ }
\keyword{classes}
