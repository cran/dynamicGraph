\name{GraphLatticeProto-class}
\docType{class}
\alias{GraphLatticeProto-class}

\title{Class "GraphLatticeProto" }
\description{  The class for dynamic graph lattice. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GraphLatticeProto", ...)}.
The object is the returned value from \code{\link{dynamicGraphMain}}.
}
\section{Slots}{
  \describe{
    \item{\code{vertices}:}{Object of class \code{"list"}:
        The \code{vertexList} of the graph window. }
    \item{\code{blocks}:}{Object of class \code{"list"}:
        The \code{blockList} of the graph window. }
    \item{\code{blockTree}:}{Object of class \code{"list"}:
        The \code{blockTree} of the graph window. }
    \item{\code{graphs}:}{Object of class \code{"list"}:
        A list of objects, each of class \code{\link{CanvasProto-class}}. }
  }
}
\section{Methods}{
No methods defined with class "GraphLatticeProto" in the signature.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{dynamicGraphMain}},
\code{\link{CanvasProto-class}}.
}
% \examples{ }
\keyword{classes}
