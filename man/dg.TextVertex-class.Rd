\name{dg.TextVertex-class}

\docType{class}

\alias{dg.TextVertex-class}
\alias{draw,dg.TextVertex-method}

\title{Class "dg.TextVertex"}

\description{ The class for vertices for setting text string
	      in the graph window.
              These vertices are given by the argument \code{extraList}
              to \code{\link{dynamicGraphMain}}. }

\section{Objects from the Class}{
The vertices of nodes of this class are drawn with very small symbols.
Objects has the methods for extracting and setting the slots for vertices.

% Objects can be created by calls of the form \code{new("dg.TextVertex", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} 
       with the \code{name} of the vertex. }
    \item{\code{index}:}{Object of class \code{"numeric"} 
       with the \code{index} of the vertex,
       the position of the vertex in the extra list. }
    \item{\code{position}:}{Object of class \code{"numeric"} 
       with the \code{position} of the vertex.
       Vertices in the same dynamic graph should
       have the same number of coordinates. 
       A small dot is placed at the position of the vertex. 
       This dot can be move outside the window. }
    \item{\code{blockindex}:}{Object of class \code{"numeric"}
       with the \code{blockindex} of the vertex. }
    \item{\code{stratum}:}{Object of class \code{"numeric"} 
       with the \code{stratum} of the vertex. }
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
Class \code{"dg.Vertex"}, directly.
Class \code{"dg.Node"}, by class \code{"dg.Vertex"}.
}
\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "dg.TextVertex")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{newVertex}},
\code{\link{returnVertexList}},
\code{\link{dg.Node-class}}, and
\code{\link{dg.ExtraEdge-class}}.
}

\keyword{classes}
