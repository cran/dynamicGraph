\name{dg.Edge-class}

\docType{class}

\alias{dg.Edge-class}
\alias{draw,dg.Edge-method}
\alias{initialize,dg.Edge-method}
\alias{name,dg.Edge-method}
\alias{nodeIndicesOfEdge<-,dg.Edge-method}
\alias{nodeIndicesOfEdge,dg.Edge-method}
\alias{label<-,dg.Edge-method}
\alias{label,dg.Edge-method}
\alias{width<-,dg.Edge-method}
\alias{width,dg.Edge-method}
\alias{dash,dg.Edge-method}
\alias{dash<-,dg.Edge-method}
\alias{nodeIndices,dg.Edge-method}

% \alias{propertyDialog,dg.Edge-method}

\title{Class "dg.Edge" }

\description{  A skeleton class for the classes of edges. }

\section{Objects from the Class}{
Objects has the methods for extracting and setting the slots for edges,
and the method for drawing the edge.

% Objects can be created by calls of the form \code{new("dg.Edge", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}:
       The \code{vertex.indices} of the vertices of the edge. }
    \item{\code{width}:}{Object of class \code{"numeric"}:
       The \code{width} of the edge. }
    \item{\code{dash}:}{Object of class \code{"character"}:
       The \code{dash} pattern of the edge.

From the Tcl/tk Reference Manual:

% \begin{verbatim}

"DASH PATTERNS

Many items support the notion of an dash pattern for outlines. 

The first possible syntax is a list of integers. Each element represents the
number of pixels of a line segment. Only the odd segments are drawn using the
"outline" color. The other segments are drawn transparant. 

The second possible syntax is a character list containing only 5 possible
characters $[.,-\_ ]$. The space can be used to enlarge the space between other
line elements, and can not occur as the first position in the string. Some
examples:    
 -dash . = -dash \{2 4\}
 -dash - = -dash \{6 4\}
 -dash -. = -dash \{6 4 2 4\}
 -dash -.. = -dash \{6 4 2 4 2 4\}
 -dash \{. \} = -dash \{2 8\}
 -dash , = -dash \{4 4\}

The main difference of this syntax with the previous is that it it
shape-conserving. This means that all values in the dash list will be
multiplied by the line width before display. This assures that "." will always
be displayed as a dot and "-" always as a dash regardless of the line width. 

On systems which support only a limited set of dash patterns, the dash pattern
will be displayed as the closest dash pattern that is available. For example,
on Windows only the first 4 of the above examples are available.  The last 2
examples will be displayed identically to the first one. "
% \end{verbatim}

 }
    \item{\code{color}:}{Object of class \code{"character"}:
       The \code{color} of the edge. }
    \item{\code{label}:}{Object of class \code{"character"}:
       The \code{label} of the edge. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}:
       The \code{label.position} of the edge. }
  }
}

\section{Extends}{
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{dash<-}{\code{signature(x = "dg.Edge")} :
       Set the \code{dash} pattern of the edge. }
    \item{dash}{\code{signature(object = "dg.Edge")}:
       Return the \code{dash} pattern of the edge. }
    \item{draw}{\code{signature(object = "dg.Edge")}: ... }
    \item{initialize}{\code{signature(.Object = "dg.Edge")}: ... }
    \item{label<-}{\code{signature(x = "dg.Edge")}:
       Set the \code{label} of the edge. }
    \item{label}{\code{signature(object = "dg.Edge")}:
       Return the \code{label} of the edge. }
    \item{name}{\code{signature(object = "dg.Edge")}: 
       Return the \code{name}, equal to the \code{label}, of the edge. }
    \item{nodeIndicesOfEdge<-}{\code{signature(x = "dg.Edge")}:
       Set the \code{indices} of the vertices of the edge. }
    \item{nodeIndicesOfEdge}{\code{signature(object = "dg.Edge")}:
       Return the \code{indices} of the vertices of the edge. }
    \item{width<-}{\code{signature(x = "dg.Edge")}:
       Set the \code{width} of the edge. }
    \item{width}{\code{signature(object = "dg.Edge")}:
       Return the \code{width} of the edge. }
    % \item{propertyDialog}{\code{signature(object = "dg.Edge")}: ... }
    % \item{addToPopups}{\code{signature(object = "dg.Edge")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dg.VertexEdge-class}},
\code{\link{dg.BlockEdge-class}},
\code{\link{dg.FactorEdge-class}}.
}

\keyword{classes}
