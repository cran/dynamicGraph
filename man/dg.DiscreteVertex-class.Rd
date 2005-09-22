\name{dg.DiscreteVertex-class}

\docType{class}

\alias{dg.DiscreteVertex-class}
\alias{draw,dg.DiscreteVertex-method}
\alias{dg.ContinuousVertex-class}
\alias{draw,dg.ContinuousVertex-method}
\alias{dg.OrdinalVertex-class}
\alias{draw,dg.OrdinalVertex-method}

\title{Classses "dg.DiscreteVertex", "dg.ContinuousVertex",
       and "dg.OrdinalVertex" }

\description{ The class for vertices for discrete variables, 
 continuous variables, and ordinal variables. }

\section{Objects from the Class}{
Objects has the methods for extracting and setting the slots for vertices,
and the method for drawing the vertex.

% Objects can be created by calls of the form
%  \code{new("dg.DiscreteVertex", ...)},
%  \code{new("dg.ContinuousVertex", ...)},
%  and \code{new("dg.OrdinalVertex", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{index}:}{Object of class \code{"numeric"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{position}:}{Object of class \code{"numeric"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{blockindex}:}{Object of class \code{"numeric"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{stratum}:}{Object of class \code{"numeric"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{constrained}:}{Object of class \code{"logical"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{color}:}{Object of class \code{"character"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{label}:}{Object of class \code{"character"}, 
                        see \code{"dg.Vertex"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, 
                        see \code{"dg.Vertex"}. }
  }
}

\section{Extends}{
% Class \code{"dg.Vertex"}, directly.
% Class \code{"dg.Node"}, by class \code{"dg.Vertex"}.
Class \code{"dg.Vertex"}, directly.
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "dg.DiscreteVertex")}:
     Method for drawing the vertex.
    The symbol will be a \code{'d'ot} for \code{'d'iscrete} variable. }
    \item{draw}{\code{signature(object = "dg.OrdinalVertex")}:
    Method for drawing the vertex.
    The symbol will be a \code{filled square} for \code{ordinal} variable. }
    \item{draw}{\code{signature(object = "dg.ContinuousVertex")}:
    Method for drawing the vertex.
    The symbol will be a \code{'c'circle} for \code{'c'ontinuous} variable. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dg.Vertex-class}},
\code{\link{returnVertexList}},
\code{\link{dg.Vertex-class}}.
}

\keyword{classes}
