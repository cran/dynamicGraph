\name{dg.Generator-class}

\docType{class}

\alias{dg.Generator-class}
\alias{draw,dg.Generator-method}
\alias{dg.DiscreteGenerator-class}
\alias{draw,dg.DiscreteGenerator-method}
\alias{dg.LinearGenerator-class}
\alias{draw,dg.LinearGenerator-method}
\alias{dg.QuadraticGenerator-class}
\alias{draw,dg.QuadraticGenerator-method}

\title{Classses "dg.Generator", "dg.DiscreteGenerator",
       "dg.Linear"\-"Generator", and "dg.QuadraticGenerator" }

\description{ The class for factor vertices for general terms,
 discrete terms, linear terms, and quadratic terms. 

 The class adds the \code{draw} method to the 
 \code{\link{dg.FactorVertex-class}}.

 The objects of the classes are usually created by the function
 \code{\link{returnFactorVerticesAndEdges}}.
}

% \section{Objects from the Class}{
% Objects can be created by calls of the form
% \code{new("dg.Generator", ...)},
% \code{new("dg.DiscreteGenerator", ...)},
% \code{new("dg.LinearGenerator", ...)}, or
% \code{new("dg.QuadraticGenerator", ...)}.
% }

\section{Slots}{
  \describe{
    % \item{\code{test.position}:}{Object of class \code{"numeric"},
    %      see \code{"dg.FactorVertex"}. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"},
          see \code{"dg.FactorVertex"}. }
    \item{\code{fixed.positions}:}{Object of class \code{"logical"}, 
                        see \code{"dg.FactorVertex"}. }
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
          see \code{"dg.Vertex"}.
       Default is \code{yellow} for general, \code{cyan} for discrete,
       \code{magenta} for linear, and \code{blue} for quadratic terms. }
    \item{\code{label}:}{Object of class \code{"character"},
          see \code{"dg.Vertex"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"},
          see \code{"dg.Vertex"}. }
  }
}

\section{Extends}{
% Class \code{"dg.FactorVertex"}, directly.
% Class \code{"dg.Vertex"}, by class \code{"dg.FactorVertex"}.
% Class \code{"dg.Node"}, by class \code{"dg.FactorVertex"}.
Class \code{"dg.FactorVertex"}, directly.
Class \code{"dg.Vertex"}, directly.
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{draw}{\code{signature(object = "dg.Generator")}: ... }
    \item{draw}{\code{signature(object = "dg.DiscreteGenerator")}: ... }
    \item{draw}{\code{signature(object = "dg.LinearGenerator")}: ... }
    \item{draw}{\code{signature(object = "dg.QuadraticGenerator")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{newFactor}},
\code{\link{returnFactorVerticesAndEdges}},
\code{\link{dg.FactorVertex-class}},
\code{\link{dg.Vertex-class}},
\code{\link{dg.Node-class}}.
}

\keyword{classes}
