\name{DynamicGraphModel-class}

\docType{class}

\alias{DynamicGraphModel-class}
\alias{Str,DynamicGraphModel-method}
\alias{show,DynamicGraphModel-method}

% \newpage

\title{Class "DynamicGraphModel" }

\description{  The class for the models of the dynamicGraph. }

\section{Objects from the Class}{
Objects of this class is created in the \code{drawModel} function
of \code{\link{dynamicGraphMain}}.

Objects of class \code{DynamicGraphModel-class} will be a part of the returned
value of the function \code{\link{dynamicGraphMain}}, and will store the model,
the views of the model, each of class \code{\link{DynamicGraphView-class}},
and the function \code{redrawView} for drawing views of the model.

% Objects of class \code{DynamicGraphModel-class} can be created by
% calls of the form \code{new("DynamicGraphModel", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"character"}:
        The value of the argument \code{title} of 
        \code{\link{dynamicGraphMain}}, combined with the slot \code{name}
        of the model object, printed by \code{Str}. }
    \item{\code{index}:}{Object of class \code{"numeric"}:
        The \code{index} of the model. }
    \item{\code{redrawView}:}{Object of class \code{"function"}: 
        For drawing views of the model. }
    \item{\code{model}:}{Object of class \code{"list"}:
        The \code{model} of the graph windows. }
    \item{\code{graphs}:}{Object of class \code{"list"}:
        A list of view objects,
        each of class \code{\link{DynamicGraphView-class}}. }
  }
}

\section{Methods}{
  \describe{
    \item{Str}{\code{signature(object = "DynamicGraphModel")}:
      Compactly display the internal *str*ucture of a dynamicGraph model object. }
    \item{show}{\code{signature(object = "DynamicGraph")}: calls the method
      \code{\link{Str}}. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{ 
\code{\link{dynamicGraphMain}},
\code{\link{DynamicGraph-class}}, and 
\code{\link{DynamicGraphView-class}}.
}

\keyword{classes}
