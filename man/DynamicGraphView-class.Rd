\name{DynamicGraphView-class}

\docType{class}

\alias{DynamicGraphView-class}
\alias{Str,DynamicGraphView-method}
\alias{show,DynamicGraphView-method}
\alias{EssentialDynamicGraphView-class}
\alias{FactorDynamicGraphView-class}
\alias{MoralDynamicGraphView-class}
\alias{SimpleDynamicGraphView-class}

\title{Class "DynamicGraphView" }

\description{ The class for views of the models of the dynamicGraph. }

\section{Objects from the Class}{
Objects of the class \code{DynamicGraphView-class} is created
in the \code{redrawView} function in the \code{drawModel}
function of \code{\link{dynamicGraphMain}}.

Objects of this class will be a part of each model in the model list
of the returned value from \code{\link{dynamicGraphMain}},
and will store the view specific items of a model,
plus the identification of the Tcl/tk window.

% Objects of class \code{DynamicGraphView-class} can be created
% by calls of the form \code{new("DynamicGraphView", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"character"}:
        The value of the argument \code{title} of 
        \code{\link{dynamicGraphMain}}, combined with the
        \code{viewType} of the model object, printed by \code{Str}. }
    \item{\code{index}:}{Object of class \code{"numeric"}:
       The \code{index} of the view. }
    \item{\code{top}:}{Object of class \code{"tkwin"}:
       The \code{tkwin}-top of the graph window. }
    \item{\code{canvas}:}{Object of class \code{"tkwin"}:
       The \code{canvas}-top of the graph window. }
    \item{\code{tags}:}{Object of class \code{"list"}:
       The tags of the objects drawn in the graph window. }
    \item{\code{update}:}{Object of class \code{"function"}: 
        For updating the view and slots of the view. }
    \item{\code{id}:}{Object of class \code{"numeric"}:
       Integer, increased for each redraw of the graph window. }
    \item{\code{viewLabel}:}{Text string with the type of the view. }
    % \item{\code{setVertexLabels}:}{Object of class \code{"logical"}: ... }
    % \item{\code{object}:}{Object of class \code{"list"}:
    %    The object of the graph. }
    \item{\code{visibleVertices}:}{Object of class \code{"numeric"}:
       Numeric vector of the indices of the vertices of
       \code{vertexList} plotted in the view. }
    \item{\code{visibleBlocks}:}{Object of class \code{"numeric"}:
       Numeric vector of the indices of the blocks of
       \code{blockList} plotted in the view. }
    \item{\code{extraVertices}:}{Object of class \code{"dg.Vertexlist"}:
       The \code{extraList} of the view. }
    \item{\code{vertexEdges}:}{Object of class \code{"dg.VertexEdgeList"}:
       The \code{edgeList} of the view. }
    \item{\code{blockEdges}:}{Object of class \code{"dg.BlockEdgeList"}:
       The \code{blockEdgeList} of the view. }
    \item{\code{factorVertices}:}{Object of class \code{"dg.FactorVertexList"}:
       The \code{factorVertexList} of the view. }
    \item{\code{factorEdges}:}{Object of class \code{"dg.FactorEdgeList"}:
       The \code{factorEdgeList} of the view. }
    \item{\code{extraEdges}:}{Object of class \code{"dg.ExtraEdgeList"}:
       The \code{extraEdgeList} of the view. }
  }
}

\section{Methods}{
  \describe{
    \item{Str}{\code{signature(object = "DynamicGraphView")}:
      Compactly display the internal *str*ucture of a dynamicGraph view object. }
    \item{show}{\code{signature(object = "DynamicGraph")}: calls the method
      \code{\link{Str}}. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dynamicGraphMain}},
\code{\link{DynamicGraph-class}}, and
\code{\link{DynamicGraphModel-class}}.
}

\keyword{classes}
