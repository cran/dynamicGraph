\name{DynamicGraph-class}

\docType{class}

\alias{DynamicGraph-class}
\alias{Str}
\alias{Str,DynamicGraph-method}
\alias{Str,NULL-method}
\alias{Str,integer-method}
\alias{Str,numeric-method}
% \alias{Str,list-method}
% \alias{Str,dg.list-method}
\alias{Str,DynamicGraph-method}
\alias{show,DynamicGraph-method}

\title{Class "DynamicGraph" }

\description{ The class for the dynamicGraph. }

\section{Objects from the Class}{
 An object of this class \code{DynamicGraph-class}
 is the returned value from \code{\link{dynamicGraphMain}}.

 The object has the lists of vertices and blocks (block trees)
 of the dynamicGraph, and a list of models, each (of class 
 \code{\link{DynamicGraphModel-class}}) with the views of the model.
 
 Each view (of class \code{\link{DynamicGraphView-class}}) of a model
 will hold the edges (edges between vertices, factors and blocks)
 and factor- and extra-vertices of the view, together with 
 vectors of indices of vertices and blocks visible in the view.

 Objects of the class \code{\link{DynamicGraph-class}} is the
 outer frame holding the "data" and the models, each model is of the class 
 \code{\link{DynamicGraphModel-class}} with one or more views of the class 
 \code{\link{DynamicGraphView-class}} in a sub-frame for the model.

 If the argument \code{returnLink} is TRUE for \code{\link{dynamicGraphMain}}
 then the object returned from \code{\link{dynamicGraphMain}} will have
 the function \code{\link{drawModel}} for drawing other models and views
 associated with the returned object of \code{\link{DynamicGraph-class}}.

 % Objects of class \code{DynamicGraph-class} can be created 
 % by calls of the form \code{new("DynamicGraph", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"character"}:
        The value of the argument \code{title} of 
        \code{\link{dynamicGraphMain}}, 
        the header label, \code{tktitle}, of the window,
	also printed by \code{Str}. }
    \item{\code{vertices}:}{Object of class \code{"dg.VertexList"}:
        The \code{vertexList} of the graph window. }
    \item{\code{blocks}:}{Object of class \code{"dg.BlockList"}:
        The \code{blockList} of the graph window. }
    \item{\code{blockTree}:}{Object of class \code{"list"}:
        The \code{blockTree} of the graph window. }
    % \item{\code{propagateAll}:}{Object of class \code{"logical"}: 
    %     }
    \item{\code{drawModel}:}{Object of class \code{"function"}: 
        Function for adding models to the returned \code{dynamicGraph}. }
    \item{\code{models}:}{Object of class \code{"list"}:
        A list of objects, 
        each of class \code{\link{DynamicGraphModel-class}}. }
  }
}

\section{Methods}{
  \describe{
    \item{Str}{\code{signature(object = "DynamicGraph")}:
      Compactly display the internal *str*ucture of a dynamicGraph object,
      using \code{\link{asDataFrame}} on each list of node objects. }
    \item{show}{\code{signature(object = "DynamicGraph")}: calls the method
      \code{\link{Str}}. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dynamicGraphMain}},
\code{\link{DynamicGraphModel-class}}, and
\code{\link{DynamicGraphView-class}}.
}

\keyword{classes}
