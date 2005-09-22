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

\alias{blocks<-,DynamicGraph-method}
\alias{blocks,DynamicGraph-method}
\alias{control<-,DynamicGraph-method}
\alias{control,DynamicGraph-method}
\alias{dg,DynamicGraph-method}
\alias{models<-,DynamicGraph-method}
\alias{models,DynamicGraph-method}
\alias{label<-,DynamicGraph-method}
\alias{label,DynamicGraph-method}
\alias{vertices<-,DynamicGraph-method}
\alias{vertices,DynamicGraph-method}

\alias{blocks<-}
\alias{blocks}
\alias{control<-}
\alias{control}
%\alias{dg}
\alias{models<-}
\alias{models}
\alias{label<-}
\alias{label}
\alias{vertices<-}
\alias{vertices}

\title{Class "DynamicGraph" }

\description{ The class of the object of dynamicGraph. }

\section{Objects from the Class}{
 An object of this class \code{DynamicGraph-class}
 is the returned value from the method \code{\link{dg}}
 and from the function \code{\link{dynamicGraphMain}}.

 The object has the lists of vertices and blocks % (block trees)
 of the dynamicGraph, and a list of models, each (of class 
 \code{\link{DynamicGraphModel-class}})
 with the model and the views of the model.
 
 Each view (of class \code{\link{DynamicGraphView-class}}) of a model
 will hold the edges (edges between vertices, factors and blocks)
 and factor- and extra-vertices of the view, together with 
 vectors of indices of vertices and blocks visible in the view.

 Objects of the class \code{\link{DynamicGraph-class}} is the
 outer frame holding the "data" and the models, each model is of the class 
 \code{\link{DynamicGraphModel-class}} with one or more views of the class 
 \code{\link{DynamicGraphView-class}} in a sub-frame for the model.

 % If the argument \code{returnLink} is TRUE for \code{\link{dynamicGraphMain}}
 % then the object returned from \code{\link{dynamicGraphMain}} will have
 % the function \code{\link{drawModel}} for drawing other models and views
 % associated with the returned object of \code{\link{DynamicGraph-class}}.

 % Objects of class \code{DynamicGraph-class} can be created 
 % by calls of the form \code{new("DynamicGraph", ...)}.

 The input to \code{\link{dynamicGraphMain}} should be
 of the class \code{\link{dg.graph-class}}.

}

\section{Slots}{
  \describe{
    \item{\code{id.env}:}{Object of class \code{"character"}: 
           Internal identification of the object. }
    \item{\code{label}:}{Object of class \code{"character"}:
        The value of the argument \code{label} of 
        \code{\link{dynamicGraphMain}}, the header label, 
        \code{tktitle}, of the window, also printed by \code{Str}. }
    \item{\code{vertices}:}{Object of class \code{"dg.VertexList"}:
        The \code{vertexList} of the graph window. }
    \item{\code{blocks}:}{Object of class \code{"dg.BlockList"}:
        The \code{blockList} of the graph window. }

    \item{\code{control}:}{Object of class \code{"list"} with the options of
        the functions 
        \code{\link{dynamicGraphMain}} and \code{\link{simpleGraphToGraph}}. }

    %% \item{\code{blockTree}:}{Object of class \code{"list"}:
    %%    The \code{blockTree} of the graph window. }

    % \item{\code{propagateAll}:}{Object of class \code{"logical"}: 
    %     }

    % \item{\code{drawModel}:}{Object of class \code{"function"}: 
    %     Function for adding models to the returned \code{dynamicGraph}. }

    \item{\code{models}:}{Object of class \code{"list"}:
        A list of objects, 
        each of class \code{\link{DynamicGraphModel-class}}. }
  }
}

\section{Methods}{
  \describe{
    \item{label<-}{\code{signature(x = "DynamicGraph")}: ... }
    \item{label}{\code{signature(object = "DynamicGraph")}: ... }

    \item{vertices<-}{\code{signature(x = "DynamicGraph")}: ... }
    \item{vertices}{\code{signature(object = "DynamicGraph")}: ... }
    \item{blocks<-}{\code{signature(x = "DynamicGraph")}: ... }
    \item{blocks}{\code{signature(object = "DynamicGraph")}: ... }

    \item{control<-}{\code{signature(x = "DynamicGraph")}: ... }
    \item{control}{\code{signature(object = "DynamicGraph")}: ... }
    \item{models<-}{\code{signature(x = "DynamicGraph")}: ... }
    \item{models}{\code{signature(object = "DynamicGraph")}: ... }

    \item{Str}{\code{signature(object = "DynamicGraph")}:
      Compactly display the internal *str*ucture of a dynamicGraph object,
      using \code{\link{asDataFrame}} on each list of node objects. }
    \item{show}{\code{signature(object = "DynamicGraph")}: calls the method
      \code{\link{Str}}. }

    \item{dg}{\code{signature(object = "DynamicGraph")}: Redraw the object. }

  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dynamicGraphMain}},
\code{\link{DynamicGraphModel-class}}, and
\code{\link{DynamicGraphView-class}}.
}

\keyword{classes}
