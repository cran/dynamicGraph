\name{dg.list-class}

\docType{class}

\alias{dg.list-class}
\alias{dg.NodeList-class}
\alias{dg.EdgeList-class}

\alias{Names}
\alias{Names<-}
\alias{Colors}
\alias{Constrained<-}
\alias{Constrained}
\alias{Colors<-}
\alias{Labels}
\alias{Labels<-}
\alias{LabelPositions}
\alias{LabelPositions<-}
\alias{Positions}
\alias{Positions<-}
\alias{Strata}
\alias{Strata<-}
\alias{Blockindices}
\alias{Blockindices<-}
\alias{Indices}
\alias{asDataFrame}
% \alias{Indices<-}
\alias{Visible}
\alias{Visible<-}

\alias{NodeTypes}
% \alias{NodeTypes<-}
\alias{NodeIndices}
% \alias{NodeIndices<-}
\alias{Widths}
\alias{Widths<-}
\alias{Dashes}
\alias{Dashes<-}
\alias{FixedPositions<-,dg.list-method}
\alias{FixedPositions,dg.list-method}
\alias{Oriented}
\alias{Oriented<-}

\alias{asDataFrame,dg.list-method}
\alias{Blockindices,dg.list-method}
\alias{Closed<-,dg.list-method}
\alias{Closed,dg.list-method}
\alias{Constrained<-,dg.list-method}
\alias{Constrained,dg.list-method}
\alias{Colors<-,dg.list-method}
\alias{Colors,dg.list-method}
\alias{Dashes<-,dg.list-method}
\alias{Dashes,dg.list-method}
\alias{Indices,dg.list-method}
\alias{LabelPositions<-,dg.list-method}
\alias{LabelPositions,dg.list-method}
\alias{Labels<-,dg.list-method}
\alias{Labels,dg.list-method}
\alias{Names<-,dg.list-method}
\alias{Names,dg.list-method}
\alias{Parents<-,dg.list-method}
\alias{Parents,dg.list-method}
\alias{Children<-,dg.list-method}
\alias{Children,dg.list-method}
\alias{NodeAncestors<-,dg.list-method}
\alias{NodeAncestors,dg.list-method}
\alias{NodeDescendants<-,dg.list-method}
\alias{NodeDescendants,dg.list-method}
\alias{Parents<-,dg.list-method}
\alias{Parents,dg.list-method}
\alias{Children<-,dg.list-method}
\alias{Children,dg.list-method}
\alias{NodeIndices,dg.list-method}
\alias{NodeTypes,dg.list-method}
\alias{Oriented<-,dg.list-method}
\alias{Oriented,dg.list-method}
\alias{Positions<-,dg.list-method}
\alias{Positions,dg.list-method}
\alias{show,dg.list-method}
\alias{Strata<-,dg.list-method}
\alias{Strata,dg.list-method}
\alias{Visible<-,dg.list-method}
\alias{Visible,dg.list-method}
\alias{Widths<-,dg.list-method}
\alias{Widths,dg.list-method}

\alias{Str,list-method}
\alias{Str,dg.list-method}

\title{Classes dg.list, dg.NodeList, and dg.EdgeList}

\description{ A class for lists of vertices, factors, blocks, and edges. }

\section{Objects from the Class}{
Appropriate slots of \code{\link{DynamicGraph-class}}
and \code{\link{DynamicGraphView-class}} should be of this class.

The lists returned from \code{\link{returnVertexList}},
\code{\link{returnEdgeList}}, \code{\link{returnFactorEdgeList}}, and
\code{\link{returnBlockEdgeList}}, and appropriate components of
the returned values from \code{\link{setBlocks}} and
\code{\link{returnFactorVerticesAndEdges}} are of this class.

Objects can be created by calls of the form \code{new("dg.list", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}: The list. }
  }
}

\section{Extends}{
% Class \code{"list"}, directly.

% Class \code{"dg.list"}, directly.
% Class \code{"list"}, by class \code{"dg.list"}.
Class "dg.EdgeList":

Class \code{"dg.NodeList"}, directly.
Class \code{"dg.list"}, by class \code{"dg.NodeList"}.
Class \code{"list"}, by class \code{"dg.NodeList"}.
}

\section{Methods}{
  \describe{
    \item{asDataFrame}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Blockindices}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Closed<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Closed}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Constrained<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Constrained}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Colors<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Colors}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Dashes<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Dashes}{\code{signature(objectlist = "dg.list")}: ... }
    \item{FixedPositions<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{FixedPositions}{\code{signature(objectlist = "dg.list")}: ... }
  % \item{Indices<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Indices}{\code{signature(objectlist = "dg.list")}: ... }
    \item{LabelPositions<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{LabelPositions}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Labels<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Labels}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Names<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Names}{\code{signature(objects = "dg.list")}: ... }
    \item{Parents<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Parents}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Children<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Children}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeAncestors<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeAncestors}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeDescendants<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeDescendants}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeIndices}{\code{signature(objectlist = "dg.list")}: ... }
    \item{NodeTypes}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Oriented<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Oriented}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Positions<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Positions}{\code{signature(objectlist = "dg.list")}: ... }
    \item{show}{\code{signature(object = "dg.list")}: ... }
    \item{Str}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Strata<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Strata}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Visible<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Visible}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Widths<-}{\code{signature(objectlist = "dg.list")}: ... }
    \item{Widths}{\code{signature(objectlist = "dg.list")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dynamicGraphMain}}, and
\code{\link{DynamicGraphModel-class}}.
}

% \examples{ }

\keyword{classes}
