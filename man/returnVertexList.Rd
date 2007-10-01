\name{returnVertexList}

\alias{vertexList}
\alias{returnVertexList}

\alias{dg.VertexList-class}
\alias{initialize,dg.VertexList-method}

% \alias{Names}
% \alias{Names<-}
% \alias{Colors}
% \alias{Colors<-}
% \alias{Labels}
% \alias{Labels<-}
% \alias{LabelPositions}
% \alias{LabelPositions<-}
% \alias{Positions}
% \alias{Positions<-}
% \alias{Strata}
% \alias{Strata<-}
% \alias{Blockindices}
% \alias{Blockindices<-}
% \alias{Indices}
% \alias{asDataFrame}
% % \alias{Indices<-}
% \alias{Visible}
% \alias{Visible<-}

% \alias{dg.list-class}

% \alias{show,dg.list-method}
% \alias{Names,dg.list-method}
% \alias{Names<-,dg.list-method}
% \alias{Colors,dg.list-method}
% \alias{Colors<-,dg.list-method}
% \alias{Labels,dg.list-method}
% \alias{Labels<-,dg.list-method}
% \alias{LabelPositions,dg.list-method}
% \alias{LabelPositions<-,dg.list-method}
% \alias{Positions,dg.list-method}
% \alias{Positions<-,dg.list-method}
% \alias{Strata,dg.list-method}
% \alias{Strata<-,dg.list-method}
% \alias{Blockindices,dg.list-method}
% \alias{Blockindices<-,dg.list-method}
% \alias{Indices,dg.list-method}
% \alias{asDataFrame,dg.list-method}
% \alias{Visible,dg.list-method}
% \alias{Visible<-,dg.list-method}

\title{Class "dg.VertexList": The vertex list }

\description{
  Return a list of vertices of classes containing the class \code{dg.Vertex}.

 Objects can be created by calls of the form \code{new("dg.VertexList", ...)}.
}

\usage{
returnVertexList(names, labels = NULL, types = NULL, 
                 strata = NULL, line = FALSE, N = 3, 
                 colors = ifelse(types == "TextVertex", 
                                 "FloralWhite", "DarkRed"), 
                 vertexClasses = validVertexClasses())
}

\arguments{
  \item{names}{ Vector of text strings with the \code{names} of the vertices. }
  \item{labels}{ Vector of text strings with the \code{labels} of the vertices. }
  \item{types}{ Vector of text strings with the \code{types} of the vertices. }
  \item{strata}{ Vector of integers with the \code{strata} of the vertices. }
  \item{line}{ Logical, if TRUE then the vertices are positioned on a
               \code{line}, else in a regular polygone, in a circle. }
  \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
  \item{colors}{ Vector of text strings with \code{colors} of the vertices. }
  \item{vertexClasses}{ The valid \code{\link{vertexClasses}}. }
}

\value{
 A list of vertices of classes containing the class \code{dg.Vertex}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
% Class \code{"dg.NodeList"}, directly.
% Class \code{"dg.list"}, by class \code{"dg.NodeList"}.
% Class \code{"list"}, by class \code{"dg.NodeList"}.

% Class \code{"list"}, from data part.
% Class \code{"dg.list"}, directly.
% Class \code{"dg.NodeList"}, directly.

Class \code{"dg.NodeList"}, directly.
Class \code{"dg.list"}, directly.
Class \code{"list"}, from data part.
% Class \code{"vector"}, by class \code{"dg.NodeList"}.
Class \code{"vector"}, by class \code{"dg.NodeList"}.
Class \code{"vector"}, by class \code{"dg.list"}.
Class \code{"vector"}, by class \code{"list"}.

}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "dg.VertexList")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\note{The methods
      \code{\link{Names}}, 
      \code{\link{Names<-}}, 
      \code{\link{Colors}}, 
      \code{\link{Colors<-}}, 
      \code{\link{Labels}}, 
      \code{\link{Labels<-}}, 
      \code{\link{LabelPositions}}, 
      \code{\link{LabelPositions<-}}, 
      \code{\link{Positions}}, 
      \code{\link{Positions<-}}, 
      \code{\link{Strata}}, 
      \code{\link{Strata<-}},
      \code{\link{Indices}}, and 
      \code{\link{asDataFrame}} 
      are available for vertex lists.
      % \code{\link{Indices<-}}, 
      % \code{\link{Visible}}, 
      % \code{\link{Visible<-}}, 
}

\examples{
vertices <- returnVertexList(c("A", "B", "C", "D"),
                             labels = c("OrdinalVertex", "TextVertex", 
                               "ContinuousVertex", "DiscreteVertex"), 
                             types = c("Ordinal", "TextVertex",
                                       "Continuous", "Discrete"), N = 2)

vertices <- new("dg.VertexList", names = c("A", "B", "C", "D"),
                             labels = c("OrdinalVertex", "TextVertex", 
                               "ContinuousVertex", "DiscreteVertex"), 
                             types = c("Ordinal", "TextVertex",
                                       "Continuous", "Discrete"), N = 2)
Names(vertices)
Colors(vertices)
Labels(vertices)
LabelPositions(vertices)
Positions(vertices)
Strata(vertices)
Indices(vertices)
Names(vertices) <- c("a", "b", "c", "d")
Colors(vertices) <- rep("Blue", 4)
Labels(vertices) <- c("A", "B", "C", "D")
LabelPositions(vertices) <- matrix(rep(0, 12), ncol = 3)
Positions(vertices) <- matrix(rep(0, 12), ncol = 3)
Strata(vertices) <- rep(1, 4)
Names(vertices)
Colors(vertices)
Labels(vertices)
LabelPositions(vertices)
Positions(vertices)
Strata(vertices)
Indices(vertices)
asDataFrame(vertices)
}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
