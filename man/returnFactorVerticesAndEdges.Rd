\name{returnFactorVerticesAndEdges}

\alias{factorVertexList}
\alias{returnFactorVerticesAndEdges}

\alias{dg.FactorVertexList-class}

\title{Class "dg.FactorVertexList": The factor vertex list}

\description{
 Create factor vertex and factor edge lists.

 Objects can be created by calls of the form
 \code{new("dg.FactorVertexList", ...)}.
}

\usage{
returnFactorVerticesAndEdges(Vertices, factors = NULL, types = "Generator", 
                             factorVertexColor = "default", 
                             factorEdgeColor = "DarkOliveGreen", 
                             % oriented = FALSE, 
                             factorClasses = validFactorClasses())
}

\arguments{
  \item{Vertices}{ The list of \code{Vertices}, each
      containing the class \code{dg.Vertex}. }
  \item{factors}{ The list of vectors identifying the \code{factors}.
          Each item in the list is a vector of the indices of
	  vertices of a factor. 
      }
  \item{types}{ The \code{types} of the factors. Either a single
      \code{type} or a list of the same length as \code{factors}.
      Each item of \code{types} should match the labels of
      \code{\link{factorClasses}}, and is used to set the class of
      the factor vertex.
  }
  \item{factorVertexColor}{ The \code{factorVertexColor}
      of the factor vertices. }
  \item{factorEdgeColor}{ The \code{factorEdgeColor} of the factor edges. }
  % \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{factorClasses}{ The valid \code{\link{factorClasses}}. }
}

\details{
      The argument \code{factors} is a list of vectors identifying 
      the \code{factors}, or \code{generators}.
      Each item in the list is a vector with of the indices (or names)
      of the vertices of a factor, or variables of a generator.
      A factor vertex is made for each factor, and factor edges
      from this factor vertex to the vertices of the factor
      or added to the factor edge list.
      Also the edges between pairs of the vertices in the factors
      are returned.
}

\value{
  A list with components
  \item{FactorVertices}{The list of factor vertices, each of class containing
        \code{\link{dg.FactorVertex}}. }
  \item{FactorEdges}{The list of factor edge, each of class containing
        \code{\link{dg.FactorEdge}}.}
  \item{PairEdges}{A matrix with the edges of the graph,
        two columns with the indices of the vertices of two ends 
        of the edges. }
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
Class \code{"dg.NodeList"}, directly.
Class \code{"dg.list"}, by class \code{"dg.NodeList"}.
Class \code{"list"}, by class \code{"dg.NodeList"}.
}

\section{Methods}{
No methods defined with class "dg.FactorVertexList" in the signature.
}

\author{Jens Henrik Badsberg}

\note{ 
    The methods of the vertex list, \code{\link{returnVertexList}},
    also applies for factor lists,
    and the methods of the edge list, \code{\link{returnEdgeList}},
    also applies for factor edge lists.

    Your \code{\link{modifyModel}} should compute the new factors,
    generators, when modifying the model. See \code{\link{dg.Model-class}}
    for examples on use of \code{returnFactorVerticesAndEdges}.

  % By default only the new factors are computed when a vertex is deleted.
  % Adding and dropping edges requires finding the cliques of the graph
  % from the edges of the graph. Finding the cliques is out of the scope
  % of  \code{\link{dynamicGraph}}.
 }

% \examples{ }

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
