\name{returnFactorVerticesAndEdges}
\alias{factorVertexList}
\alias{returnFactorVerticesAndEdges}
\title{ The factor vertex list}
\description{
    Create factor vertex and factor edge lists.
}
\usage{
returnFactorVerticesAndEdges(Vertices, factors = NULL, types = "Generator", 
                             factorVertexColor = "default", 
                             factorEdgeColor = "DarkOliveGreen", 
                             % oriented = FALSE, 
                             factorClasses = validFactorClasses())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Vertices}{ The list of \code{Vertices}, each
      containing the class \code{VertexProto}. }
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
        \code{\link{FactorVertexProto}}. }
  \item{FactorEdges}{The list of factor edge, each of class containing
        \code{\link{FactorEdgeProto}}.}
  \item{PairEdges}{A matrix with the edges of the graph,
        two columns with the indices of the vertices of two ends 
        of the edges. }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
    The methods of the vertex list, \code{\link{returnVertexList}},
    also applies for factor lists,
    and the methods of the edge list, \code{\link{returnEdgeList}},
    also applies for factor edge lists.

    Your \code{\link{modifyModel}} should compute the new factors,
    generators, when modifying the model. By default only the new
    factors are computed when a vertex is deleted.
    Adding and dropping edges requires finding the cliques of the graph
    from the edges of the graph. Finding the cliques is out of the scope
    of  \code{\link{dynamicGraph}}.
 }
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{

setClass("defaultModelObjectProto", representation(name = "character"))

if (!isGeneric("modifyModel")) {
  if (is.function("modifyModel"))
    fun <- modifyModel
  else
    fun <- function(object, action, name, name.1, name.2, ...)
                    standardGeneric("modifyModel")
  setGeneric("modifyModel", fun)
}

setMethod("modifyModel", signature(object = "defaultModelObjectProto"),
          function(object, action, name, name.1, name.2, ...)
 {
    args <- list(...)
    FactorVertices <- NULL
    FactorEdges <- NULL
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    if (action == "dropEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(args$from.type), "to", name.2, f(args$to.type),
                     "deleted from the argument object"))
    } else if (action == "addEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(args$from.type), "to", name.2, f(args$to.type),
                     "added to the argument object"))
    } else if (action == "dropVertex")  {
       message(paste("Should return an object with the vertex", 
                     name, f(args$type),
                     "deleted from the argument object"))
       if (!is.null(args$Arguments) && (args$index > 0)
                       && !is.null(args$Arguments$factorVertexList)
                       && !is.null(args$Arguments$vertexList)) {
         x <- (args$Arguments$factorVertexList)
         factors <- lapply(x, function(i) i@vertex.indices)
         types <- lapply(x, function(i) class(i))
         factors <- lapply(factors, function(x) x[x != args$index])
         if (!(is.null(factors))) {
           result <- returnFactorVerticesAndEdges(
                                   args$Arguments$vertexList, factors, types, 
                                   factorClasses = validFactorClasses())
           FactorVertices <- result$FactorVertices
           FactorEdges <- result$FactorEdges
         }
       }
    } else if (action == "addVertex") {
       message(paste("Should return an object with the vertex", 
                     name, f(args$type), args$index, 
                     "added to the argument object"))
    }
    return(list(object = object,
                FactorVertices = FactorVertices,
                FactorEdges = FactorEdges))
 })

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
