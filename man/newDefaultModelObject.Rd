\name{newDefaultModelObject}
\alias{newDefaultModelObject}
\alias{modifyModel}
\alias{testEdge}
\title{The model object}
\description{Returns an object of the class \code{defaultModelObjectProto}.}
\usage{newDefaultModelObject(name)}
\arguments{\item{name}{ Text string with the \code{name} of the model object.}}
\details{ 
This is an example of the object for interface between
\code{\link{dynamicGraphMain}} and your models.
The model object of the call of \code{\link{dynamicGraphMain}}
should have the methods \code{modifyModel} and \code{testEdge}.
When the graph is modified, by adding or dropping vertices or edge,
the method \code{modifyModel} is called on the argument \code{object}
of \code{\link{dynamicGraphMain}}. If an \code{object} is returned in
the list of the returned value from \code{modifyModel} then
\code{object} in \code{\link{dynamicGraphMain}} is replaced by this
object, and the object is also assigned in the top level environment,
if  \code{objectName} was given to \code{\link{dynamicGraphMain}}.

The methods \code{testEdge} of \code{object} should return an object with
the methods \code{labelOfTest} and \code{widthOfTest} for labeling edges,
see \code{\link{newDefaultTestObject}}.
}
\value{An object of class \code{defaultModelObjectProto}.}
\references{ 
    CoCo, with a guide at \url{http://www.jstatsoft.org/v06/i04/},
    avaliable form \url{http://www.math.auc.dk/gr/material/CoCo/} and
    \url{http://www.jbs.agrsci.dk/Biometry/Software-Datasets/CoCo/CoCo.1.6/}
    has an interface to \code{\link{dynamicGraph}}. }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{\code{\link{newDefaultTestObject}}}
\examples{

# Edit the following to meet your needs:
#
# - Change the name "defaultModelObjectProto"
#
# - Work out how the get names, types and edges from the model object.
#
# - At "message", insert the relevant code for testing and modifying the model.
#


setClass("defaultModelObjectProto", representation(name = "character"))

"newDefaultModelObject"<-
  function(name)
  {
    result <- new("defaultModelObjectProto", name = name)
    return(result)
  }

if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph"))
    fun <- dynamic.Graph
  else
    fun <- function(object, ...)
  standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}

setMethod("dynamic.Graph", signature(object = "defaultModelObjectProto"),
          function(object, ...)
  {

    Names <- Your.function.for.extracting.variable.names.from.object(
             object = object)
    Types <- Your.function.for.extracting.variable.types.from.object(
             object = object)
    Edges <- Your.function.for.extracting.variable.edges.from.object(
             object = object)

    DynamicGraph(names = Names, types = Types, 
                 from = Edges[,1], to = Edges[,2], 
                 object = object, ...)
 })

if (!isGeneric("testEdge")) {
  if (is.function("testEdge"))
    fun <- testEdge
  else
    fun <- function(object, action, name.1, name.2, ...) 
           standardGeneric("testEdge")
  setGeneric("testEdge", fun)
}

setMethod("testEdge", signature(object = "defaultModelObjectProto"),
          function(object, action, name.1, name.2, ...)
 {
    args <- list(...)
    from.type <- args$from.type
    to.type <- args$to.type
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    message(paste("Should return an object with the edge from",
                  name.1, f(from.type), "to", name.2, f(to.type),
                  "deleted from the argument object"))
    return(newDefaultTestObject())
 })

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
                     name, f(args$type), "deleted from the argument object"))
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

newDefaultModelObject("ModelObject")

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
