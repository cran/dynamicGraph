\name{dg.Model-class}
\docType{class}

\alias{dg.Model-class}
\alias{modifyModel,dg.Model-method}
\alias{testEdge,dg.Model-method}
\alias{setGraphComponents,dg.Model-method}
\alias{returnGraphComponents,dg.Model-method}
\alias{graphComponents,dg.Model-method}
\alias{vertexEdges,dg.Model-method}
\alias{vertexEdges<-,dg.Model-method}
\alias{Str,dg.Model-method}
\alias{setGraphComponents}
\alias{returnGraphComponents}
\alias{graphComponents}
\alias{vertexEdges}
\alias{vertexEdges<-}

\alias{newDefaultModelObject}

\alias{newDefaultModelObject}
\alias{modifyModel}
\alias{testEdge}

\title{Class "dg.Model" }

\description{  An example class for the model object of dynamicGraph. }

\section{Objects from the Class}{

This is an example of the object for interface between
\code{\link{dynamicGraphMain}} and your models.

The model object of the call of \code{\link{dynamicGraphMain}}
should have the methods \code{modifyModel}, \code{testEdge},
\code{graphComponents} and \code{setGraphComponents}.

When the graph is modified, by adding or dropping vertices or edge,
the method \code{modifyModel} is called on the argument \code{object}
of \code{\link{dynamicGraphMain}}. 

If a value different from NULL is returned from \code{modifyModel} at the 
key event "add edge" then the edge is added to the the view of the model,
the graph window.

If NULL is returned from \code{modifyModel} then \code{\link{drawModel}}
can be used to draw the new graph.
If a value different from NULL is returned from \code{modifyModel} then 
\code{\link{redrawView}} and \code{\link{drawModel}} should not be called
from \code{modifyModel} for the action add edge.

If the edge is to be added in a slave window then the edges to
draw can be returned in a component with name \code{edgeList} (or
\code{newEdges$vertexEdges}) of the returned structure.

If an \code{object} is returned in the list of the returned
value from \code{modifyModel} then \code{object} in 
\code{\link{dynamicGraphMain}} is replaced by this object
(and the object is also assigned in the top level environment,
if \code{objectName} was given to \code{\link{dynamicGraphMain}}).

If a factor edge is added then the component \code{edgeList} with the
edges of the new model should be available in the returned structure.
New factor edges and factor vertices should also be avaliable in the
components \code{FactorEdges} and \code{FactorVertices}.

Similar for the key events "dropEdge", "addVertex" and "dropVertex".

The methods \code{graphComponents} and \code{setGraphComponents}
are used to communicate the graph components between several views
of the same model in \code{dynamicGraphMain}.
The method \code{graphComponents} of the model object is for returning
the objects to draw in the view, depending on the \code{viewType}.

If \code{NULL} is returned for a component in the returned list
from \code{graphComponents} then the corresponding value of
\code{Arguments} is used in \code{\link{redrawView}}.
To force an empty list, return the value \code{list()}
(or \code{numeric(0)} for \code{VisibleVertices}
and \code{VisibleBlocks}). See also the argument
\code{factorVertexList} of \code{\link{drawModel}}.

The method \code{setGraphComponents} of the model object
is called on the model object when the model is modified.

The methods \code{testEdge} of \code{object} should return
an object with the methods \code{label} and \code{width}
for labeling edges, see \code{\link{newDefaultTestObject}}.

% Objects can be created by calls of the form \code{new("dg.Model", ...)}.
}

\usage{newDefaultModelObject(name)}

\arguments{\item{name}{ Text string with the \code{name} of the model object.}}

% \details{ 

% This is an example of the object for interface between
% \code{\link{dynamicGraphMain}} and your models.

% The model object of the call of \code{\link{dynamicGraphMain}}
% should have the methods \code{modifyModel}, \code{testEdge}
% \code{graphComponents}, and \code{setGraphComponents}.

% When the graph is modified, by adding or dropping vertices or edge,
% the method \code{modifyModel} is called on the argument \code{object}
% of \code{\link{dynamicGraphMain}}. 

% If an \code{object} is returned in the list of the returned
% value from \code{modifyModel} then \code{object} in
% \code{\link{dynamicGraphMain}} is replaced by this object,
% and the object is also assigned in the top level environment,
% if \code{objectName} was given to \code{\link{dynamicGraphMain}}.

% The methods \code{graphComponents} and \code{setGraphComponents}
% are used to communicate the graph components between several views
% of the same model.
% The method \code{graphComponents} of the model object is for returning
% the objects to draw in the view, depending on the \code{viewType}.
% The method\code{setGraphComponents} of the model object
% is called on the model object when the model is modified.

% The methods \code{testEdge} of \code{object} should return
% an object with the methods \code{label} and \code{width}
% for labeling edges, see \code{\link{newDefaultTestObject}}.

% }

\value{An object of class \code{dg.Model}.}

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}:
      The \code{name} of the model. }
  }
}

\section{Methods}{
  \describe{
    % \item{vertexEdges<-}{\code{signature(x = "dg.Model")}: ... }
    % \item{vertexEdges}{\code{signature(object = "dg.Model")}: ... }
    \item{modifyModel}{\code{signature(object = "dg.Model")}: ... }
    % \item{returnGraphComponents}{\code{signature(object = "dg.Model")}: ... }
    \item{graphComponents}{\code{signature(object = "dg.Model")}: ... }
    \item{setGraphComponents}{\code{signature(object = "dg.Model")}: ... }
    \item{testEdge}{\code{signature(object = "dg.Model")}: ... }
  }
}

\author{Jens Henrik Badsberg}

% \references{ 
%     CoCo, with a guide at \url{http://www.jstatsoft.org/v06/i04/},
%     avaliable form \url{http://www.math.auc.dk/gr/material/CoCo/} and
%    \url{http://www.jbs.agrsci.dk/Biometry/Software-Datasets/CoCo/CoCo.1.6/}
%    has an interface to \code{\link{dynamicGraph}}. }

\references{ 
    CoCo, with a guide at \url{http://www.jstatsoft.org/v06/i04/},
    has an interface to \code{\link{dynamicGraph}}. }

\seealso{\code{\link{dg.Test-class}}.
% \code{\link{newDefaultModelObject}}, and \code{\link{newDefaultTestObject}}.
}

\examples{
# Part of the example "defaultObjects" of demo:


# Edit the following to meet your needs:
#
# - Change the name "your.Model"
#
# - Work out how the get names, types and edges from the model object.
#
# - At "message", insert the relevant code for testing and modifying the model.
#
# - The slots visibleVertices, visibleBlocks, extraVertices, vertexEdges, 
#   blockEdges, factorVertices, factorEdges, and, extraEdges should be
#   eliminated, and you should in "graphComponents" return relevant lists.
#



setClass("your.Model", 
         representation(name             = "character",
                        visibleVertices  = "numeric",
                        visibleBlocks    = "numeric",
                        extraVertices    = "dg.VertexList",
                        vertexEdges      = "dg.VertexEdgeList",
                        blockEdges       = "dg.BlockEdgeList",
                        factorVertices   = "dg.FactorVertexList",
                        factorEdges      = "dg.FactorEdgeList",
                        extraEdges       = "dg.ExtraEdgeList"))

"newYourModelObject"<- function(name) {
    result <- new("your.Model", name = name,
                  extraVertices  = .emptyDgList("dg.VertexList"),
                  vertexEdges    = .emptyDgList("dg.VertexEdgeList"),
                  blockEdges     = .emptyDgList("dg.BlockEdgeList"),
                  factorVertices = .emptyDgList("dg.FactorVertexList"),
                  factorEdges    = .emptyDgList("dg.FactorEdgeList"),
                  extraEdges     = .emptyDgList("dg.ExtraEdgeList"))
    return(result)
  }

if (!isGeneric("graphComponents")) {
  if (is.function("graphComponents"))
    fun <- graphComponents
  else
    fun <- function(object, viewType = NULL, ...)
    standardGeneric("graphComponents")
  setGeneric("graphComponents", fun)
}

setMethod("graphComponents", "your.Model",
          function(object, viewType = NULL, ...)
          { # print(viewType); print ("graphComponents")
            args <- list(...)
            Args <- args$Arguments
            Edges <- object@vertexEdges
            Vertices <- Args$vertexList
            VisibleVertices <- object@visibleVertices
            if (viewType == "Factor") {
              factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)
              if (is.null(factors) || (length(factors) == 0)) {
                FactorVertices  <- .emptyDgList("dg.FactorVertexList")
                FactorEdges     <- .emptyDgList("dg.FactorEdgeList")
              } else {
                result <- returnFactorVerticesAndEdges(Vertices, factors)
                FactorVertices  <- result$FactorVertices
                FactorEdges     <- result$FactorEdges
              }
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = object@blockEdges, 
                   factorVertices  = FactorVertices,
                   factorEdges     = FactorEdges,
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = object@visibleBlocks, 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Moral") {
              message("Moral view not implemented; ")
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = .emptyDgList("dg.BlockEdgeList"),
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = numeric(), 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Essential") {
              message("Essential view not implemented; ")
              list(vertexEdges      = object@vertexEdges, 
                   blockEdges      = .emptyDgList("dg.BlockEdgeList"),
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = numeric(), 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else if (viewType == "Simple") {
              list(vertexEdges     = object@vertexEdges, 
                   blockEdges      = object@blockEdges, 
                   factorVertices  = .emptyDgList("dg.FactorVertexList"),
                   factorEdges     = .emptyDgList("dg.FactorEdgeList"),
                   visibleVertices = object@visibleVertices, 
                   visibleBlocks   = object@visibleBlocks, 
                   extraVertices   = object@extraVertices, 
                   extraEdges      = object@extraEdges)
            } else 
              message("View type not implemented; ")
          })

if (!isGeneric("setGraphComponents")) {
  if (is.function("setGraphComponents"))
    fun <- setGraphComponents
  else
    fun <- function(object, viewType = NULL,
                    visibleVertices = NULL,
                    extraVertices   = NULL,
                    vertexEdges     = NULL,
                    blockEdges      = NULL,
                    factorVertices  = NULL,
                    factorEdges     = NULL,
                    extraEdges      = NULL, ...)
      standardGeneric("setGraphComponents")
  setGeneric("setGraphComponents", fun)
}

setMethod("setGraphComponents", signature(object = "your.Model"),
          function(object, viewType = NULL,
                   visibleVertices = NULL,
                   visibleBlocks   = NULL,
                   extraVertices   = NULL,
                   vertexEdges     = NULL,
                   blockEdges      = NULL,
                   factorVertices  = NULL,
                   factorEdges     = NULL,
                   extraEdges      = NULL, ...)
 {
    if (!is.null(visibleVertices)) object@visibleVertices <- visibleVertices
    if (!(viewType == "Moral"))
      if (!is.null(visibleBlocks  )) object@visibleBlocks   <- visibleBlocks
    if (!is.null(extraVertices  )) object@extraVertices   <- extraVertices
    if (!is.null(vertexEdges    )) object@vertexEdges     <- vertexEdges
    if (!is.null(blockEdges     )) object@blockEdges      <- blockEdges
    if ((viewType == "Factor")) {
      if (!is.null(factorVertices )) object@factorVertices  <- factorVertices
      if (!is.null(factorEdges    )) object@factorEdges     <- factorEdges
    }
    return(object)
 })


if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph"))
    fun <- dynamic.Graph
  else
    fun <- function(object, ...)
  standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}

setMethod("dynamic.Graph", signature(object = "your.Model"),
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

setMethod("testEdge", signature(object = "your.Model"),
          function(object, action, name.1, name.2, ...)
 {
    args <- list(...)
    from.type <- args$from.type
    to.type <- args$to.type
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    message(paste("Should return an object with the edge from",
                  name.1, f(from.type), "to", name.2, f(to.type),
                  "deleted from the argument object"))
    return(newYourTestObject())
 })


if (!isGeneric("modifyModel")) {
  if (is.function("modifyModel"))
    fun <- modifyModel
  else
    fun <- function(object, action, name, name.1, name.2, ...)
                    standardGeneric("modifyModel")
  setGeneric("modifyModel", fun)
}

setMethod("modifyModel", signature(object = "your.Model"),
          function(object, action, name, name.1, name.2, ...)
 {
    args <- list(...)
    Args <- args$Arguments
    Edges <- args$newEdges$vertexEdges
    Vertices <- Args$vertexList

    DoFactors <- FALSE
    if (!is.null(args$Arguments)
        && !is.null(args$Arguments$factorVertexList)
        && (length(args$Arguments$factorVertexList) > 0)
        && !is.null(args$Arguments$vertexList))
      DoFactors <- TRUE

    FactorVertices  <- NULL
    FactorEdges     <- NULL
    BlockEdges      <- NULL
    VisibleVertices <- Args$visibleVertices
    VisibleBlocks   <- Args$visibleBlocks
    ExtraVertices   <- NULL
    ExtraEdges      <- NULL

    f <- function(type) if (is.null(type)) "" else paste("(", type, ")")
    g <- function(type) if (is.null(type)) "" else type
    if (action == "dropEdge") {
      message(paste("Should return an object with the edge from",
                    name.1, f(args$from.type), "to", name.2, f(args$to.type),
                    "deleted from the argument object"))
      if ((g(args$from.type) == "Factor") || (g(args$from.type) == "Factor"))
        return(NULL)
    } else if (action == "addEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(args$from.type), "to", name.2, f(args$to.type),
                     "added to the argument object"))
      if ((g(args$from.type) == "Factor") || (g(args$from.type) == "Factor"))
        return(NULL)
    } else if (action == "dropVertex")  {
       message(paste("Should return an object with the vertex", 
                     name, f(args$type),
                     "deleted from the argument object"))
      if ((g(args$type) == "Factor"))
        return(NULL)
      VisibleVertices <- VisibleVertices[VisibleVertices != args$index]
      if (DoFactors && (args$index > 0)) {
        x <- (args$Arguments$factorVertexList)
        factors <- lapply(x, function(i) i@vertex.indices)
        types   <- lapply(x, function(i) class(i))
        factors <- lapply(factors, 
                          function(x) { 
                            y <- x[x != args$index]
                            if (length(y) > 0) return(y) else return(NULL) } )


        if (!is.null(factors)) {
          types   <- types[unlist(lapply(factors, function(i) !is.null(i)))]
          factors <- .removeNull(factors)
        }
        if (!is.null(factors)) {
          subset <- function(x)
            lapply(x, function(a) 
                        any(unlist(lapply(x, 
                                          function(A) 
                                            all(!is.na(match(a, A))) &&
                                            (length(a) < length(A))))))
          s <- subset(factors)
          types   <- types[!unlist(s)]
          factors <- factors[!unlist(s)]
          if (!(is.null(factors))) {
            result <- returnFactorVerticesAndEdges(
                            args$Arguments$vertexList, factors, types, 
                            factorClasses = validFactorClasses())
            FactorVertices <- result$FactorVertices
            FactorEdges <- result$FactorEdges
          }
        } else { 
          DoFactors <- FALSE
          FactorVertices <- .emptyDgList("dg.FactorVertexList")
          FactorEdges    <- .emptyDgList("dg.FactorEdgeList")
        }
      }
    } else if (action == "addVertex") {
      VisibleVertices <- c(VisibleVertices, args$index)
      message(paste("Should return an object with the vertex", 
                    name, f(args$type), args$index, 
                    "added to the argument object"))
      if (DoFactors && (args$index > 0)) {
        x <- (args$Arguments$factorVertexList)
        factors <- lapply(x, function(i) i@vertex.indices)
        types   <- lapply(x, function(i) class(i))
        if (!is.null(factors))
          factors <- .removeNull(factors)
        if (is.null(factors)) {
          factors <- list(args$index)
          types   <- validFactorClasses()[1, 1]
        } else { 
          n <- length(types)
          factors <- append(factors, list(args$index))
          types   <- append(types, types[n])
        }
        if (!(is.null(factors))) {
          result <- returnFactorVerticesAndEdges(
                          args$Arguments$vertexList, factors, types, 
                          factorClasses = validFactorClasses())
          FactorVertices <- result$FactorVertices
          FactorEdges <- result$FactorEdges } }
    }
    if (is.null(FactorVertices) && DoFactors && !is.null(Edges)) {

      factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)

      if (is.null(factors) || (length(factors) == 0)) {
        FactorVertices <- .emptyDgList("dg.FactorVertexList")
        FactorEdges    <- .emptyDgList("dg.FactorEdgeList")
      } else {
        result <- returnFactorVerticesAndEdges(Vertices, factors)
        FactorVertices  <- result$FactorVertices
        FactorEdges     <- result$FactorEdges } }
    return(list(object          = object,
                BlockEdges      = BlockEdges, 
                FactorVertices  = FactorVertices,
                FactorEdges     = FactorEdges,
                VisibleVertices = VisibleVertices, 
                VisibleBlocks   = VisibleBlocks, 
                ExtraVertices   = ExtraVertices,
                ExtraEdges      = ExtraEdges))
 })

setMethod("Str", "your.Model",
          function(object, setRowLabels = FALSE, title = "", ...) {
              message(object@name) })

newYourModelObject("ModelObject")
}

\keyword{classes}
