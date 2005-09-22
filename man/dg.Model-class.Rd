\name{dg.Model-class}
\docType{class}

\alias{dg.Model-class}

\alias{modifyModel,dg.Model-method}
\alias{testEdge,dg.Model-method}

\alias{initialize,dg.Model-method}

\alias{setGraphComponents,dg.Model-method}
\alias{returnGraphComponents,dg.Model-method}
\alias{graphComponents,dg.Model-method}

\alias{setGraphEdges,dg.Model-method}
\alias{graphEdges,dg.Model-method}
\alias{setSlots,dg.Model-method}

% \alias{vertexEdges,dg.Model-method}
% \alias{vertexEdges<-,dg.Model-method}

\alias{Str,dg.Model-method}

\alias{setGraphComponents}
\alias{returnGraphComponents}
\alias{graphComponents}

\alias{setGraphEdges}
\alias{graphEdges}

% \alias{vertexEdges}
% \alias{vertexEdges<-}

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
\code{graphEdges} and \code{setGraphEdges}.

When the graph is modified, by adding or dropping vertices or edge,
the method \code{modifyModel} is called on the argument \code{object}
of \code{\link{dynamicGraphMain}}. 

If a value different from NULL is returned from \code{modifyModel} at the 
key event "add edge" then the edge is added to the the view of the model,
the graph window.

If NULL is returned from \code{modifyModel} then the methods
\code{\link{addModel}} and \code{\link{replaceModel}}
can be used to draw the new graph inside \code{modifyModel}.
If a value different from NULL is returned from \code{modifyModel} then 
the methods \code{\link{addView}} ,\code{\link{addModel}},
\code{\link{replaceView}}, and \code{\link{replaceModel}}
should not be called from \code{modifyModel} for the action add edge.

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

The methods \code{graphEdges} and \code{setGraphEdges}
are used to communicate the graph components between several views
of the same model in \code{dynamicGraphMain}.

The method \code{graphEdges} of the model object is for returning
an object of class \code{\link{dg.graphedges-class}}
to draw in the view, depending on the \code{viewType}.

[[ UPDATE:
If \code{NULL} is returned for a component in the returned list
from \code{graphEdges} then the corresponding value of
\code{Arguments} is used in \code{\link{redrawView}}.
To force an empty list, return the value \code{list()}
(or \code{numeric(0)} for \code{VisibleVertices}
and \code{VisibleBlocks}). See also the argument
\code{factorVertexList} of \code{\link{drawModel}}. ]]

The method \code{setGraphEdges} of the model object
is called on the model object when the model is modified.

The methods \code{testEdge} of \code{object} should return
an object with the methods \code{label} and \code{width}
for labeling edges, see \code{\link{dg.Test-class}}.

% Objects can be created by calls of the form \code{new("dg.Model", ...)}.
}

% \usage{newDefaultModelObject(name)}

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
    \item{graphEdges}{\code{signature(object = "dg.Model")}: ... }
    \item{initialize}{\code{signature(.Object = "dg.Model")}: ... }
    \item{setGraphEdges}{\code{signature(object = "dg.Model")}: ... }
    \item{Str}{\code{signature(object = "dg.Model")}: ... }
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

% # "newYourModelObject" <- function(name)
% #   return(new("your.Model", name = name, dg = new("dg.graphedges")))

\examples{
# Part of the example "defaultObjects" of demo:


# Edit the following to meet your needs:
#
# - Change the name "your.Model"
#
# - Work out how the get names, types and edges from your model object.
#
# - At "message", insert the relevant code for testing and modifying the model.
#
# - The slots visibleVertices, visibleBlocks, extraVertices, edges, 
#    blockEdges, factorVertices, factorEdges should be eliminated,
#    and you should in "graphEdges" return relevant lists.
#

setClass("your.Model", 
         representation(name = "character",
                        dg   = "dg.graphedges"))

# "newDefaultModelObject"<-
#   function(name)
#   {
#     result <- new("your.Model", name = name, dg = new("dg.graphedges"))
#     return(result)
#   }

setMethod("setSlots", "your.Model",
          function(object, arguments) { 
            for (i in seq(along = arguments)) {
              name <- names(arguments)[i]
              if (is.element(name, slotNames(object)))
                slot(object, name) <- arguments[[i]]
              else
                message(paste("Argument '", name, "' not valid slot of '", 
                              class(object), "', thus ignored.",
                              sep = "")) }
            return(object)
          })

setMethod("initialize", "your.Model",
          function(.Object, ...) {
              # print(c("initialize", "your.Model", class(.Object)))
              Args <- list(...)
              .Object <- setSlots(.Object, Args)
              return(.Object)
            }
          )

setMethod("graphEdges", "your.Model",
          function(object, viewType = NULL, ...)
          { # print(viewType); print ("graphEdges")

            dots           <- list(...)
            localArguments <-      dots$Arguments
            Vertices       <-      localArguments$vertexList

            Edges           <- object@dg@edgeList
            VisibleVertices <- object@dg@visibleVertices

            if (viewType == "Factor") {
              factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)
              # print(factors)
              if (is.null(factors) || (length(factors) == 0)) {
                FactorVertices  <- new("dg.FactorVertexList")
                FactorEdges     <- new("dg.FactorEdgeList")
              } else {
                result <- returnFactorVerticesAndEdges(Vertices, factors)
                FactorVertices  <- result$FactorVertices
                FactorEdges     <- result$FactorEdges
              }
              new("dg.graphedges", 
                   viewType         = viewType, 
                   oriented         = object@dg@oriented, 
                   edgeList         = object@dg@edgeList, 
                   blockEdgeList    = object@dg@blockEdgeList, 
                   factorVertexList = FactorVertices,
                   factorEdgeList   = FactorEdges,
                   visibleVertices  = object@dg@visibleVertices, 
                   visibleBlocks    = object@dg@visibleBlocks, 
                   extraList        = object@dg@extraList, 
                   extraEdgeList    = object@dg@extraEdgeList)
            } else if (viewType == "Moral") {
              message("Moral view not implemented; ")
              new("dg.graphedges", 
                   viewType         = viewType, 
                   oriented         = object@dg@oriented, 
                   edgeList         = object@dg@edgeList, 
                 # blockEdgeList    = new("dg.BlockEdgeList"),
                 # factorVertexList = new("dg.FactorVertexList"),
                 # factorEdgeList   = new("dg.FactorEdgeList"),
                   visibleVertices  = object@dg@visibleVertices, 
                   visibleBlocks    = numeric(), 
                   extraList        = object@dg@extraList, 
                   extraEdgeList    = object@dg@extraEdgeList)
            } else if (viewType == "Essential") {
              message("Essential view not implemented; ")
              new("dg.graphedges", 
                   viewType         = viewType, 
                   oriented         = object@dg@oriented, 
                   edgeList         = object@dg@edgeList, 
                 # blockEdgeList    = new("dg.BlockEdgeList"),
                 # factorVertexList = new("dg.FactorVertexList"),
                 # factorEdgeList   = new("dg.FactorEdgeList"),
                   visibleVertices  = object@dg@visibleVertices, 
                   visibleBlocks    = numeric(), 
                   extraList        = object@dg@extraList, 
                   extraEdgeList    = object@dg@extraEdgeList)
            } else if (viewType == "Simple") {
              new("dg.graphedges", 
                   viewType         = viewType, 
                   oriented         = object@dg@oriented, 
                   edgeList         = object@dg@edgeList, 
                   blockEdgeList    = object@dg@blockEdgeList, 
                 # factorVertexList = new("dg.FactorVertexList"),
                 # factorEdgeList   = new("dg.FactorEdgeList"),
                   visibleVertices  = object@dg@visibleVertices, 
                   visibleBlocks    = object@dg@visibleBlocks, 
                   extraList        = object@dg@extraList, 
                   extraEdgeList    = object@dg@extraEdgeList)
            } else 
              message("View type not implemented; ")
          })

setMethod("setGraphEdges", signature(object = "your.Model"),
          function(object, dg = NULL, ...)
 {
    if (!is.null(dg)) object@dg <- dg
    return(object)
 })


setMethod("dg", signature(object = "your.Model"),
          function(object, 
                   modelObject = NULL,
                   modelObjectName = NULL,
                   control = dg.control(...), 
                   ...) 
  {

    Names <- Your.function.for.extracting.variable.names.from.object(
             object = object)
    Types <- Your.function.for.extracting.variable.types.from.object(
             object = object)
    Edges <- Your.function.for.extracting.variable.edges.from.object(
             object = object)

    simpleGraph <- new("dg.simple.graph", vertex.names = Names, 
                      types = Types, # edge.list = Edges,
                      from = Edges[,1], to = Edges[,2])

    graph <- simpleGraphToGraph(simpleGraph)

    dg(graph, object = object, ...)

 })


setMethod("testEdge", signature(object = "your.Model"),
          function(object, action, name.1, name.2, ...)
 {
    dots <- list(...)
    from.type <- dots$from.type
    to.type <- dots$to.type
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    message(paste("Should return an object with the edge from",
                  name.1, f(from.type), "to", name.2, f(to.type),
                  "deleted from the argument object"))
    return(new("your.Test", name = "TestObject"))
 })


setMethod("modifyModel", signature(object = "your.Model"),
          function(object, action, name, name.1, name.2, ...)
 {
    dots           <- list(...)
    localArguments <-      dots$Arguments
    Edges          <-      dots$newEdges$vertexEdges
    Vertices       <-      localArguments$vertexList

    viewType <- "Simple"

    DoFactors <- FALSE
    if (!is.null(dots$Arguments)
        && !is.null(dots$Arguments$factorVertexList)
        && (length(dots$Arguments$factorVertexList) > 0)
        && !is.null(dots$Arguments$vertexList))
      DoFactors <- TRUE

    if (DoFactors)
      viewType <- "Factor"

    # print(names(dots))
    # str(dots)

    # print(names(localArguments))

    # print(localArguments$visibleVertices)

    # str(localArguments$selectedNodes)
    # if (length(dots$selectedNodes) > 0)
    #   str(dots$selectedNodes)

    # str(localArguments$selectedEdges)
    # if (length(dots$selectedEdges) > 0)
    #   str(dots$selectedEdges)

    FactorVertices  <- new("dg.FactorVertexList")
    FactorEdges     <- new("dg.FactorEdgeList")
    BlockEdges      <- new("dg.BlockEdgeList")
    VisibleVertices <- localArguments$visibleVertices
    VisibleBlocks   <- localArguments$visibleBlocks
    ExtraVertices   <- new("dg.VertexList")
    ExtraEdges      <- new("dg.ExtraEdgeList")

    f <- function(type) if (is.null(type)) "" else paste("(", type, ")")
    g <- function(type) if (is.null(type)) "" else type
    if (action == "dropEdge") {
      message(paste("Should return an object with the edge from",
                    name.1, f(dots$from.type), "to", name.2, f(dots$to.type),
                    "deleted from the argument object"))
      if ((g(dots$from.type) == "Factor") || (g(dots$from.type) == "Factor"))
        return(NULL)
    } else if (action == "addEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(dots$from.type), "to", name.2, f(dots$to.type),
                     "added to the argument object"))
      if ((g(dots$from.type) == "Factor") || (g(dots$from.type) == "Factor"))
        return(NULL)
    } else if (action == "dropVertex")  {
       message(paste("Should return an object with the vertex", 
                     name, f(dots$type),
                     "deleted from the argument object"))
      if ((g(dots$type) == "Factor"))
        return(NULL)
      VisibleVertices <- VisibleVertices[VisibleVertices != dots$index]
      if (DoFactors && (dots$index > 0)) {
        x <- (localArguments$factorVertexList)
        factors <- lapply(x, function(i) i@vertex.indices)
        types   <- lapply(x, function(i) class(i))
        factors <- lapply(factors, 
                          function(x) { 
                            y <- x[x != dots$index]
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
                            localArguments$vertexList, factors, types, 
                            factorClasses = validFactorClasses())
            FactorVertices <- result$FactorVertices
            FactorEdges <- result$FactorEdges
          }
        } else { 
          DoFactors <- FALSE
          FactorVertices <- new("dg.FactorVertexList")
          FactorEdges    <- new("dg.FactorEdgeList")
        }
      }
    } else if (action == "addVertex") {
      VisibleVertices <- c(VisibleVertices, dots$index)
      message(paste("Should return an object with the vertex", 
                    name, f(dots$type), dots$index, 
                    "added to the argument object"))
      if (DoFactors && (dots$index > 0)) {
        x <- (localArguments$factorVertexList)
        factors <- lapply(x, function(i) i@vertex.indices)
        types   <- lapply(x, function(i) class(i))
        if (!is.null(factors))
          factors <- .removeNull(factors)
        if (is.null(factors)) {
          factors <- list(dots$index)
          types   <- validFactorClasses()[1, 1]
        } else { 
          n <- length(types)
          factors <- append(factors, list(dots$index))
          types   <- append(types, types[n])
        }
        if (!(is.null(factors))) {
          result <- returnFactorVerticesAndEdges(
                          localArguments$vertexList, factors, types, 
                          factorClasses = validFactorClasses())
          FactorVertices <- result$FactorVertices
          FactorEdges <- result$FactorEdges
        }
      }
    }
    if (is.null(FactorVertices) && DoFactors && !is.null(Edges)) {

      factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)

      if (is.null(factors) || (length(factors) == 0)) {
        FactorVertices <- new("dg.FactorVertexList")
        FactorEdges    <- new("dg.FactorEdgeList")
      } else {
        result <- returnFactorVerticesAndEdges(Vertices, factors)
        FactorVertices  <- result$FactorVertices
        FactorEdges     <- result$FactorEdges
      }
    }
    dg <- new("dg.graphedges", 
              edgeList         = Edges,
              viewType         = viewType, 
            # oriented         = oriented, 
              blockEdgeList    = BlockEdges, 
              factorVertexList = FactorVertices,
              factorEdgeList   = FactorEdges,
              visibleVertices  = VisibleVertices, 
              visibleBlocks    = VisibleBlocks, 
              extraList        = ExtraVertices,
              extraEdgeList    = ExtraEdges)
    ".IsEmpty" <- function(x) {
      if (is.null(x) || (length(x) == 0) ||
          (length(x) == 1) && is.null(x[[1]]))
        return(TRUE)
      else
        return(FALSE)
      }
    if (.IsEmpty(FactorEdges) && (viewType == "Factor")) {
      object <- setGraphEdges(object, dg = dg)
      graphContent <- graphEdges(object, viewType = viewType, 
                                 Arguments = localArguments)
      dg <- graphContent
    }
    return(list(object  = object, dg = dg))
 })

setMethod("Str", "your.Model",
          function(object, setRowLabels = FALSE, title = "", ...) {
              message(object@name) })


new("your.Model", name = "YourModelObject")

}

\keyword{classes}
