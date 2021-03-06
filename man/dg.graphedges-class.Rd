\name{dg.graphedges-class}

\docType{class}

% \alias{coerce,dg.simple.graph,dg.graph-method}

\alias{dg.graphedges-class}

\alias{show,dg.graphedges-method}
\alias{Str,dg.graphedges-method}

\alias{addModel,dg.graphedges-method}
\alias{addView,dg.graphedges-method}
\alias{replaceModel,dg.graphedges-method}
\alias{replaceView,dg.graphedges-method}

\alias{blockEdgeList<-,dg.graphedges-method}
\alias{blockEdgeList,dg.graphedges-method}
\alias{blockList<-,dg.graphedges-method}
\alias{blockList,dg.graphedges-method}
\alias{edgeList<-,dg.graphedges-method}
\alias{edgeList,dg.graphedges-method}
\alias{extraEdgeList<-,dg.graphedges-method}
\alias{extraEdgeList,dg.graphedges-method}
\alias{extraList<-,dg.graphedges-method}
\alias{extraList,dg.graphedges-method}
\alias{factorEdgeList<-,dg.graphedges-method}
\alias{factorEdgeList,dg.graphedges-method}
\alias{factorVertexList<-,dg.graphedges-method}
\alias{factorVertexList,dg.graphedges-method}
\alias{oriented,dg.graphedges-method}
\alias{viewType<-,dg.graphedges-method}
\alias{viewType,dg.graphedges-method}
\alias{visibleBlocks<-,dg.graphedges-method}
\alias{visibleBlocks,dg.graphedges-method}
\alias{visibleVertices<-,dg.graphedges-method}
\alias{visibleVertices,dg.graphedges-method}

\title{Class dg.graphedges}

\description{ The representation of a graph for dynamicGraph. 
              Vertices, blocks, viewType, edges, etc. 
              are here the dynamicGraph objects. }

\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("dg.graphedges", ...)}.
}

\section{Slots}{
  \describe{

    \item{\code{viewType}:}{Object of class \code{"character"}
                            with the type of view. }

    \item{\code{visibleVertices}:}{Object of class \code{"numeric"}:
        Numeric vector of the indices of the vertices of
        \code{vertexList} to plot. }

    \item{\code{visibleBlocks}:}{Object of class \code{"numeric"}:
        Numeric vector of the indices of the blocks of
        \code{blockList} to plot. }

    \item{\code{oriented}:}{Object of class \code{"logical"}:
        If TRUE (or FALSE) then edges are oriented (or not), 
        also when blocks are missing. 
        If NA then the edges are directed according to the blocks
        of the edge. }

    \item{\code{edgeList}:}{Object of class \code{"dg.VertexEdgeList"}:
        List of edges (of class containing \code{dg.Edge})
        created by \code{\link{returnEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}.  }

    \item{\code{blockEdgeList}:}{Object of class \code{"dg.BlockEdgeList"}:
        List of blockedges
        (of class containing the class \code{dg.BlockEdge})
        created by \code{\link{returnBlockEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}. }

    \item{\code{factorVertexList}:}{Object of class
                                    \code{"dg.FactorVertexList"}:
        List of secondary vertices,
        called factor vertices, for, e.g., the generators of the model,
        (each of class containing \code{dg.FactorVertex})
        created by \code{\link{returnFactorVerticesAndEdges}}
        or exported from \code{\link{dynamicGraphMain}}. }

    \item{\code{factorEdgeList}:}{Object of
        class \code{"dg.FactorEdgeList"}:
        List of bipartite graph edges, called factor edges,
        (each of class containing \code{dg.FactorEdge})
        created by the function \code{\link{returnFactorEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}.
        Factor edges are edges between vertices and factor vertices. }

    \item{\code{extraList}:}{Object of class \code{"dg.VertexList"}:
        List of vertices (of class containing the class \code{dg.Vertex}) 
        created by the function \code{\link{returnVertexList}}
        or exported from the function \code{\link{dynamicGraphMain}},
        for, e.g., additional titles in the plot. }

    \item{\code{extraEdgeList}:}{Object of class \code{"dg.ExtraEdgeList"}:
          List of edges between extra vertices and vertices. }
  }
}

\section{Methods}{
  \describe{

    \item{addModel}{\code{signature(object = "dg.graphedges")}: ... }
    \item{addView}{\code{signature(object = "dg.graphedges")}: ... }
    \item{replaceModel}{\code{signature(object = "dg.graphedges")}: ... }
    \item{replaceView}{\code{signature(object = "dg.graphedges")}: ... }

    \item{show}{\code{signature(object = "dg.graphedges")}: ... }
    \item{Str}{\code{signature(object = "dg.graphedges")}: ... }

    \item{blockEdgeList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{blockEdgeList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{blockList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{blockList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{edgeList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{edgeList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{extraEdgeList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{extraEdgeList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{extraList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{extraList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{factorEdgeList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{factorEdgeList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{factorVertexList<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{factorVertexList}{\code{signature(object = "dg.graphedges")}: ... }
    \item{oriented}{\code{signature(object = "dg.graphedges")}: ... }
    \item{viewType<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{viewType}{\code{signature(object = "dg.graphedges")}: ... }
    \item{visibleBlocks<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{visibleBlocks}{\code{signature(object = "dg.graphedges")}: ... }
    \item{visibleVertices<-}{\code{signature(x = "dg.graphedges")}: ... }
    \item{visibleVertices}{\code{signature(object = "dg.graphedges")}: ... }

  }
}

\author{Jens Henrik Badsberg}

\seealso{
  \code{\link{dg.simple.graph-class}}, \code{\link{dg.graph-class}},
  and \code{\link{dynamicGraphMain}}.
}

\examples{

# The use of "addModel"  and "addModel"
# in the example "usermenus" of demo:

your.DrawModel <- function(object, slave = FALSE, viewType = "Simple", ...) {

    dots <- list(...)
    localArguments    <- dots$Arguments

    # Here you should make your new model.
    # A copy is made by the following:

    ModelObject <- object

    title <- ModelObject@name

    # and compute graph edges:

    dgEdges <- graphEdges(ModelObject, viewType, Arguments = localArguments)

    show(dgEdges)

    if (slave) {
      # Drawing ''an other model'' in a new window:
      addModel(dgEdges, 
               frameModels =        localArguments$frameModels, 
               modelObject =        ModelObject, 
               modelObjectName =    ModelObject@name)
    }
    else {
      # Overwriting with ''an other model'' in same view:
      replaceModel(dgEdges,
                   frameModels     = localArguments$frameModels, 
                   modelIndex      = localArguments$modelIndex, 
                   viewIndex       = localArguments$viewIndex, 
                   modelObject     = ModelObject, 
                   modelObjectName = ModelObject@name) 
    }
}

your.LabelAllEdges <- function(object, slave = FALSE, ...) 
 {

  dots           <- list(...)
  localArguments <-      dots$Arguments
  dg             <-      localArguments$dg

  # browser()

  getNodeName <- function(index, type)
    if (type == "Vertex")
      name(localArguments$frameModel@vertices[[index]])
    else if (type == "Factor")
      name(localArguments$dg@factorVertexList[[abs(index)]])
    else if (type == "Extra")
      name(localArguments$dg@extraList[[abs(index)]])
    else if (type == "Block")
      label(localArguments$dg@blockList[[abs(index)]])
    else
      NULL

  visitEdges <- function(edges) {
    for (i in seq(along = edges)) {
      vertices <- nodeIndicesOfEdge(edges[[i]])
      types    <- nodeTypesOfEdge(edges[[i]])

      name.f <- getNodeName(vertices[1], types[1])
      name.t <- getNodeName(vertices[2], types[2])

      R <- testEdge(object, action = "remove",
                    name.1 = name.f, name.2 = name.t,
                    from = vertices[1], to = vertices[2],
                    from.type = types[1], to.type = types[2],
                    edge.index = i, force = force, Arguments = localArguments)

      if (!is.null(R)) {
        if (TRUE || (hasMethod("label", class(R))))
          label(edges[[i]]) <- label(R)
        if (TRUE || (hasMethod("width", class(R))))
          width(edges[[i]]) <- width(R)
      }
    }
    return(edges)
  }

  dg@edgeList       <- visitEdges(dg@edgeList)
  dg@factorEdgeList <- visitEdges(dg@factorEdgeList)
  dg@blockEdgeList  <- visitEdges(dg@blockEdgeList)

  if (slave) {
    # Drawing ''an other model'' in a new window:
    addModel(dg, 
             frameModels     = localArguments$frameModels, 
             modelObject     = localArguments$object, 
             modelObjectName = localArguments$object@name)
  }
  else {
    # Overwriting with ''an other model'' in same view:
    replaceModel(dg,
                 frameModels     = localArguments$frameModels, 
                 frameViews      = localArguments$frameViews, 
                 graphWindow     = localArguments$graphWindow, 
                 modelObject     = localArguments$object, 
                 modelObjectName = localArguments$object@name)
  }

}

 test.function <- function(...) print("Test.Function")
 test.function <- function(...) print(list(...)$Arguments$object@name)

 Menus <- 
 list(MainUser = 
      list(label = 
      "Transformation by 'prcomp' on position of \"vertices\", and redraw",
           command = function(object, ...) {
             localArguments <- list(...)$Arguments
             transformation <- 
                  t(prcomp(Positions(localArguments$vertexList))$rotation)
             control <- localArguments$control
             control$transformation <- transformation
             replaceControls(control, 
                             frameModels = localArguments$frameModels, 
                             modelIndex  = localArguments$modelIndex,
                             viewIndex   = localArguments$viewIndex,
                             Arguments   = localArguments)
             }),
      MainUser = 
      list(label = "Position of \"vertices\" by 'cmdscale', and redraw",
           command = function(object, ...) {
             localArguments <- list(...)$Arguments
             Vertices <- localArguments$dg@vertexList
             Edges <- localArguments$dg@edgeList
             positions <- Positions(localArguments$dg@vertexList)
             N <- dim(positions)[2]
             e <- NodeIndices(Edges)
             n <- Names(Vertices)
             X <- matrix(rep(-1, length(n)^2), ncol = length(n))
             for (i in 1:length(e)) {
               suppressWarnings(w <- as.numeric(names(e)[i]))
               if (is.na(w)) w <- .5
               X[e[[i]][1], e[[i]][2]] <- w
               X[e[[i]][2], e[[i]][1]] <- w
             }
             dimnames(X) <- list(n, n)
             d <- 1.25
             X[X==-1] <- d
             X <- X - d * diag(length(n))
             mdsX <- cmdscale(X, k = N, add = TRUE, eig = TRUE, x.ret = TRUE)
             # mdsX <- isoMDS(X, k = N)
             M <- max(abs(mdsX$points))
             Positions(localArguments$dg@vertexList) <<- mdsX$points / M * 45

             # replaceVertexList(localArguments$vertexList, 
             #                   frameModels = localArguments$frameModels, 
             #                   modelIndex  = localArguments$modelIndex,
             #                   viewIndex   = localArguments$viewIndex,
             #                   Arguments   = localArguments)

             vertices(localArguments$frameModels) <- 
                                                localArguments$dg@vertexList
             }),
      MainUser = 
      list(label = "Position of \"vertices\"",
           command = function(object, ...) 
             print(Positions(list(...)$Arguments$vertexList))),
      MainUser = 
      list(label = "Label all edges, in this window",
           command = function(object, ...) 
                       your.LabelAllEdges(object, slave = FALSE, ...)),
      MainUser = 
      list(label = "Label all edges, in slave window",
           command = function(object, ...) 
                       your.LabelAllEdges(object, slave = TRUE, ...)),
      MainUser = 
      list(label = "Draw model, in this window",
           command = function(object, ...) 
                       your.DrawModel(object, slave = FALSE, ...)),
      MainUser = 
      list(label = "Draw model, in slave window",
           command = function(object, ...) 
                       your.DrawModel(object, slave = TRUE, ...)),
      MainUser = 
      list(label = "Call of function 'modalDialog', result on 'title' at top",
           command = function(object, ...) 
           {
             localArguments <- list(...)$Arguments
             ReturnVal <- modalDialog("Test modalDialog Entry",
                                      "Enter name", 
                                      localArguments$control$title, 
                                      top = localArguments$top)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return()
             tktitle(localArguments$top) <- ReturnVal
           }
          ),
      MainUser = 
      list(label = 
           "Call of function 'test.function', result on 'viewLabel' at bottom",
           command = function(object, ...)
           {
             localArguments <- list(...)$Arguments
             tkconfigure(localArguments$viewLabel, 
                         text = paste(localArguments$dg@viewType, " | ", 
                                      test.function(...))) } ),
      MainUser = 
      list(label = 
           "Test of 'Arguments'",
           command = function(object, ...)
           {

             dots              <- list(...)
             localArguments    <- list(...)$Arguments

             print(names(dots))
             print(names(localArguments))

             # Only the nine values
             # "frameModels", "modelIndex", "viewIndex",
             # "object", "objectName",
             # "selectedNodes", "selectedEdges",
             # "closedBlock", and "hiddenBlock "
             # should be extracted from "list(...)$Arguments":

             frameModels       <- localArguments$frameModels

             modelIndex        <- localArguments$modelIndex
             viewIndex         <- localArguments$viewIndex

             object            <- localArguments$object
             objectName        <- localArguments$objectName

             selectedNodes     <- localArguments$selectedNodes
             selectedEdges     <- localArguments$selectedEdges
             closedBlock       <- localArguments$closedBlock
             hiddenBlock       <- localArguments$hiddenBlock

             #  "control" can alse be extracted from "framModels"

             Control           <- localArguments$control

             control           <- frameModels@control

             # These 3 are 'deprecated':

             drawModel         <- localArguments$drawModel   #
             redrawView        <- localArguments$redrawView  #

             envir             <- localArguments$envir       #

             # The following are currently retained in "list(...)$Arguments", 
             # but can be extracted from the above teen values:

             frameViews        <- frameModels@models[[modelIndex]]
             graphWindow       <- frameViews@graphs[[viewIndex]]

             vertexList        <- frameModels@vertices
             blockList         <- frameModels@blocks

             dg                <- graphWindow@dg

             visibleVertices   <- visibleVertices(dg)
             visibleBlocks     <- visibleBlocks(dg)
             edgeList          <- edgeList(dg)
             oriented          <- oriented(dg)
             blockEdgeList     <- blockEdgeList(dg)
             factorVertexList  <- factorVertexList(dg)
             factorEdgeList    <- factorEdgeList(dg)
             extraList         <- extraList(dg)
             extraEdgeList     <- extraEdgeList(dg)

             viewType          <- viewType(dg)

             top               <- top(graphWindow)
             box               <- vbox(graphWindow)
             canvas            <- canvas(graphWindow)
             viewLabel         <- viewLabel(graphWindow)
             tags              <- tags(graphWindow)

             # The values now in 'control' are no longer 
             # avaliable in "list(...)$Arguments":

             title             <- control$label # bad example since the  
                                                # name "title" has been  
                                                # change to "label".

             vertexColor       <- control$vertexColor

             print(names(control))

             browser()

             } ),

      Vertex = 
      list(label = "Test of user popup menu for vertices: Label",
           command = function(object, name, ...) 
           {
             # print(name)
             dots <- list(...)
             # print(names(dots))
             # print(c(dots$type))
             # print(c(dots$index))
             localArguments <- dots$Arguments
             # print(names(localArguments))
             # str(localArguments$selectedNodes)
             # str(localArguments$selectedEdges)
             print(localArguments$dg@vertexList[[dots$index]]@label)
           }
          ),
      Edge = 
      list(label = "Test of user popup menu for edges: Class",
           command = function(object, name1, name2, ...) 
           {
             dots           <- list(...)
             localArguments <-      dots$Arguments

             # print(c(name1, name2))
             # print(c(dots$edge.index, dots$which.edge, dots$from, dots$to))
             # print(c(dots$from.type, dots$to.type, dots$edge.type))
             # print(names(localArguments))
             # str(localArguments$selectedNodes)
             # str(localArguments$selectedEdges)

             ReturnVal <- selectDialog("Test selectDialog Entry", 
                                       "Select name", 
                                       localArguments$control$edgeClasses[,1], 
                                       top = localArguments$top)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return()
             if ((dots$from > 0) && (dots$to > 0)) {
               dg <- localArguments$dg
               class(dg@edgeList[[dots$edge.index]]) <- 
                           localArguments$control$edgeClasses[ReturnVal, 2]
               # replaceView ?
               replaceModel(dg,
                   frameModels     = localArguments$frameModels, 
                   modelIndex      = localArguments$modelIndex, 
                   viewIndex       = localArguments$viewIndex, 
                   modelObject     = localArguments$object, 
                   modelObjectName = localArguments$objectName) 
             }
           }
          ),
      ClosedBlock = 
      list(label = "Test of user popup menu for blocks",
           command = function(object, name, ...) 
           {
             print(name)
             print(c(list(...)$index))
           }
          )
     )

}

\keyword{classes}
