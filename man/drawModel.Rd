\name{drawModel}

\alias{drawModel}
\alias{redrawView}
\alias{redrawGraphWindow}
\alias{menu}

\title{Draw the dynamicGraph window and slaves }

% \newpage

\description{
  The functions \code{drawModel} and \code{redrawView} within
  \code{dynamicGraph} is for adding models to \code{dynamicGraph},
  for adding new views of a model, and for overwriting an existing
  view with an other model.

  The functions can not be found at top level.
}

% \usage{
% redrawGraphWindow(graphLattice = NULL, graphWindow = NULL, 
%                   edgeList = NULL, blockEdgeList = NULL, 
%                   factorVertexList = NULL, factorEdgeList = NULL, 
%                   visibleVertices = NULL, extraList = NULL, 
%                   object = NULL, title = "dynamicGraph", 
%                   transformation = NULL, width = NULL, height = NULL, 
%                   w = NULL, 
%                   vertexColor = NULL, extraVertexColor = NULL, 
%                   edgeColor = NULL, factorVertexColor = NULL, 
%                   factorEdgeColor = NULL, blockEdgeColor = NULL, 
%                   blockColors = NULL, background = NULL, ...)
% 
% drawModel(frameModels = NULL, frameViews = NULL, graphWindow = NULL, 
%           edgeList = NULL, oriented = FALSE, blockEdgeList = NULL, 
%           factorVertexList = NULL, factorEdgeList = NULL, 
%           visibleVertices = NULL, extraList = NULL, 
%           object = NULL, viewType = NULL, title = "dynamicGraph", 
%           transformation = NULL, width = NULL, height = NULL, w = NULL, 
%           vertexColor = NULL, extraVertexColor = NULL, edgeColor = NULL, 
%           factorVertexColor = NULL, factorEdgeColor = NULL, 
%           blockEdgeColor = NULL, blockColors = NULL, 
%           background = NULL, initialWindow = FALSE, 
%           returnLink = FALSE, returnNull = FALSE, ...)
% 
% redrawView(frameModels = NULL, frameViews = NULL, graphWindow = NULL, 
%            edgeList = NULL, oriented = FALSE, blockEdgeList = NULL, 
%            factorVertexList = NULL, factorEdgeList = NULL, 
%            visibleVertices = NULL, extraList = NULL, 
%            viewType = NULL, title = "dynamicGraph", 
%            transformation = NULL, width = NULL, height = NULL, w = NULL, 
%            vertexColor = NULL, extraVertexColor = NULL, edgeColor = NULL, 
%            factorVertexColor = NULL, factorEdgeColor = NULL, 
%            blockEdgeColor = NULL, blockColors = NULL, 
%            background = NULL, initialWindow = FALSE, 
%            returnLink = FALSE, returnNull = FALSE, 
%            setUpdateCountModelMain = FALSE, ...)
% 
% }


\arguments{
  \item{frameModels}{ An object of class \code{\link{DynamicGraph-class}}.
		      NULL,
                      or \code{frameModels} of \code{list(...)$Arguments}. }
  \item{frameViews}{ An object of class \code{\link{DynamicGraphModel-class}}.
                     NULL,
                     or \code{frameViews} of \code{list(...)$Arguments}.
                     If \code{frameViews} is set to \code{NULL}, 
                     the default value, then a new model frame will be created
		     by \code{drawModel}. }
  \item{graphWindow}{ An object of class \code{\link{DynamicGraphView-class}}.
		      If \code{graphWindow} is set to the value of 
                      \code{list(...)$Arguments$graphWindow} then
                      the calling graph window will be redrawn.
                      If \code{graphWindow} is set to \code{NULL}, 
                      the default value,
                      then a new slave graph window will be drawn. }
  \item{graphComponent}{ Returned structure from \code{graphComponent}. }
  \item{edgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{edgeList} is given (set to a value different from NULL)
        then this value is used, else the value extracted from
        \code{graphComponent} or \code{list(...)$Arguments} is used.
        If the value is not NULL in \code{graphComponent} then this
	value is used, else the value from \code{list(...)$Arguments}
        is used when \code{edgeList} is NULL. }
  \item{oriented}{ As for \code{\link{dynamicGraphMain}}.
        If \code{oriented} is given then this value is used,
        else the value extracted from \code{list(...)$Arguments} is used. }
  \item{blockEdgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockEdgeList} ... (as for \code{edgeList}). }
  \item{factorVertexList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorVertexList} ... (as for \code{edgeList}). }
  \item{factorEdgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorEdgeList} ... (as for \code{edgeList}). }
  \item{extraEdgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraEdgeList} ... (as for \code{edgeList}). }
  \item{visibleVertices}{ As for \code{\link{dynamicGraphMain}}.
        If \code{visibleVertices} ... (as for \code{edgeList}). }
  \item{visibleBlocks}{ As for \code{\link{dynamicGraphMain}}.
        If \code{visibleBlocks} ... (as for \code{edgeList}). }
  \item{extraList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraList} ... (as for \code{edgeList}). }
  \item{object}{ As for \code{\link{dynamicGraphMain}}.
        If \code{object} ... (as for \code{oriented}). }
  \item{viewType}{ As for \code{\link{dynamicGraphMain}}.
        If \code{viewType} ... (as for \code{oriented}). }
  \item{title}{ As for \code{\link{dynamicGraphMain}}.
        If \code{title} ... (as for \code{oriented}). }
  \item{transformation}{ As for \code{\link{dynamicGraphMain}}.
        If \code{transformation} ... (as for \code{oriented}). }
  \item{width}{ As for \code{\link{dynamicGraphMain}}.
        If \code{width} ... (as for \code{oriented}). }
  \item{height}{ As for \code{\link{dynamicGraphMain}}.
        If \code{height} ... (as for \code{oriented}). }
  \item{w}{ As for \code{\link{dynamicGraphMain}}.
        If \code{w} ... (as for \code{oriented}). }
  \item{vertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{vertexColor} ... (as for \code{oriented}). }
  \item{extraVertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraVertexColor} ... (as for \code{oriented}). }
  \item{edgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{edgeColor} ... (as for \code{oriented}). }
  \item{factorVertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorVertexColor} ... (as for \code{oriented}). }
  \item{factorEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorEdgeColor} ... (as for \code{oriented}). }
  \item{blockEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockEdgeColor} ... (as for \code{oriented}). }
  \item{blockColors}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockColors} ... (as for \code{oriented}). }
  \item{extraEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraEdgeColor} ... (as for \code{oriented}). }
  \item{background}{ As for \code{\link{dynamicGraphMain}}.
        If \code{background} ... (as for \code{oriented}). }
  \item{initialWindow}{ Logical, if \code{initialWindow} is TRUE
			the the labels of the edges are updated. }
  \item{returnLink}{ As for \code{\link{dynamicGraphMain}}.
        If \code{returnLink} ... (as for \code{oriented}). }
  \item{returnNull}{ As for \code{\link{dynamicGraphMain}}.
        If \code{returnNull} ... (as for \code{oriented}). }
  \item{setUpdateCountModelMain}{ Logical. If
                                  \code{setUpdateCountModelMain} is TRUE then
                                  views of the same model will be updated. }
  \item{\dots}{ Used to porting \code{list(...)$Arguments}. }
}

\details{
  The \code{drawModel} and \code{redrawView} functions can be called
  from the functions of menus (main menu and pop up menus) of 
  \code{\link{dynamicGraphMain}}, from .GlobalEnv in 
  \code{\link{DynamicGraph}}) via returned values from
  \code{\link{dynamicGraphMain}} (and from the methods of the model
  object in the scope of the function \code{\link{dynamicGraphMain}}).
  As a result the graph window will be redrawn with an other view of the model,
  possible with, e.g., other edges, an other model is drawn,
  or a new slave graph window will appear.

  If the value of a argument to  \code{drawModel} or \code{redrawView} is set,
  then this value is used, else the value from the calling window is used.
  The value of the calling window is given in the argument
  \code{Arguments} in the call of the function of the menu item.

  Below is an example, where items for labeling all the edges
  of the graph are added to the menu.
  The edges are visited, a test is computed for each edge,
  the label and width of the edge is updated, and the
  graph is drawn with the updated edge list.
}

\value{
  The returned value from \code{\link{dynamicGraphMain}}.
}


\author{Jens Henrik Badsberg}

\note{ The functions can not be called from top level, that is,
       the functions does not exists at .GlobalEnv, but only
       in returned values from \code{\link{dynamicGraphMain}}.

       It is recommended that the functions not are called, but that
       \code{\link{DynamicGraph}} is used with the arguments
       \code{frameModels}, \code{frameViews}, \code{graphWindow},
       \code{addModel}, \code{addView}, and/or \code{overwrite}
       to call the functions.
 }

\seealso{ See also \code{\link{dynamicGraphMain}},
          \code{\link{DynamicGraph}}
	  \code{\link{DynamicGraph-class}}, and
	  \code{\link{DynamicGraphModel-class}}.  }

\examples{
# The use of "drawModel"  and "redrawView" by 
# "DynamicGraph" in the example "usermenus" of demo:

your.DrawModel <- function(object, slave = FALSE, viewType = "Simple", ...) {
    args <- list(...)
    Args <- args$Arguments

    # Here you should make your new model (this is just a copy):

    Object <- object
    title <- Object@name

    # and compute edges (here 'NULL' if the model not has been updated):

    Edges <- graphComponents(Object, viewType, Arguments = Args)
    EdgeList       <- Edges$vertexEdges
    ExtraVertices  <- Edges$extraVertices
    FactorVertices <- Edges$factorVertices
    FactorEdges    <- Edges$factorEdges
    BlockEdges     <- Edges$blockEdges
    ExtraEdges     <- Edges$extraEdges
    visualVertices <- Edges$visualVertices
    visualBlocks   <- Edges$visualBlocks

    if (slave) {
      # Drawing ''an other model'' in a new window:
      DynamicGraph(addModel = TRUE,                   # <-
                   frameModels = Args$frameModels, 
                   frameViews = NULL,                 # <- not used here
                   graphWindow = NULL,                # <- not used here
                   edgeList = EdgeList,
                   object = Object, 
                   extraList = ExtraVertices, 
                   extraEdgeList = ExtraEdges, 
                   factorVertexList = FactorVertices, 
                   factorEdgeList = FactorEdges, 
                   blockEdgeList = BlockEdges, 
                   visualVertices = visualVertices,
                   visualBlocks = visualBlocks,
                   title = title, 
                   Arguments = Args)
    } else {
      # Overwriting with ''an other model'' in same view:
      DynamicGraph(overwrite = TRUE,                 # <-
                   addModel = TRUE,                  # <-
                   frameModels = Args$frameModels, 
                   frameViews = Args$frameViews, 
                   graphWindow = Args$graphWindow,   # <-
                   edgeList = EdgeList, 
                   object = Object, 
                   extraList = ExtraVertices, 
                   extraEdgeList = ExtraEdges, 
                   factorVertexList = FactorVertices, 
                   factorEdgeList = FactorEdges, 
                   blockEdgeList = BlockEdges, 
                   visualVertices = visualVertices,
                   visualBlocks = visualBlocks,
                   title = "Not used!", 
                   width = NULL, height = NULL, 
                   Arguments = Args) }
}

your.LabelAllEdges <- function(object, slave = FALSE, ...) 
 {
  args <- list(...)
  Args <- args$Arguments

  getNodeName <- function(index, type)
    if (type == "Vertex")
      name(Args$vertexList[[index]])
    else if (type == "Factor")
      name(Args$factorVertexList[[abs(index)]])
    else if (type == "Extra")
      name(Args$extraList[[abs(index)]])
    else if (type == "Block")
      label(Args$blockList[[abs(index)]])
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
                    edge.index = i, force = force, Arguments = Args)

      if (!is.null(R)) {
        if (TRUE || (hasMethod("label", class(R))))
          label(edges[[i]]) <- label(R)
        if (TRUE || (hasMethod("width", class(R))))
          width(edges[[i]]) <- width(R)
      }
    }
    return(edges)
  }

  edgeList <- visitEdges(Args$edgeList)
  factorEdgeList <- visitEdges(Args$factorEdgeList)
  blockEdgeList <- visitEdges(Args$blockEdgeList)

  if (slave) {
    # Adding an other view of the same model:
    DynamicGraph(addView = TRUE,                  # <-
                 frameModels = Args$frameModels, 
                 frameViews = Args$frameViews, 
                 graphWindow = NULL,              # <- not used here
                 edgeList = edgeList, 
                 factorEdgeList = factorEdgeList, 
                 blockEdgeList = blockEdgeList, 
                 title = "A slave window", 
                 Arguments = Args)
  } else {
    # Overwriting with an other view of the same model:
    DynamicGraph(overwrite = TRUE,                # <-
                 addView = TRUE,                  # <-
                 frameModels = Args$frameModels, 
                 frameViews = Args$frameViews, 
                 graphWindow = Args$graphWindow,  # <-
                 edgeList = edgeList, 
                 factorEdgeList = factorEdgeList, 
                 blockEdgeList = blockEdgeList, 
                 title = "Not used!", 
                 width = NULL, height = NULL, 
                 Arguments = Args) } 
 }

 palle <- function(...) print("Palle")
 palle <- function(...) print(list(...)$Arguments$object@name)

 Menus <- 
 list(MainUser = 
      list(label = "Transformation by 'prcomp' on position of \"vertices\", and redraw",
           command = function(object, ...) {
             Args <- list(...)$Arguments
             transformation <- t(prcomp(Positions(Args$vertexList))$rotation)
             Args$redrawView(graphWindow = Args$graphWindow,
                             transformation = transformation, Arguments = Args)
             }),
      MainUser = 
      list(label = "Position of \"vertices\" by 'cmdscale', and redraw",
           command = function(object, ...) {
             Args <- list(...)$Arguments
             Vertices <- Args$vertexList
             Edges <- Args$edgeList
             positions <- Positions(Args$vertexList)
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
             Positions(Args$vertexList) <<- mdsX$points / M * 45
             Args$redrawView(graphWindow = Args$graphWindow, 
                             # Positions = Positions(Args$vertexList), 
                             vertexList = Args$vertexList, Arguments = Args)
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
           command = function(object, ...) {
             Args <- list(...)$Arguments
             ReturnVal <- modalDialog("Test modalDialog Entry",
                                      "Enter name", Args$title,
                                      top = Args$top)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return()
             tktitle(Args$top) <- ReturnVal } ),
      MainUser = 
      list(label = "Call of function 'palle', result on 'viewLabel' at bottom",
           command = function(object, ...) {
             Args <- list(...)$Arguments
             tkconfigure(Args$viewLabel, 
                         text = paste(Args$viewType, " | ", palle(...))) } ),
      Vertex = 
      list(label = "Test of user popup menu for vertices: Label",
           command = function(object, name, ...) {
             # print(name)
             args <- list(...)
             # print(names(args))
             # print(c(args$type))
             # print(c(args$index))
             Args <- args$Arguments
             print(Args$vertexList[[args$index]]@label) } ),
      Edge = 
      list(label = "Test of user popup menu for edges: Class",
           command = function(object, name1, name2, ...) {
             args <- list(...)
             # print(c(name1, name2))
             # print(c(args$edge.index, args$which.edge, args$from, args$to))
             # print(c(args$from.type, args$to.type, args$edge.type))
             Args <- list(...)$Arguments
             ReturnVal <- selectDialog("Test selectDialog Entry",
                                       "Select name", Args$edgeClasses[,1],
                                       top = Args$top)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return()
             if ((args$from > 0) && (args$to > 0)) {
               edgeList <- Args$edgeList
               class(edgeList[[args$edge.index]]) <- 
                                              Args$edgeClasses[ReturnVal, 2]
               # vertexEdges(Args$object) <<- edgeList # Not working !!!
               Args$redrawView(graphWindow = Args$graphWindow,
                               edgeList = edgeList, title = "Not used!", 
                               width = NULL, height = NULL, Arguments = Args)
             } } ),
      ClosedBlock = 
      list(label = "Test of user popup menu for blocks",
           command = function(object, name, ...) {
             print(name)
             print(c(list(...)$index)) } )
     )
}

\keyword{ methods }
