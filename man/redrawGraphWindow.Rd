\name{redrawGraphWindow}
\alias{redrawGraphWindow}
\alias{menu}
\title{Redraw the dynamicGraph window and draw slaves}
\description{
    To redraw the graph window and to draw slave graph windows.
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
% }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphLattice}{ NULL, or \code{graphLattice} of 
      \code{list(...)$Arguments} }
  \item{graphWindow}{ If \code{graphWindow} is set to 
      the value of \code{list(...)$Arguments$graphWindow} then
      the calling graph window will be redrawn.
      If \code{graphWindow} is set to \code{NULL}, the default value,
      then a new slave graph window will be drawn. }
  \item{edgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{edgeList} is given then this value is used,
        else the value extracted from \code{list(...)$Arguments} is used. }
  \item{blockEdgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockEdgeList} ... (as for \code{edgeList}). }
  \item{factorVertexList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorVertexList} ... (as for \code{edgeList}). }
  \item{factorEdgeList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorEdgeList} ... (as for \code{edgeList}). }
  \item{visibleVertices}{ As for \code{\link{dynamicGraphMain}}.
        If \code{visibleVertices} ... (as for \code{edgeList}). }
  \item{extraList}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraList} ... (as for \code{edgeList}). }
  \item{object}{ As for \code{\link{dynamicGraphMain}}.
        If \code{object} ... (as for \code{edgeList}). }
  \item{title}{ As for \code{\link{dynamicGraphMain}}.
        If \code{title} ... (as for \code{edgeList}). }
  \item{transformation}{ As for \code{\link{dynamicGraphMain}}.
        If \code{transformation} ... (as for \code{edgeList}). }
  \item{width}{ As for \code{\link{dynamicGraphMain}}.
        If \code{width} ... (as for \code{edgeList}). }
  \item{height}{ As for \code{\link{dynamicGraphMain}}.
        If \code{height} ... (as for \code{edgeList}). }
  \item{w}{ As for \code{\link{dynamicGraphMain}}.
        If \code{w} ... (as for \code{edgeList}). }
  \item{vertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{vertexColor} ... (as for \code{edgeList}). }
  \item{extraVertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{extraVertexColor} ... (as for \code{edgeList}). }
  \item{edgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{edgeColor} ... (as for \code{edgeList}). }
  \item{factorVertexColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorVertexColor} ... (as for \code{edgeList}). }
  \item{factorEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{factorEdgeColor} ... (as for \code{edgeList}). }
  \item{blockEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockEdgeColor} ... (as for \code{edgeList}). }
  \item{blockColors}{ As for \code{\link{dynamicGraphMain}}.
        If \code{blockColors} ... (as for \code{edgeList}). }
  \item{background}{ As for \code{\link{dynamicGraphMain}}.
        If \code{background} ... (as for \code{edgeList}). }
  \item{\dots}{ Used to porting \code{list(...)$Arguments}. }
}
\details{
  This function can be called from the functions of menus 
  (main menu and pop up menus) of \code{\link{dynamicGraphMain}}
  (and from the methods of the model object in the scope of
  the function \code{\link{dynamicGraphMain}}).
  As a result the graph window will be redrawn,
  possible with, e.g., other edges,
  or a new slave graph window will appear.

  If the value of a argument to \code{redrawGraphWindow} is set,
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
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
    The function can not be called from top level. }
\seealso{ See also \code{\link{dynamicGraphMain}}. }
\examples{

myLabelAllEdges <- function(object, slave = FALSE, ...) 
 {
  args <- list(...)
  Args <- args$Arguments

  getNodeName <- function(index, type)
    if (type == "Vertex")
      name(Args$vertexList[[index]])
    else if (type == "Factor")
      name(Args$factorVertexList[[abs(index)]])
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

  if (slave)
    Args$redrawGraphWindow(graphWindow = NULL, 
                           edgeList = edgeList,
                           factorEdgeList = factorEdgeList,
                           blockEdgeList = blockEdgeList,
                           title = "A slave window", ...)
  else
    Args$redrawGraphWindow(graphWindow = Args$graphWindow,
                           edgeList = edgeList,
                           factorEdgeList = factorEdgeList,
                           blockEdgeList = blockEdgeList,
                           title = "Not used!", 
                           width = NULL, height = NULL, 
                           Arguments = Args)
 }


Menus <- 
 list(MainUser = 
      list(label = "Test of user drag down menu - Position of \"vertices\"",
           command = function(object, ...) 
             print(Positions(list(...)$Arguments$vertexList))),
      MainUser = 
      list(label = "Label all edges, in this window",
           command = function(object, ...) 
                       myLabelAllEdges(object, slave = FALSE, ...)),
      MainUser = 
      list(label = "Label all edges, in slave window",
           command = function(object, ...) 
                       myLabelAllEdges(object, slave = TRUE, ...)),
      MainUser = 
      list(label = "Test of user drag down menu - modalDialog",
           command = function(object, ...) {
             Args <- list(...)$Arguments
             ReturnVal <- modalDialog("Test modalDialog Entry",
                                      "Enter name", Args$title,
                                      # top = Args$graphWindow@top,
                                      graphWindow = Args$graphWindow)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return() } ),
      Vertex = 
      list(label = "Test of user popup menu for vertices",
           command = function(object, name, ...) {
             print(name)
             print(c(list(...)$index)) } ),
      Edge = 
      list(label = "Test of user popup menu for edges",
           command = function(object, name1, name2, ...) {
             args <- list(...)
             print(c(name1, name2))
             print(c(args$edge.index, args$from, args$to)) } ),
      ClosedBlock = 
      list(label = "Test of user popup menu for blocks",
           command = function(object, name, ...) {
             print(name)
             print(c(list(...)$index)) } )
     )
}
\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
