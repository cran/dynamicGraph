"dynamicGraphMain" <-
function (vertexList, visibleVertices = 1:length(vertexList), 
    visibleBlocks = 1:length(blockList), edgeList = NULL, oriented = FALSE, 
    blockList = NULL, blockEdgeList = NULL, blockTree = NULL, 
    factorVertexList = NULL, factorEdgeList = NULL, extraList = NULL, 
    extraEdgeList = NULL, object = NULL, objectName = NULL, vertexClasses = validVertexClasses(), 
    factorClasses = validFactorClasses(), edgeClasses = validEdgeClasses(), 
    viewClasses = validViewClasses(), viewType = "Simple", title = "dynamicGraph", 
    transformation = NULL, width = 400, height = 400, w = 6, 
    vertexColor = "blue", extraVertexColor = "black", edgeColor = "blue", 
    factorVertexColor = "green", factorEdgeColor = "green", blockEdgeColor = "default", 
    blockColors = NULL, extraEdgeColor = "peru", background = "white", 
    closeenough = 2, setVertexLabels = FALSE, drawBlockFrame = TRUE, 
    drawBlockBackground = FALSE, UserMenus = NULL, hasMethods = TRUE, 
    enterLeaveUpdate = TRUE, updateAllViews = TRUE, namesOnEdges = TRUE, 
    updateEdgeLabels = TRUE, useNamesForLabels = TRUE, debug.strata = FALSE, 
    debug.edges = FALSE, debug.position = FALSE, debug.update = FALSE, 
    permitZoom = TRUE, margin = 100, returnLink = FALSE, returnNull = FALSE, 
    ...) 
{
    permit.update.block.index <- TRUE
    font.vertex.label <- "8x16"
    font.edge.label <- "8x16"
    font.block <- "10x20"
    min.x <- -margin
    max.x <- width + margin
    d.x <- -min.x/(max.x - min.x)
    min.y <- -margin
    max.y <- height + margin
    d.y <- -min.y/(max.y - min.y)
    colors <- c("DarkGreen", "navy", "NavyBlue", "DarkBlue", 
        "DarkRed", "MidnightBlue", "DarkSlateGray", "DarkSlateGrey", 
        "MediumBlue", "ForestGreen", "SaddleBrown", "DarkOliveGreen", 
        "firebrick", "brown", "blue", "green", "red", "DarkSlateBlue", 
        "SeaGreen", "DarkCyan", "DarkMagenta", "OliveDrab", "sienna", 
        "LimeGreen", "DimGray", "DimGrey", "maroon", "OrangeRed", 
        "DarkGoldenrod", "chocolate", "MediumSeaGreen", "DarkViolet", 
        "LawnGreen", "LightSeaGreen", "SteelBlue", "chartreuse", 
        "SpringGreen", "black", "SlateGray", "SlateGrey", "VioletRed", 
        "IndianRed", "DarkOrange", "RoyalBlue", "peru", "SlateBlue", 
        "BlueViolet", "DarkOrchid", "LightSlateGray", "LightSlateGrey", 
        "YellowGreen", "CadetBlue", "DarkTurquoise", "goldenrod", 
        "orange", "DeepPink", "tomato", "DodgerBlue", "purple", 
        "DeepSkyBlue", "coral", "gold", "DarkSeaGreen", "RosyBrown", 
        "GreenYellow", "MediumPurple", "PaleVioletRed", "DarkKhaki", 
        "MediumOrchid", "CornflowerBlue", "salmon", "LightCoral", 
        "turquoise", "LightSlateBlue", "SandyBrown", "DarkSalmon", 
        "DarkGray", "DarkGrey", "cyan", "magenta", "yellow", 
        "LightGreen", "tan", "LightSalmon", "HotPink", "burlywood", 
        "orchid", "PaleGreen", "gray", "grey", "SkyBlue", "LightGoldenrod", 
        "LightSkyBlue", "aquamarine", "LightSteelBlue", "plum", 
        "violet", "khaki", "LightBlue", "thistle", "LightPink", 
        "PowderBlue", "LightGray", "LightGrey", "PaleGoldenrod", 
        "wheat", "NavajoWhite", "pink", "PaleTurquoise", "PeachPuff", 
        "gainsboro", "moccasin", "bisque", "BlanchedAlmond", 
        "AntiqueWhite", "PapayaWhip", "MistyRose", "beige", "lavender", 
        "LemonChiffon", "linen", "cornsilk", "OldLace", "LightCyan", 
        "LightYellow", "honeydew", "WhiteSmoke", "seashell", 
        "LavenderBlush", "AliceBlue", "FloralWhite", "azure", 
        "ivory", "MintCream", "GhostWhite", "snow", "white")
    "myColor" <- function(i) colors[min(i%%137, length(colors))]
    "drawModel" <- function(frameModels = NULL, frameViews = NULL, 
        graphWindow = NULL, edgeList = NULL, oriented = FALSE, 
        blockEdgeList = NULL, factorVertexList = NULL, factorEdgeList = NULL, 
        visibleVertices = NULL, visibleBlocks = NULL, extraList = NULL, 
        extraEdgeList = NULL, object = NULL, viewType = NULL, 
        title = "dynamicGraph", transformation = NULL, width = NULL, 
        height = NULL, w = NULL, vertexColor = NULL, extraVertexColor = NULL, 
        edgeColor = NULL, factorVertexColor = NULL, factorEdgeColor = NULL, 
        blockEdgeColor = NULL, blockColors = NULL, extraEdgeColor = NULL, 
        background = NULL, initialWindow = FALSE, returnNewMaster = FALSE, 
        redraw = FALSE, returnLink = FALSE, returnNull = FALSE, 
        returnFrameModel = TRUE, ...) {
        "redrawView" <- function(frameModels = NULL, frameViews = NULL, 
            graphWindow = NULL, edgeList = NULL, oriented = NULL, 
            blockEdgeList = NULL, factorVertexList = NULL, factorEdgeList = NULL, 
            visibleVertices = NULL, visibleBlocks = NULL, extraList = NULL, 
            extraEdgeList = NULL, graphComponents = NULL, viewType = NULL, 
            title = "dynamicGraph", transformation = NULL, width = NULL, 
            height = NULL, w = NULL, vertexColor = NULL, extraVertexColor = NULL, 
            edgeColor = NULL, factorVertexColor = NULL, factorEdgeColor = NULL, 
            blockEdgeColor = NULL, blockColors = NULL, extraEdgeColor = NULL, 
            background = NULL, initialWindow = FALSE, returnNewMaster = FALSE, 
            redraw = FALSE, returnLink = NULL, returnNull = FALSE, 
            returnFrameModel = TRUE, setUpdateCountModelMain = FALSE, 
            ...) {
            "setSrcLabel" <- function(viewLabel) {
                g <- function(x, i, max) {
                  if (!is.null(zoomCenter)) 
                    x <- (x - c(zoomCenter[i]))/Scale + zoomCenter[i]
                  x <- c(x[1], mean(x), x[2])
                  x <- c(x, 100/max * x - 50)
                  return(round(x))
                }
                x <- (min.x + (max.x - min.x) * as.numeric(tkxview(canvas)))
                x <- g(x, 1, width)
                x.txt <- paste(format(x, digits = 3, trim = FALSE), 
                  collapse = ", ")
                y <- (min.y + (max.y - min.y) * as.numeric(tkyview(canvas)))
                y <- g(y, 2, height)
                y.txt <- paste(format(y, digits = 3, trim = FALSE), 
                  collapse = ", ")
                ViewType <- viewClasses[viewClasses[, 2] == class(GraphWindow), 
                  1]
                tkconfigure(viewLabel, text = paste(ViewType, 
                  " | ", "X:", x.txt, "Y:", y.txt))
            }
            getLabel <- function() {
                title <- paste("Model: ", Frame.Views@index, 
                  sep = " ")
                if (exists("GraphWindow")) 
                  title <- paste(title, "; Graph: ", GraphWindow@index, 
                    GraphWindow@id, sep = " ")
                return(title)
            }
            "newGraph" <- function(viewType, visibleVertices, 
                visibleBlocks, extra.vertices, graph.edges, block.edges, 
                factor.vertices, factor.edges, extra.edges, title = "Graph diddler", 
                index = -1, id = 0, close.enough = closeenough, 
                background = "white", width = 400, height = 400) {
                prototype <- "DynamicGraphView"
                x <- match(viewType, viewClasses[, 1])
                if (is.null(x) || all(is.na(x))) {
                  x <- match(viewType, viewClasses[, 2])
                  viewType <- paste(viewClasses[, 1][x])
                }
                if (!is.null(x) && !all(is.na(x))) 
                  prototype <- paste(viewClasses[, 2][x])
                top <- tktoplevel()
                if (permitZoom) {
                  f <- tkframe(top)
                  tkpack(f, expand = "yes", side = "top", fill = "both")
                  xscr <- tkscrollbar(f, repeatinterval = 5, 
                    orient = "horizontal", background = "white", 
                    command = function(...) {
                      tkxview(canvas, ...)
                      setSrcLabel(viewLabel)
                    })
                  yscr <- tkscrollbar(f, repeatinterval = 5, 
                    orient = "vertical", command = function(...) {
                      tkyview(canvas, ...)
                      setSrcLabel(viewLabel)
                    })
                  viewLabel <- tklabel(f, text = viewType, foreground = "DarkSlateBlue", 
                    background = "LightGrey")
                  f1 <- tkframe(f)
                  canvas <- tkcanvas(f1, relief = "raised", background = background, 
                    closeenough = close.enough, borderwidth = 5, 
                    highlightthickness = 2, scrollregion = c(min.x, 
                      min.y, max.x, max.y), xscrollincrement = -4, 
                    yscrollincrement = -4, xscrollcommand = function(...) tkset(xscr, 
                      ...), yscrollcommand = function(...) tkset(yscr, 
                      ...), width = width, height = height)
                  tkpack(canvas, expand = "yes", fill = "both")
                  tkgrid(f1, padx = 1, pady = 1, row = 0, column = 0, 
                    rowspan = 1, columnspan = 1, sticky = "news")
                  tkgrid(yscr, padx = 1, pady = 1, row = 0, column = 1, 
                    rowspan = 1, columnspan = 1, sticky = "news")
                  tkgrid(xscr, padx = 1, pady = 1, row = 1, column = 0, 
                    rowspan = 1, columnspan = 1, sticky = "news")
                  tkgrid(viewLabel, padx = 1, pady = 1, row = 2, 
                    column = 0, rowspan = 1, columnspan = 1, 
                    sticky = "news")
                  tkgrid.columnconfigure(f, 0, weight = 1, minsize = 0)
                  tkgrid.rowconfigure(f, 0, weight = 1, minsize = 0)
                  tkpack(canvas, expand = "yes", fill = "both", 
                    padx = 1, pady = 1)
                  tkfocus(canvas)
                }
                else {
                  canvas <- tkcanvas(top, relief = "raised", 
                    background = background, closeenough = close.enough, 
                    width = width, height = height)
                  tkpack(canvas)
                }
                prototype <- "DynamicGraphView"
                x <- match(viewType, viewClasses[, 1])
                if (is.null(x) || all(is.na(x))) 
                  x <- match(viewType, viewClasses[, 2])
                if (!is.null(x) && !all(is.na(x))) 
                  prototype <- paste(viewClasses[, 2][x])
                GraphWindow.top <<- list(top)
                GraphWindow.canvas <<- list(canvas)
                GraphWindow.viewLabel <<- list(viewLabel)
                GraphWindow.tags <<- list(NULL)
                result <- new(prototype, title = paste(title, 
                  viewType, sep = " / "), index = index, top = list(NULL), 
                  canvas = list(NULL), viewLabel = list(NULL), 
                  tags = list(NULL), id = id, visibleVertices = .nullToEmpty(visibleVertices), 
                  visibleBlocks = .nullToEmpty(visibleBlocks), 
                  extraVertices = .nullToList(extra.vertices, 
                    type = "dg.VertexList"), vertexEdges = graph.edges, 
                  blockEdges = block.edges, factorVertices = .nullToList(factor.vertices, 
                    type = "dg.FactorVertexList"), factorEdges = factor.edges, 
                  extraEdges = extra.edges)
                tktitle(top) <- title
                return(result)
            }
            "Args" <- function(x.drawModel = drawModel, x.redrawView = redrawView, 
                x.frameModels = Frame.Models, x.frameViews = Frame.Views, 
                x.graphWindow = GraphWindow, x.vertexList = vertexList, 
                x.visibleVertices = visibleVertices, x.visibleBlocks = visibleBlocks, 
                x.edgeList = currentEdges(edge.type = "VertexEdge"), 
                x.oriented = oriented, x.blockList = blockList, 
                x.blockEdgeList = currentEdges(edge.type = "BlockEdge"), 
                x.blockTree = blockTree, x.factorVertexList = factorVertexList, 
                x.factorEdgeList = currentEdges(edge.type = "FactorEdge"), 
                x.extraList = extraList, x.extraEdgeList = currentEdges(edge.type = "ExtraEdge"), 
                x.object = object, x.objectName = objectName, 
                x.vertexClasses = vertexClasses, x.factorClasses = factorClasses, 
                x.edgeClasses = edgeClasses, x.viewClasses = viewClasses, 
                x.viewType = viewType, x.top = GraphWindow.top[[1]], 
                x.canvas = GraphWindow.canvas[[1]], x.viewLabel = GraphWindow.viewLabel[[1]], 
                x.tags = GraphWindow.tags, x.title = title, x.transformation = transformation, 
                x.width = width, x.height = height, x.w = w, 
                x.vertexColor = vertexColor, x.extraVertexColor = extraVertexColor, 
                x.edgeColor = edgeColor, x.factorVertexColor = factorVertexColor, 
                x.factorEdgeColor = factorEdgeColor, x.blockEdgeColor = blockEdgeColor, 
                x.blockColors = blockColors, x.extraEdgeColor = extraEdgeColor, 
                x.background = background, x.returnLink = returnLink, 
                x.selectedNodes = selectedNodes, x.selectedEdges = selectedEdges, 
                x.closedBlock = closedBlock, x.hiddenBlock = hiddenBlock) return(list(drawModel = x.drawModel, 
                redrawView = x.redrawView, frameModels = x.frameModels, 
                frameViews = x.frameViews, graphWindow = x.graphWindow, 
                visibleVertices = x.visibleVertices, visibleBlocks = x.visibleBlocks, 
                vertexList = x.vertexList, edgeList = x.edgeList, 
                oriented = x.oriented, blockList = x.blockList, 
                blockEdgeList = x.blockEdgeList, blockTree = x.blockTree, 
                factorVertexList = x.factorVertexList, factorEdgeList = x.factorEdgeList, 
                extraList = x.extraList, extraEdgeList = x.extraEdgeList, 
                object = x.object, objectName = x.objectName, 
                vertexClasses = x.vertexClasses, factorClasses = x.factorClasses, 
                edgeClasses = x.edgeClasses, viewClasses = x.viewClasses, 
                viewType = x.viewType, top = x.top, canvas = x.canvas, 
                viewLabel = x.viewLabel, tags = x.tags, title = x.title, 
                transformation = x.transformation, width = x.width, 
                height = x.height, w = x.w, vertexColor = x.vertexColor, 
                extraVertexColor = x.vertexColor, edgeColor = x.edgeColor, 
                factorVertexColor = x.factorVertexColor, factorEdgeColor = x.factorEdgeColor, 
                blockEdgeColor = x.blockEdgeColor, blockColors = x.blockColors, 
                extraEdgeColor = x.extraEdgeColor, background = x.background, 
                returnLink = x.returnLink, selectedNodes = x.selectedNodes, 
                selectedEdges = x.selectedEdges, closedBlock = x.closedBlock, 
                hiddenBlock = x.hiddenBlock))
            "relativePositionsCanvas" <- function(positions) {
                if (!is.null(zoomPositions)) {
                  diff <- 100/(zoomPositions[, 2] - zoomPositions[, 
                    1])
                  p <- (diag(diff) %*% (.asRow(positions)) - 
                    0)
                }
                else p <- .asRow(positions)
                x <- t(diag(c(width, height, rep(100, N - 2))/100) %*% 
                  (p + 0))
                x <- (x - 0) * Scale + 0
                x <- round(x)
                return(x)
            }
            "inversCanvasRelativePosition" <- function(positions) {
                p <- .asRow(positions)
                p <- (p - 0)/Scale + 0
                if (is.null(dim(p)) && (length(p) < N)) 
                  if (length(p) == 1) 
                    p <- rep(p, N)
                  else p <- rep(0, N)
                p <- t(diag(c(100/width, 100/height, rep(1, N - 
                  2))) %*% p - 0)
                if (!is.null(zoomPositions)) {
                  diff <- (zoomPositions[, 2] - zoomPositions[, 
                    1])/100
                  q <- t(diag(diff) %*% t(p + 0))
                  return(q)
                }
                else return(p)
            }
            "positionsCanvas" <- function(positions) {
                if (!is.null(zoomPositions)) {
                  a <- zoomPositions[, 1]
                  b <- zoomPositions[, 2]
                  A <- matrix(rep(a, ifelse(is.null(dim(positions)), 
                    1, nrow(positions))), byrow = TRUE, ncol = N)
                  diff <- 100/(b - a)
                  p <- (diag(diff) %*% (.asRow(positions) - t(A)) - 
                    50)
                }
                else p <- .asRow(positions)
                x <- t(diag(c(width, height, rep(100, N - 2))/100) %*% 
                  (p + 50))
                if (!is.null(zoomCenter)) 
                  x <- t((t(x) - c(zoomCenter)) * Scale + zoomCenter)
                x <- round(x)
                return(x)
            }
            "inversCanvasPosition" <- function(positions) {
                p <- .asRow(positions)
                if (!is.null(zoomCenter)) 
                  p <- t((t(p) - zoomCenter)/Scale + zoomCenter)
                if (is.null(dim(p)) && (length(p) < N)) 
                  if (length(p) == 1) 
                    p <- rep(p, N)
                  else p <- rep(0, N)
                p <- t(diag(c(100/width, 100/height, rep(1, N - 
                  2))) %*% p - 50)
                if (!is.null(zoomPositions)) {
                  a <- zoomPositions[, 1]
                  b <- zoomPositions[, 2]
                  A <- matrix(rep(a, ifelse(is.null(dim(positions)), 
                    1, nrow(positions))), byrow = TRUE, ncol = N)
                  diff <- (b - a)/100
                  q <- t(diag(diff) %*% t(p + 50) + t(A))
                  return(q)
                }
                else return(p)
            }
            "replaceXY" <- function(x, y, position) {
                position[1] <- as.numeric(x)
                position[2] <- as.numeric(y)
                if (permitZoom) {
                  r.x <- as.numeric(tkxview(GraphWindow.canvas[[1]]))
                  r.y <- as.numeric(tkyview(GraphWindow.canvas[[1]]))
                  if (d.x == 0) 
                    m.x <- 0
                  else m.x <- -(r.x[1] - d.x)/d.x * min.x
                  if (d.y == 0) 
                    m.y <- 0
                  else m.y <- -(r.y[1] - d.y)/d.y * min.y
                  m <- c(m.x, m.y, rep(0, N - 2))
                  position <- position + m
                }
                return(position)
            }
            "callPopup" <- function(i, PopupMenu) {
                force(i)
                function(x, y) {
                  rootx <- as.integer(tkwinfo("rootx", canvas))
                  rooty <- as.integer(tkwinfo("rooty", canvas))
                  xCanvas <- as.integer(x) + rootx
                  yCanvas <- as.integer(y) + rooty
                  .Tcl(paste("tk_popup", .Tcl.args(PopupMenu, 
                    xCanvas, yCanvas)))
                }
            }
            "getTag" <- function(text, number, setTag = TRUE) {
                tag <- paste(text, number, GraphWindow@id, sep = "-")
                if (is.null(GraphWindow.tags[[1]])) 
                  GraphWindow.tags <<- list(tag)
                else if (!any(unlist(lapply(GraphWindow.tags, 
                  function(i) i == tag)))) 
                  GraphWindow.tags <<- append(list(tag), GraphWindow.tags)
                else if (setTag) 
                  message(paste("(( Duplicated tag: ", tag, " ))", 
                    sep = " "))
                if (returnLink) 
                  GraphWindow@tags <<- GraphWindow.tags
                return(tag)
            }
            "deleteTags" <- function(text = "deleteTags: ") {
                for (i in seq(along = GraphWindow.tags)) {
                  tkdelete(GraphWindow.canvas[[1]], GraphWindow.tags[[i]])
                }
                for (i in seq(along = GraphWindow.tags)) {
                  tag <- tkgettags(GraphWindow.canvas[[1]], i)
                  if (length(as.character(tag)) > 0) {
                    if (TRUE || debug.edges) {
                      print(paste(text, i, as.character(tag), 
                        sep = ": "))
                    }
                    tkdelete(GraphWindow.canvas[[1]], i)
                  }
                }
            }
            "destroyView" <- function() {
                function(...) {
                  deleteTags("destroyView")
                  tkdestroy(GraphWindow.top[[1]])
                }
            }
            "verticesUpdate" <- function(update.positions = TRUE, 
                update.label.positions = TRUE) {
                vertexList <<- Frame.Models@vertices
                for (i in seq(along = vertexList)) {
                  position <- positionsVertices[i, ]
                  position(vertexList[[i]]) <<- position
                  position <- positionsLabels[i, ]
                  labelPosition(vertexList[[i]]) <<- position
                  label(vertexList[[i]]) <<- Labels[i]
                  vertexList[[i]]@name <<- namesVertices[i]
                  color(vertexList[[i]]) <<- colorsVertices[i]
                  blockindex(vertexList[[i]]) <<- blocksVertices[i]
                  if (!.IsEmpty(blockList)) 
                    if (blocksVertices[i] == 0) 
                      stratum(vertexList[[i]]) <<- 0
                    else stratum(vertexList[[i]]) <<- strataBlocks[blocksVertices[i]]
                }
                Frame.Models@vertices <<- vertexList
                return(vertexList)
            }
            "edgesUpdate" <- function(update.label.positions = TRUE) {
                edgeList <<- GraphWindow@vertexEdges
                for (i in seq(along = edgeList)) {
                  position <- positionsEdgeLabels[i, ]
                  edgeList[[i]]@label.position <<- position
                }
                GraphWindow@vertexEdges <<- edgeList
                return(edgeList)
            }
            "blockTreeUpdate" <- function(tree) {
                "subBlockTreeUpdate" <- function(tree) {
                  i <- abs(tree$block@index)
                  position <- positionsBlocks[i, , ]
                  position(tree$block) <<- position
                  position <- positionsBlockLabels[i, ]
                  labelPosition(tree$block) <<- position
                  tree$block@stratum <<- strataBlocks[i]
                  tree$block@label <<- blockLabels[i]
                  tree$block@closed <<- closedBlock[i]
                  tree$block@visible <<- !hiddenBlock[i]
                  if (!is.null((tree$sub.blocks))) 
                    for (j in 1:length(tree$sub.blocks)) {
                      tree$sub.blocks[[j]] <<- subBlockTreeUpdate(tree$sub.blocks[[j]])
                    }
                  return(tree)
                }
                if (!.IsEmpty(blockList) && !.IsEmpty(tree) && 
                  !(length(tree) == 0)) 
                  subBlockTreeUpdate(tree)
                return(tree)
            }
            "blocksUpdate" <- function(updateTree = TRUE) {
                if ((length(blockTree) == 0) || (is.null(blockTree))) 
                  blockTree <<- Frame.Models@blockTree
                if (!.IsEmpty(blockList)) 
                  for (i in seq(along = blockList)) {
                    position <- positionsBlocks[i, , ]
                    position(blockList[[i]]) <<- position
                    position <- positionsBlockLabels[i, ]
                    labelPosition(blockList[[i]]) <<- position
                    blockList[[i]]@stratum <<- strataBlocks[i]
                    blockList[[i]]@label <<- blockLabels[i]
                    blockList[[i]]@closed <<- closedBlock[i]
                    blockList[[i]]@visible <<- !hiddenBlock[i]
                  }
                if (updateTree && !is.null(blockTree)) 
                  blockTreeUpdate(blockTree)
                if (is.null(blockList)) {
                  Frame.Models@blocks <<- .emptyDgList("dg.BlockList")
                  return(Frame.Models@blocks)
                }
                else {
                  Frame.Models@blocks <<- blockList
                  return(blockList)
                }
            }
            "blockEdgesUpdate" <- function() {
                blockEdgeList <<- GraphWindow@blockEdges
                return(blockEdgeList)
            }
            "factorVerticesUpdate" <- function() {
                factorVertexList <<- GraphWindow@factorVertices
                for (i in seq(along = factorVertexList)) {
                  position <- positionsFactorVertices[i, ]
                  position(factorVertexList[[i]]) <<- position
                  position <- positionsFactorLabels[i, ]
                  labelPosition(factorVertexList[[i]]) <<- position
                  label(factorVertexList[[i]]) <<- factorLabels[i]
                  factorVertexList[[i]]@name <<- namesFactorVertices[i]
                  color(factorVertexList[[i]]) <<- colorsFactorVertices[i]
                  if (!.IsEmpty(blockList)) {
                    blockindex(factorVertexList[[i]]) <<- blocksFactorVertices[i]
                    stratum(factorVertexList[[i]]) <<- strataBlocks[blocksFactorVertices[i]]
                  }
                }
                GraphWindow@factorVertices <<- factorVertexList
                return(factorVertexList)
            }
            "factorEdgesUpdate" <- function() {
                factorEdgeList <<- GraphWindow@factorEdges
                return(factorEdgeList)
            }
            "extraEdgesUpdate" <- function() {
                extraEdgeList <<- GraphWindow@extraEdges
                return(extraEdgeList)
            }
            "extraVerticesUpdate" <- function() {
                extraList <<- GraphWindow@extraVertices
                for (i in seq(along = extraList)) {
                  position <- positionsExtraVertices[i, ]
                  position(extraList[[i]]) <<- position
                  position <- positionsExtraLabels[i, ]
                  labelPosition(extraList[[i]]) <<- position
                  label(extraList[[i]]) <<- extraLabels[i]
                  extraList[[i]]@name <<- namesExtraVertices[i]
                  color(extraList[[i]]) <<- colorsExtraVertices[i]
                  if (!.IsEmpty(blockList)) {
                    blockindex(extraList[[i]]) <<- blocksExtraVertices[i]
                    stratum(extraList[[i]]) <<- strataBlocks[blocksExtraVertices[i]]
                  }
                }
                GraphWindow@extraVertices <<- extraList
                return(extraList)
            }
            "updateArguments" <- function(menuItem, vertices = TRUE, 
                edges = TRUE, blocks = FALSE) {
                if (vertices && (is.null(menuItem$update.vertices) || 
                  menuItem$update.vertices)) {
                  V <- verticesUpdate()
                }
                if (edges && (is.null(menuItem$update.edges) || 
                  menuItem$update.edges)) {
                  E <- edgesUpdate()
                  V <- factorVerticesUpdate()
                  E <- factorEdgesUpdate()
                  V <- extraVerticesUpdate()
                  E <- extraEdgesUpdate()
                }
                if (blocks && (is.null(menuItem$update.blocks) || 
                  menuItem$update.blocks)) 
                  B <- blocksUpdate()
                E <- blockEdgesUpdate()
                if (!is.null(blockTree)) 
                  BT <- blockTreeUpdate(blockTree)
                m <- Frame.Views@index
                n <- GraphWindow@index
                Frame.Views@graphs[[n]] <<- GraphWindow
                Frame.Models@models[[m]]@graphs[[n]] <<- GraphWindow
            }
            "update" <- function(type = "Arguments", ...) {
                if (type == "Arguments") 
                  updateArguments(...)
                else subUpdatePositions(...)
            }
            "subUpdateAllFrames" <- function(type = "Arguments", 
                ...) {
                V <- verticesUpdate()
                for (m in 1:length(Frame.Models@models)) {
                  title <- paste("Model:", m, sep = " ")
                  frame.view <- Frame.Models@models[[m]]
                  Frame.Models@models[[m]]@model <<- list(object)
                  for (n in 1:length(frame.view@graphs)) {
                    graph.window <- frame.view@graphs[[n]]
                    if (is.null(graph.window)) {
                      message(paste("Empty window: ", title, 
                        "; Graph:", n, sep = " "))
                    }
                    else {
                      if (is.null(formals(graph.window@update))) {
                        if (debug.update) 
                          message(paste("(( No update function: ", 
                            title, "; Graph:", n, graph.window@title, 
                            " ))", sep = " "))
                      }
                      else graph.window@update(type, ...)
                    }
                  }
                }
            }
            "UpdateAllFrames" <- function(type = "Arguments", 
                txt) {
                force(type)
                force(txt)
                function(...) {
                  subUpdateAllFrames(type, txt)
                }
            }
            "objectAssign" <- function(R) {
                if (!is.null(R) && !is.null(R$object)) {
                  if (!is.null(objectName)) 
                    assign(objectName, object, pos = 1)
                }
            }
            "setModel" <- function(R.object, R = NULL, txt = "", 
                graphWindow = NULL, edgeList = copyCurrentEdges(edge.type = "VertexEdge", 
                  copyProperties = copyProperties), blockEdgeList = copyCurrentEdges(edge.type = "BlockEdge", 
                  copyProperties = copyProperties), factorVertexList = GraphWindow@factorVertices, 
                factorEdgeList = copyCurrentEdges(edge.type = "FactorEdge", 
                  copyProperties = copyProperties), extraEdgeList = copyCurrentEdges(edge.type = "ExtraEdge", 
                  copyProperties = copyProperties), visible.Vertices = GraphWindow@visibleVertices, 
                visible.Blocks = GraphWindow@visibleBlocks, extraList = GraphWindow@extraVertices, 
                copyProperties = FALSE, setUpdate = TRUE) {
                if (hasMethod("setGraphComponents", class(R.object))) {
                  if (is.null(edgeList)) 
                    edgeList <- .emptyDgList("dg.VertexEdgeList")
                  object <<- setGraphComponents(R.object, viewType = viewType, 
                    visibleVertices = .nullToEmpty(visible.Vertices), 
                    visibleBlocks = .nullToEmpty(visible.Blocks), 
                    extraVertices = .nullToList(extraList, type = "dg.VertexList"), 
                    vertexEdges = edgeList, blockEdges = blockEdgeList, 
                    factorVertices = .nullToList(factorVertexList, 
                      type = "dg.FactorVertexList"), factorEdges = factorEdgeList, 
                    extraEdges = .nullToList(extraEdgeList, type = "dg.ExtraEdgeList"), 
                    ...)
                }
                if (setUpdate) {
                  updateCountModelMain <<- updateCountModelMain + 
                    1
                  updateCountModel <<- updateCountModelMain
                }
            }
            "updateModel" <- function() {
                if (hasMethod("returnGraphComponents", class(object)) || 
                  hasMethod("graphComponents", class(object))) {
                  tkconfigure(canvas, cursor = "watch")
                  tkconfigure(GraphWindow.viewLabel[[1]], text = paste(viewType, 
                    " | Working !!!"))
                  tkfocus(GraphWindow.top[[1]])
                  Arguments <- Args()
                  if (hasMethod("graphComponents", class(object))) 
                    graphComponents <- graphComponents(object, 
                      viewType = viewType, Arguments = Arguments)
                  else graphComponents <- returnGraphComponents(object, 
                    viewType = viewType, Arguments = Arguments)
                  if (is.list(graphComponents)) {
                    names <- names(graphComponents)
                    checkClass <- function(name, class, z = "$", 
                      a = "graphComponents", b = paste(a, z, 
                        name, sep = "")) {
                      text <- paste(c("if ((\"", name, "\" %in% names) ", 
                        "&& (class(", b, ") != \"", class, "\")) ", 
                        "{ message(paste(\"Invalid class of '", 
                        name, "' in list from 'graphComponents'; \")); ", 
                        b, " <<- new(\"", class, "\", .nullToList(", 
                        b, ")) }"), collapse = "")
                      eval(parse(text = text))
                    }
                    checkClass("extraVertices", "dg.VertexList")
                    checkClass("vertexEdges", "dg.VertexEdgeList")
                    checkClass("graphEdges", "dg.VertexEdgeList")
                    checkClass("blockEdges", "dg.BlockEdgeList")
                    checkClass("factorVertices", "dg.FactorVertexList")
                    checkClass("factorEdges", "dg.FactorEdgeList")
                    checkClass("extraEdges", "dg.ExtraEdgeList")
                  }
                  redrawView(frameModels = Frame.Models, frameViews = Frame.Views, 
                    graphWindow = GraphWindow, viewType = viewType, 
                    graphComponents = graphComponents, Arguments = Arguments)
                  tkconfigure(GraphWindow.viewLabel[[1]], text = viewType)
                  tkconfigure(canvas, cursor = "arrow")
                }
            }
            "testUpdateModel" <- function() {
                if ((updateCountModel < updateCountModelMain)) {
                  updateModel()
                  updateCountModel <<- updateCountModelMain
                }
            }
            "extractEdgesResult" <- function(R, newEdges, from.R.edgeList = TRUE, 
                title) {
                if (!from.R.edgeList || is.null(R$edgeList)) 
                  if (is.null(R$newEdges$vertexEdges)) 
                    Edges <- newEdges$vertexEdges
                  else Edges <- R$newEdges$vertexEdges
                else Edges <- R$edgeList
                return(Edges)
            }
            "getEdges" <- function(edge.type = "VertexEdge") {
                if (edge.type == "VertexEdge") 
                  return(GraphWindow@vertexEdges)
                else if (edge.type == "FactorEdge") 
                  return(GraphWindow@factorEdges)
                else if (edge.type == "ExtraEdge") 
                  return(GraphWindow@extraEdges)
                else if (edge.type == "BlockEdge") 
                  return(GraphWindow@blockEdges)
                else return(NULL)
            }
            "edgesClass" <- function(edge.type = "VertexEdge") if (edge.type == 
                "VertexEdge") 
                return("dg.VertexEdgeList")
            else if (edge.type == "FactorEdge") 
                return("dg.FactorEdgeList")
            else if (edge.type == "ExtraEdge") 
                return("dg.ExtraEdgeList")
            else if (edge.type == "BlockEdge") 
                return("dg.BlockEdgeList")
            else return(NULL)
            "currentEdges" <- function(edge.type = "VertexEdge") {
                E <- getEdges(edge.type = edge.type)
                if (length(E) > 0) {
                  E <- lapply(E, function(egde) if (sum(abs(egde@vertex.indices)) > 
                    0) 
                    egde)
                  E <- .removeNull(E)
                }
                else E <- NULL
                if (!is.null(E)) {
                  class(E) <- edgesClass(edge.type = edge.type)
                }
                return(E)
            }
            "append.index.edge" <- function(e, edge.type = "VertexEdge", 
                edgeClass = NULL) {
                if (edge.type == "VertexEdge") 
                  new.edge <- returnEdgeList(list(e), vertexList, 
                    color = edgeColor, oriented = oriented, types = edgeClass, 
                    N = N, edgeClasses = edgeClasses)
                else if (edge.type == "FactorEdge") 
                  new.edge <- returnFactorEdgeList(list(e), vertexList, 
                    color = factorEdgeColor, factorVertexList)
                else if (edge.type == "ExtraEdge") 
                  new.edge <- returnExtraEdgeList(list(e), vertexList, 
                    color = extraEdgeColor, extraVertexList)
                else if (edge.type == "BlockEdge") 
                  new.edge <- .emptyDgList("dg.BlockEdgeList")
                E <- append(getEdges(edge.type = edge.type), 
                  new.edge)
                class(E) <- edgesClass(edge.type = edge.type)
                if (edge.type == "VertexEdge") 
                  GraphWindow@vertexEdges <<- E
                else if (edge.type == "FactorEdge") 
                  GraphWindow@factorEdges <<- E
                else if (edge.type == "ExtraEdge") 
                  GraphWindow@extraEdges <<- E
                else if (edge.type == "BlockEdge") 
                  GraphWindow@blockEdges <<- E
                return(E)
            }
            "append.edge" <- function(e, edge.type = "VertexEdge") {
                E <- append(getEdges(edge.type = edge.type), 
                  list(e))
                class(E) <- edgesClass(edge.type = edge.type)
                if (edge.type == "VertexEdge") 
                  GraphWindow@vertexEdges <<- E
                else if (edge.type == "FactorEdge") 
                  GraphWindow@factorEdges <<- E
                else if (edge.type == "ExtraEdge") 
                  GraphWindow@extraEdges <<- E
                else if (edge.type == "BlockEdge") 
                  GraphWindow@blockEdges <<- E
                return(E)
            }
            "selectCurrentEdges" <- function(omitEdges = FALSE, 
                edge.type = "VertexEdge") {
                E <- getEdges(edge.type = edge.type)
                if (length(E) > 0) {
                  j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                  if (edge.type == "VertexEdge") 
                    j <- j | non.graph.edge(edge.type = edge.type)
                  E <- sapply(1:length(E), function(x) if (!j[x]) 
                    E[[x]])
                  E <- .removeNull(E)
                }
                else E <- NULL
                return(E)
            }
            "copyCurrentEdges" <- function(omitEdges = FALSE, 
                edge.type = "VertexEdge", copyProperties = FALSE) {
                E <- getEdges(edge.type = edge.type)
                if (length(E) > 0) {
                  j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                  if (edge.type == "VertexEdge") 
                    j <- j | non.graph.edge(edge.type = edge.type)
                  edge.classes <- lapply(1:length(E), function(x) if (!j[x]) 
                    class(E[[x]]))
                  edge.classes <- .removeNull(edge.classes)
                  if (copyProperties) {
                    edge.widths <- lapply(1:length(E), function(x) if (!j[x]) 
                      width(E[[x]]))
                    edge.widths <- .removeNull(edge.widths)
                    edge.colors <- lapply(1:length(E), function(x) if (!j[x]) 
                      color(E[[x]]))
                    edge.colors <- .removeNull(edge.colors)
                    edge.dashs <- lapply(1:length(E), function(x) if (!j[x]) 
                      dash(E[[x]]))
                    edge.dashs <- .removeNull(edge.dashs)
                  }
                  if (edge.type == "BlockEdge") {
                    blockEdges <- lapply(1:length(E), function(x) if (!j[x]) 
                      E[[x]])
                    blockEdges <- .removeNull(blockEdges)
                    class(blockEdges) <- "dg.BlockEdgeList"
                  }
                  else {
                    E <- lapply(1:length(E), function(x) if (!j[x]) 
                      E[[x]]@vertex.indices)
                  }
                  E <- .removeNull(E)
                }
                else {
                  edge.classes <- NULL
                  if (copyProperties) {
                    edge.widths <- NULL
                    edge.colors <- NULL
                    edge.dashs <- NULL
                  }
                  E <- NULL
                  blockEdges <- NULL
                }
                if (edge.type == "VertexEdge") 
                  E <- returnEdgeList(E, vertexList, types = edge.classes, 
                    color = edgeColor, oriented = oriented, N = N, 
                    edgeClasses = edgeClasses)
                else if (edge.type == "FactorEdge") 
                  E <- returnFactorEdgeList(E, vertexList, factorVertexList, 
                    color = factorEdgeColor)
                else if (edge.type == "ExtraEdge") 
                  E <- returnExtraEdgeList(E, vertexList, extraVertexList, 
                    color = extraEdgeColor)
                else if (edge.type == "BlockEdge") 
                  E <- blockEdges
                if (copyProperties && !is.null(E)) {
                  Widths(E) <- unlist(edge.widths)
                  Colors(E) <- unlist(edge.colors)
                  Dashes(E) <- unlist(edge.dashs)
                }
                return(E)
            }
            "appendToCurrentEdges" <- function(omitEdges = FALSE, 
                new.edge = NULL, edge.type = "VertexEdge", edgeClass = NULL) {
                E <- getEdges(edge.type = edge.type)
                if (length(E) > 0) {
                  j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                  if (edge.type == "VertexEdge") 
                    j <- j | non.graph.edge(edge.type = edge.type)
                  E <- lapply(1:length(E), function(x) if (!j[x]) 
                    E[[x]])
                  E <- .removeNull(E)
                  edge.list <- lapply(E, function(e) e@vertex.indices)
                  edge.classes <- lapply(E, function(e) class(e))
                  if (!is.null(new.edge)) {
                    edge.list <- append(edge.list, new.edge)
                    edge.classes <- append(edge.classes, edgeClass)
                  }
                }
                else {
                  edge.classes <- edgeClass
                  edge.list <- new.edge
                }
                if (edge.type == "VertexEdge") 
                  E <- returnEdgeList(edge.list, vertexList, 
                    types = edge.classes, color = edgeColor, 
                    oriented = oriented, N = N, edgeClasses = edgeClasses)
                else if (edge.type == "FactorEdge") 
                  E <- NULL
                else if (edge.type == "ExtraEdge") 
                  E <- NULL
                else if (edge.type == "BlockEdge") 
                  E <- NULL
                return(E)
            }
            "which.unordered.edge" <- function(e, edge.type = "VertexEdge") {
                n <- length(e)
                unlist(lapply(getEdges(edge.type = edge.type), 
                  function(i) length(e[!is.na(match(e, i@vertex.indices))]) == 
                    n))
            }
            "which.edge" <- function(e, edge.type = "VertexEdge") unlist(lapply(getEdges(edge.type = edge.type), 
                function(i) all(i@vertex.indices == e)))
            "vertex.in.edge" <- function(e, edge.type = "VertexEdge") unlist(lapply(getEdges(edge.type = edge.type), 
                function(i) is.element(e, i@vertex.indices)))
            "non.graph.edge" <- function(edge.type = "VertexEdge") unlist(lapply(getEdges(edge.type = edge.type), 
                function(i) any(i@vertex.indices <= 0)))
            "edge.vertices" <- function(i, type.negative = "Factor", 
                edge.type = "VertexEdge") {
                E <- getEdges(edge.type = edge.type)
                edge <- E[[i]]@vertex.indices
                edge.vertices <- vector("list", length(edge))
                for (j in seq(along = edge)) if (edge[j] > 0) 
                  edge.vertices[[j]] <- vertexList[[edge[j]]]
                else if (type.negative == "Factor") 
                  edge.vertices[[j]] <- factorVertexList[[-edge[j]]]
                else if (type.negative == "Extra") 
                  edge.vertices[[j]] <- extraList[[-edge[j]]]
                else if (type.negative == "ClosedBlock") 
                  edge.vertices[[j]] <- blockList[[-edge[j]]]
                return(edge.vertices)
            }
            "edge.negative.type" <- function(edge.type = "VertexEdge") if (edge.type == 
                "VertexEdge") 
                return("Vertex")
            else if (edge.type == "FactorEdge") 
                return("Factor")
            else if (edge.type == "ExtraEdge") 
                return("Extra")
            else if (edge.type == "BlockEdge") 
                return("ClosedBlock")
            "edge.names" <- function(i, type.negative = edge.negative.type(edge.type), 
                edge.type = "VertexEdge") lapply(edge.vertices(i, 
                type.negative = type.negative, edge.type = edge.type), 
                function(v) retVertexName(v@index, vertex.type = ifelse(v@index > 
                  0, "Vertex", type.negative)))
            "edge.positions" <- function(i, type.negative = edge.negative.type(edge.type), 
                edge.type = "VertexEdge") {
                lapply(edge.vertices(i, type.negative = type.negative, 
                  edge.type = edge.type), function(v) retVertexPos(v@index, 
                  ifelse(v@index > 0, "Vertex", type.negative)))
            }
            "edge.strata" <- function(i, type.negative = edge.negative.type(edge.type), 
                edge.type = "VertexEdge") {
                lapply(edge.vertices(i, type.negative = type.negative, 
                  edge.type = edge.type), function(v) retStratum(v@index, 
                  vertex.type = ifelse(v@index > 0, "Vertex", 
                    type.negative)))
            }
            "clearEdge" <- function(i, edge.type = "VertexEdge") if (edge.type == 
                "VertexEdge") 
                GraphWindow@vertexEdges[[i]]@vertex.indices <<- c(0, 
                  0)
            else if (edge.type == "FactorEdge") 
                GraphWindow@factorEdges[[i]]@vertex.indices <<- c(0, 
                  0)
            else if (edge.type == "ExtraEdge") 
                GraphWindow@extraEdges[[i]]@vertex.indices <<- c(0, 
                  0)
            else if (edge.type == "BlockEdge") 
                GraphWindow@blockEdges[[i]]@vertex.indices <<- c(0, 
                  0)
            "from" <- function(i, edge.type = "VertexEdge") getEdges(edge.type = edge.type)@vertex.indices[1]
            "to" <- function(i, edge.type = "VertexEdge") getEdges(edge.type = edge.type)[[i]]@vertex.indices[2]
            "setTransformation" <- function(value = NULL) {
                if (is.null(value) == (!is.null(transformation))) {
                  if (!.IsEmpty(blockList)) 
                    for (i in seq(along = blockList)) if ((closedBlock[i] || 
                      hiddenBlock[i])) {
                    }
                    else deleteBlock(i)
                  transformation <<- value
                  if (!.IsEmpty(blockList)) 
                    for (i in seq(along = blockList)) if ((closedBlock[i] || 
                      hiddenBlock[i])) {
                    }
                    else if (is.element(i, visibleBlocks)) 
                      drawBlock(blockList[[i]], i, setTag = FALSE)
                }
                else transformation <<- value
                subUpdateGraphWindow("setTransformation", all.blockframes = TRUE)
            }
            "angle" <- function(value = NULL) if (!is.null(value)) 
                Angle <<- value
            else return(Angle)
            "project" <- function(position) if (!is.null(transformation)) 
                t(transformation %*% .asRow(position))
            else position
            "inversProject" <- function(position) if (!is.null(transformation)) 
                t(solve(transformation, .asRow(position)))
            else position
            "applyTransformation" <- function(trans, draw.box = FALSE, 
                redraw = TRUE) {
                if (!is.null(transformation)) {
                  transformation <<- transformation %*% trans
                  if (redraw) 
                    subUpdateGraphWindow("applyTransformation", 
                      all.blockframes = TRUE)
                }
            }
            "sphereRand" <- function(n) {
                nx2 <- 2
                while ((nx2 >= 1)) {
                  x <- 2 * runif(n) - 1
                  nx2 <- sum(x^2)
                }
                return(x/sqrt(nx2))
            }
            "makeRotation" <- function(x, y, alpha = 0, use.alpha = FALSE, 
                n = length(x)) {
                if (length(x) != length(y) || !is.null(dim(x)) || 
                  !is.null(dim(y))) 
                  stop("Invalid arguments")
                dnrm2 <- function(x) sqrt(sum(x^2))
                rot <- diag(1, n)
                nx <- dnrm2(x)
                ny <- dnrm2(y)
                if ((nx == 0) || (ny == 0)) 
                  return(rot)
                x <- x * (1/nx)
                y <- y * (1/ny)
                xy <- t(x) %*% y
                c <- ifelse(use.alpha, cos(alpha), xy)
                cc <- 1 - c^2
                s <- ifelse(use.alpha, sin(alpha), ifelse(cc > 
                  0, sqrt(cc), 0))
                cm1 <- c - 1
                y <- y - xy * x
                ny <- dnrm2(y)
                if (ny == 0) 
                  return(rot)
                y <- y * (1/ny)
                a <- x * cm1 + y * s
                b <- -x * s + y * cm1
                rot <- rot + a %*% t(x) + b %*% t(y)
                return(rot)
            }
            "canvasToSphere" <- function(X) {
                rad <- 100
                pos <- inversCanvasRelativePosition(X)
                x <- pos[1]
                y <- pos[2]
                norm.2 <- x^2 + y^2
                rad.2 <- rad^2
                z <- sqrt(max(rad.2 - norm.2, 0))
                if (N > 2) 
                  res <- c(x, y, z, rep(0, N - 3))
                else res <- c(x, y)
                if (norm.2 < rad.2) 
                  return(res)
                else {
                  r <- sqrt(norm.2/rad.2)
                  return(res/r)
                }
            }
            "doHandRotate" <- function() {
                p <- NULL
                function(x, y) {
                  tkconfigure(canvas, cursor = "watch")
                  tkfocus(canvas)
                  X <- replaceXY(x, y, rep(50, N))
                  if (is.null(p)) 
                    p <<- canvasToSphere(X)
                  else {
                    oldp <- p
                    p <<- canvasToSphere(X)
                    applyTransformation(makeRotation(oldp, p), 
                      draw.box = FALSE, redraw = TRUE)
                    tkconfigure(canvas, cursor = "arrow")
                  }
                }
            }
            "rockPlot" <- function(k = 2) {
                function(x, y) {
                  tkconfigure(canvas, cursor = "watch")
                  tkfocus(GraphWindow.top[[1]])
                  print("rockPlot")
                  angle <- 10
                  p1 <- sphereRand(N)
                  p2 <- sphereRand(N)
                  for (i in 1:k) applyTransformation(makeRotation(p1, 
                    p2, alpha = angle, use.alpha = TRUE), draw.box = FALSE, 
                    redraw = TRUE)
                  for (i in 1:(2 * k)) applyTransformation(makeRotation(p1, 
                    p2, alpha = -angle, use.alpha = TRUE), draw.box = FALSE, 
                    redraw = TRUE)
                  for (i in 1:k) applyTransformation(makeRotation(p1, 
                    p2, alpha = angle, use.alpha = TRUE), draw.box = FALSE, 
                    redraw = TRUE)
                  print("Finished rocking!")
                  tkconfigure(canvas, cursor = "arrow")
                }
            }
            "keyRotate" <- function(v = 0, sign = 1) {
                force(v)
                force(sign)
                function(...) {
                  if ((v > 2) || (N > 2)) {
                    v1 <- ifelse(v == 1, 2, 1)
                    v2 <- ifelse(v == 3, 2, 3)
                    if (is.null(transformation)) 
                      X <- diag(N)
                    else X <- transformation
                    angle <- pi/16
                    applyTransformation(makeRotation((X[, v1]), 
                      (X[, v2]), alpha = ifelse(sign == 1, angle, 
                        -angle), use.alpha = TRUE), draw.box = FALSE, 
                      redraw = TRUE)
                  }
                }
            }
            "vertexItem" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (vertex.type == "ClosedBlock") 
                  return(itemsClosedBlocks[[i]])
                else if (vertex.type == "Vertex") 
                  return(itemsVertices[[i]])
                else if (vertex.type == "Factor") 
                  return(itemsFactors[[-i]])
                else if (vertex.type == "Extra") 
                  return(itemsExtras[[abs(i)]])
            }
            "setVertexItem" <- function(i, value, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (vertex.type == "ClosedBlock") 
                  itemsClosedBlocks[[i]] <<- value
                else if (vertex.type == "Vertex") 
                  itemsVertices[[i]] <<- value
                else if (vertex.type == "Factor") 
                  itemsFactors[[abs(i)]] <<- value
                else if (vertex.type == "Extra") 
                  itemsExtras[[i]] <<- value
            }
            "edgeItem" <- function(i, edge.type = "VertexEdge") {
                if (i > 0) 
                  return(itemsEdges[[i]])
                else if (edge.type == "BlockEdge") {
                  if (is.element(abs(i), visibleBlocks)) 
                    return(itemsBlockEdges[[-i]])
                  else return(NULL)
                }
                else if (edge.type == "FactorEdge") 
                  return(itemsFactorEdges[[-i]])
                else return(itemsExtraEdges[[-i]])
            }
            "setEdgeItem" <- function(i, edge.type = "VertexEdge", 
                edges = NULL) {
                if (i > 0) 
                  itemsEdges[[i]] <<- edges
                else if (edge.type == "BlockEdge") {
                  itemsBlockEdges[[-i]] <<- edges
                }
                else if (edge.type == "FactorEdge") 
                  itemsFactorEdges[[-i]] <<- edges
                else itemsExtraEdges[[-i]] <<- edges
            }
            "openBlockItem" <- function(i) return(itemsOpenBlocks[[i]])
            "setOpenBlockItem" <- function(i, blocks) itemsOpenBlocks[[i]] <<- blocks
            "closedBlockItem" <- function(i) {
                return(itemsClosedBlocks[[i]])
            }
            "setCloseVertex" <- function(i, value, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) if (vertex.type == "Vertex") {
                closedVertex[i] <<- value
                if (value) {
                  tkdelete(canvas, vertexItem(i)$tag)
                  updateBlockEdges()
                  updateCountBlockEdges <<- updateCountBlockEdgesMain
                }
                else {
                  vertexColor <- retVertexColor(i, vertex.type)
                  drawVertex(i, w = w, vertexcolor = vertexColor, 
                    vertex.type = "Vertex")
                  setVertexColor(i, color = vertexColor, vertex.type = vertex.type)
                }
            }
            "setClosedBlock" <- function(i, value, update = TRUE) if (i > 
                0) {
                closedBlock[i] <<- value
                if (all(is.na(positionsClosedBlocks[i, ]))) 
                  positionsClosedBlocks[i, ] <<- apply(positionsBlocks[i, 
                    , ], 1, mean)
                if (value) 
                  tkdelete(canvas, openBlockItem(i)$tag)
                else tkdelete(canvas, closedBlockItem(i)$tag)
                if (update) 
                  if ((updateCountBlockEdges < updateCountBlockEdgesMain)) {
                    updateBlockEdges()
                    updateCountBlockEdges <<- updateCountBlockEdgesMain
                  }
            }
            "isInClosedBlock" <- function(i) {
                a <- blockList[[i]]@ancestors
                result <- FALSE
                if (length(a) > 1) {
                  a <- a[a != 0]
                  result <- any(closedBlock[a])
                }
                return(result)
            }
            "setHiddenBlock" <- function(i, value, update = TRUE) if (i > 
                0) {
                hiddenBlock[i] <<- value
                if (value) {
                  if (all(is.na(positionsClosedBlocks[i, ]))) 
                    positionsClosedBlocks[i, ] <<- apply(positionsBlocks[i, 
                      , ], 1, mean)
                  if (closedBlock[i]) 
                    tkdelete(canvas, closedBlockItem(i)$tag)
                  else tkdelete(canvas, openBlockItem(i)$tag)
                }
            }
            "retStratum" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                strata <- function(i) if (i > 0) 
                  strataBlocks[i]
                else i
                if (vertex.type == "ClosedBlock") 
                  strataBlocks[abs(i)]
                else if (vertex.type == "Vertex") {
                  if (.IsEmpty(blockList)) 
                    strataVertices[i]
                  else if (blocksVertices[i] == 0) 
                    return(0)
                  else strata(blocksVertices[i])
                }
                else if (vertex.type == "Factor") {
                  if (.IsEmpty(blockList)) 
                    strataFactorVertices[-i]
                  else strata(blocksFactorVertices[-i])
                }
                else if (vertex.type == "Extra") {
                  if (.IsEmpty(blockList)) 
                    strataExtraVertices[abs(i)]
                  else strata(blocksExtraVertices[i])
                }
            }
            "retBlockIndex" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (vertex.type == "ClosedBlock") 
                  abs(i)
                else if (vertex.type == "Vertex") 
                  blocksVertices[i]
                else if (vertex.type == "Factor") 
                  blocksFactorVertices[-i]
                else if (vertex.type == "Extra") 
                  blocksExtraVertices[i]
            }
            "setBlockIndex" <- function(i, value, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                update <- FALSE
                if (permit.update.block.index) {
                  if (vertex.type == "Vertex") {
                    blocksVertices[i] <<- value
                    b <- closedBlock[value] || hiddenBlock[value]
                    if ((value > 0) && (b != closedVertex[i])) {
                      if (b %in% visibleBlocks) 
                        setCloseVertex(i, !closedVertex[i], vertex.type)
                      if (!closedVertex[i]) {
                        pos <- retVertexPos(i, vertex.type)
                        moveEdgesToVertex(pos, i, edge.type = "VertexEdge")
                      }
                      update <- TRUE
                    }
                  }
                  else if (vertex.type == "Factor") 
                    blocksFactorVertices[-i] <<- value
                  else if (vertex.type == "Extra") 
                    blocksExtraVertices[i] <<- value
                }
                return(update)
            }
            "updateVertexBlockIndex" <- function(position, i) {
                currentIndex <- retBlockIndex(i, vertex.type = "Vertex")
                update <- FALSE
                if (permit.update.block.index) {
                  if (!.IsEmpty(blockList)) {
                    k <- 0
                    for (j in seq(along = blockList)) if (inBlock(position, 
                      j)) {
                      k <- j
                    }
                    change <- setBlockIndex(i, k, vertex.type = "Vertex")
                    update <- update || change
                  }
                }
                return(update || (currentIndex != retBlockIndex(i, 
                  vertex.type = "Vertex")))
            }
            "updateAllBlockIndices" <- function() {
                updateEdges <- FALSE
                if (permit.update.block.index) {
                  if (!is.null(vertexList)) 
                    for (i in seq(along = vertexList)) {
                      update <- updateVertexBlockIndex(positionsVertices[i, 
                        ], i)
                      updateEdges <- updateEdges || update
                    }
                  if (updateEdges) {
                    setUpdateBlockEdges("updateAllBlockIndices")
                  }
                }
                return(updateEdges)
            }
            "findMove" <- function(position, dxy = rep(0, N)) {
                return(inversProject(inversCanvasPosition(positionsCanvas(project(position)) + 
                  dxy)))
            }
            "findDifference" <- function(p1, p2) {
                return(relativePositionsCanvas(project(inversProject(inversCanvasPosition(p1)) - 
                  inversProject(inversCanvasPosition(p2)))))
            }
            "retVertexPos" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (vertex.type == "ClosedBlock") 
                  position <- positionsClosedBlocks[abs(i), ]
                else if (vertex.type == "Vertex") {
                  if (closedVertex[i]) 
                    position <- positionsClosedBlocks[blockReferences[retBlockIndex(i, 
                      vertex.type)], ]
                  else position <- positionsVertices[i, ]
                }
                else if (vertex.type == "Factor") 
                  position <- positionsFactorVertices[-i, ]
                else if (vertex.type == "Extra") 
                  position <- positionsExtraVertices[abs(i), 
                    ]
                return(positionsCanvas(project(position)))
            }
            "setVertexPos" <- function(i, xy, dxy = rep(0, N), 
                vertex.type = ifelse(i > 0, "Vertex", "Factor")) {
                position <- inversProject(inversCanvasPosition(xy))
                if (vertex.type == "ClosedBlock") 
                  positionsClosedBlocks[i, ] <<- position
                else if (vertex.type == "Vertex") {
                  positionsVertices[i, ] <<- position
                  positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                    ], dxy)
                }
                else if (vertex.type == "Factor") {
                  positionsFactorVertices[-i, ] <<- position
                  positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                    ], dxy)
                }
                else if (vertex.type == "Extra") {
                  positionsExtraVertices[abs(i), ] <<- position
                  positionsExtraLabels[abs(i), ] <<- findMove(positionsExtraLabels[abs(i), 
                    ], dxy)
                }
                if ((vertex.type != "ClosedBlock") && (vertex.type != 
                  "Extra") && (vertex.type != "Factor")) 
                  if (permit.update.block.index) 
                    if (updateVertexBlockIndex(position, i)) {
                      setUpdateBlockEdges("setVertexPos")
                    }
            }
            "changeVertexPos" <- function(i, dxy = rep(0, N), 
                vertex.type = ifelse(i > 0, "Vertex", "Factor")) {
                if (vertex.type == "Vertex") {
                  positionsVertices[i, ] <<- findMove(positionsVertices[i, 
                    ], dxy)
                  positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                    ], dxy)
                }
                else if (vertex.type == "Factor") {
                  positionsFactorVertices[-i, ] <<- findMove(positionsFactorVertices[-i, 
                    ], dxy)
                  positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                    ], dxy)
                }
                else if (vertex.type == "Extra") {
                  positionsExtraVertices[i, ] <<- findMove(positionsExtraVertices[i, 
                    ], dxy)
                  positionsExtraLabels[i, ] <<- findMove(positionsExtraLabels[i, 
                    ], dxy)
                }
            }
            "retVertexName" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) if (vertex.type == "OpenBlock") 
                blockLabels[i]
            else if (vertex.type == "ClosedBlock") 
                blockLabels[abs(i)]
            else if (vertex.type == "Vertex") 
                namesVertices[i]
            else if (vertex.type == "Factor") 
                namesFactorVertices[-i]
            else if (vertex.type == "Extra") 
                extraLabels[abs(i)]
            "selectedNodesMatrix" <- function() {
                if (length(selectedNodes) > 0) {
                  r <- data.frame(index = unlist(lapply(selectedNodes, 
                    function(k) k$index)), hit = unlist(lapply(selectedNodes, 
                    function(k) k$hit.type)), type = unlist(lapply(selectedNodes, 
                    function(k) k$node.type)))
                  r
                }
            }
            "selectedEdgesMatrix" <- function() {
                if (length(selectedEdges) > 0) {
                  data.frame(index = unlist(lapply(selectedEdges, 
                    function(k) k$index)), from = unlist(lapply(selectedEdges, 
                    function(k) k$from)), to = unlist(lapply(selectedEdges, 
                    function(k) k$to)), hit = unlist(lapply(selectedEdges, 
                    function(k) k$hit.type)), type = unlist(lapply(selectedEdges, 
                    function(k) k$edge.type)))
                }
            }
            "retVertexColor" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (length(selectedNodes) > 0) {
                  x <- lapply(selectedNodes, function(k) ((i == 
                    k$index) && ("none" != k$hit.type) && (vertex.type == 
                    k$node.type)))
                  if (any(unlist(x))) 
                    return("YellowGreen")
                }
                if (vertex.type == "ClosedBlock") 
                  color(blockList[[i]])
                else if (vertex.type == "Vertex") 
                  colorsVertices[i]
                else if (vertex.type == "Factor") 
                  colorsFactorVertices[-i]
                else if (vertex.type == "Extra") 
                  colorsExtraVertices[abs(i)]
            }
            "setEdgeColor" <- function(i = 0, edge.type = "VertexEdge", 
                color = NULL) {
                activefill <- "LimeGreen"
                if (is.null(color)) 
                  activefill <- "DarkSlateGray"
                f <- function(x) if (is.null(x)) 
                  "NULL"
                else x
                E <- getEdges(edge.type = edge.type)[[i]]
                if (is.null(color)) 
                  color <- E@color
                setEdge <- function(k, edges) {
                  if (k != 0) {
                    edges <- edgeItem(k, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) 
                        if ((e$nr == i) && (e$type == edge.type)) 
                          if (e$to > k) {
                            for (l in 1:length(e$edges)) tkitemconfigure(canvas, 
                              e$edges[[l]], fill = color, activefill = activefill)
                            for (l in 1:length(e$tags)) tkitemconfigure(canvas, 
                              e$tags[[l]], fill = color, activefill = activefill)
                          }
                  }
                }
                for (j in E@vertex.indices) setEdge(j, edgeItem(j, 
                  edge.type = edge.type))
            }
            "setEdgeDash" <- function(i = 0, edge.type = "VertexEdge", 
                dash = NULL) {
                f <- function(x) if (is.null(x)) 
                  "NULL"
                else x
                E <- getEdges(edge.type = edge.type)[[i]]
                if (is.null(color)) 
                  color <- E@color
                setEdge <- function(k, edges) {
                  if (k != 0) {
                    edges <- edgeItem(k, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) 
                        if ((e$nr == i) && (e$type == edge.type)) 
                          if (e$to > k) {
                            for (l in 1:length(e$edges)) tkitemconfigure(canvas, 
                              e$edges[[l]], dash = dash)
                            for (l in 1:length(e$tags)) tkitemconfigure(canvas, 
                              e$tags[[l]], dash = dash)
                          }
                  }
                }
                for (j in E@vertex.indices) setEdge(j, edgeItem(j, 
                  edge.type = edge.type))
            }
            "setVertexColor" <- function(i, color = retVertexColor(i, 
                vertex.type), vertex.type = ifelse(i > 0, "Vertex", 
                "Factor"), permanent = FALSE) {
                if (!(length(color) == 1)) {
                  print(color)
                  print(i)
                }
                if (color == "cyan") 
                  activefill <- "DarkCyan"
                else activefill <- "LimeGreen"
                if (color == retVertexColor(i, vertex.type)) 
                  activefill <- "IndianRed"
                items <- vertexItem(i, vertex.type)$dot$dynamic
                if (!is.null(items)) 
                  if (length(items) > 0) 
                    for (k in seq(length(items))) tkitemconfigure(canvas, 
                      items[[k]], fill = color[[1]], activefill = activefill)
                if (permanent) {
                  if (vertex.type == "ClosedBlock") 
                    color(blockList[[i]]) <<- color
                  else if (vertex.type == "Vertex") 
                    colorsVertices[i] <<- color
                  else if (vertex.type == "Factor") 
                    colorsFactorVertices[abs(i)] <<- color
                  else if (vertex.type == "Extra") 
                    colorsExtraVertices[abs(i)] <<- color
                }
            }
            "retVertexLabel" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) if (useNamesForLabels) {
                if ((vertex.type == "OpenBlock") || (vertex.type == 
                  "ClosedBlock")) 
                  blockLabels[i]
                else if (vertex.type == "Vertex") 
                  namesVertices[i]
                else if (vertex.type == "Factor") 
                  namesFactorVertices[-i]
                else if (vertex.type == "Extra") 
                  namesExtraVertices[i]
            }
            else {
                if ((vertex.type == "OpenBlock") || (vertex.type == 
                  "ClosedBlock")) 
                  blockLabels[i]
                else if (vertex.type == "Vertex") 
                  Labels[i]
                else if (vertex.type == "Factor") 
                  factorLabels[-i]
                else if (vertex.type == "Extra") 
                  extraLabels[i]
            }
            "setVertexLabel" <- function(i, label, vertex.type) {
                if (vertex.type == "ClosedBlock") {
                  blockLabels[i] <<- label
                  tkitemconfigure(canvas, itemsClosedBlocks[[i]]$l, 
                    text = label)
                }
                else if (vertex.type == "Vertex") {
                  Labels[i] <<- label
                  tkitemconfigure(canvas, itemsVertices[[i]]$l, 
                    text = label)
                }
                else if (vertex.type == "Factor") {
                  factorLabels[-i] <<- label
                  tkitemconfigure(canvas, itemsFactors[[-i]]$l, 
                    text = label)
                }
                else if (vertex.type == "Extra") {
                  extraLabels[i] <<- label
                  tkitemconfigure(canvas, itemsExtras[[i]]$l, 
                    text = label)
                }
            }
            "retLabelPos" <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                if (vertex.type == "ClosedBlock") 
                  position <- positionsBlockLabels[i, ]
                else if (vertex.type == "Vertex") 
                  position <- positionsLabels[i, ]
                else if (vertex.type == "Factor") 
                  position <- positionsFactorLabels[-i, ]
                else if (vertex.type == "Extra") 
                  position <- positionsExtraLabels[i, ]
                positionsCanvas(project(position))
            }
            "setLabelPos" <- function(i, xy, dxy = rep(0, N), 
                vertex.type = ifelse(i > 0, "Vertex", "Factor")) if (vertex.type == 
                "ClosedBlock") {
                positionsBlockLabels[i, ] <<- positionsBlockLabels[i, 
                  ] + inversCanvasRelativePosition(dxy)
            }
            else if (vertex.type == "Vertex") 
                positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                  ], dxy)
            else if (vertex.type == "Factor") 
                positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                  ], dxy)
            else if (vertex.type == "Extra") 
                positionsExtraLabels[i, ] <<- findMove(positionsExtraLabels[i, 
                  ], dxy)
            "retEdgeLabelPos" <- function(label.number, f = 0, 
                t = 0) relativePositionsCanvas(positionsEdgeLabels[label.number, 
                ])
            "setEdgeLabelPos" <- function(edgeNode, label.number, 
                xy, dxy = rep(0, N), f = 0, t = edgeNode$to, 
                edge.type = edgeNode$type) {
                positionsEdgeLabels[label.number, ] <<- positionsEdgeLabels[label.number, 
                  ] + inversCanvasRelativePosition(dxy)
                E <- getEdges(edge.type = edge.type)
                labelPosition(E[[edgeNode$nr]]) <- positionsEdgeLabels[label.number, 
                  ]
            }
            "setEdgeLabel" <- function(edgeNode, label = "", 
                i = edgeNode$label.number, f = 0, t = edgeNode$to, 
                edge.type = edgeNode$type, permanent = TRUE) {
                if (debug.strata && (label != " ")) {
                  text <- paste(paste(i, paste(f, t, sep = "-"), 
                    sep = "<"), label, sep = ">")
                  tkitemconfigure(canvas, edgeNode$label, text = text)
                  tkitemconfigure(canvas, edgeNode$label, fill = myColor(i + 
                    2))
                }
                else tkitemconfigure(canvas, edgeNode$label, 
                  text = label)
                if (permanent) {
                  if (edgeNode$type == "VertexEdge") 
                    label(GraphWindow@vertexEdges[[edgeNode$nr]]) <<- label
                  else if (edgeNode$type == "FactorEdge") 
                    label(GraphWindow@factorEdges[[edgeNode$nr]]) <<- label
                  else if (edgeNode$type == "ExtraEdge") 
                    label(GraphWindow@extraEdges[[edgeNode$nr]]) <<- label
                  else if (edgeNode$type == "BlockEdge") 
                    label(GraphWindow@blockEdges[[edgeNode$nr]]) <<- label
                }
            }
            "retEdgeLabel" <- function(edgeNode, i, f, t, edge.type = "VertexEdge") {
                if (edgeNode$type == "VertexEdge") 
                  label(GraphWindow@vertexEdges[[edgeNode$nr]])
                else if (edgeNode$type == "FactorEdge") 
                  label(GraphWindow@factorEdges[[edgeNode$nr]])
                else if (edgeNode$type == "ExtraEdge") 
                  label(GraphWindow@extraEdges[[edgeNode$nr]])
                else if (edgeNode$type == "BlockEdge") 
                  label(GraphWindow@blockEdges[[edgeNode$nr]])
            }
            "setEdgeWidth" <- function(edgeNode, width = 1, i = edgeNode$label.number, 
                f = 0, t = edgeNode$to, edge.type = edgeNode$type) {
                for (l in 1:length(edgeNode$edges)) tkitemconfigure(canvas, 
                  edgeNode$edges[[l]], width = width)
                if (edgeNode$type == "VertexEdge") 
                  width(GraphWindow@vertexEdges[[edgeNode$nr]]) <<- width
                else if (edgeNode$type == "FactorEdge") 
                  width(GraphWindow@factorEdges[[edgeNode$nr]]) <<- width
                else if (edgeNode$type == "ExtraEdge") 
                  width(GraphWindow@extraEdges[[edgeNode$nr]]) <<- width
                else if (edgeNode$type == "BlockEdge") 
                  width(GraphWindow@blockEdges[[edgeNode$nr]]) <<- width
            }
            "vertexTypeOfEdge" <- function(index, edge.type = "VertexEdge", 
                edgeObject = NULL) if (edge.type == "factorBlockEdge") 
                ifelse(index > 0, "Factor", "ClosedBlock")
            else ifelse(index > 0, "Vertex", ifelse(edge.type == 
                "FactorEdge", "Factor", ifelse(edge.type == "ExtraEdge", 
                "Extra", "ClosedBlock")))
            "displayNode" <- function(i, type) {
                display <- TRUE
                if (type == "Factor") {
                }
                else if (type == "Extra") {
                }
                else if (type == "ClosedBlock") {
                  if (!closedBlock[abs(i)]) 
                    display <- FALSE
                  if (hiddenBlock[abs(i)]) 
                    display <- FALSE
                  if (!(abs(i) %in% visibleBlocks)) 
                    display <- FALSE
                }
                else if (closedVertex[abs(i)]) 
                  display <- FALSE
                return(display)
            }
            "displayEdge" <- function(edgeNode, f, t, from.type = vertexTypeOfEdge(f, 
                edgeNode$type), to.type = vertexTypeOfEdge(t, 
                edgeNode$type)) {
                display <- displayNode(f, from.type)
                if (display) 
                  display <- displayNode(t, to.type)
                return(display)
            }
            "setEdgeCoords" <- function(edgeNode, posFrom, posTo, 
                f, t, from.type = vertexTypeOfEdge(f, edgeNode$type), 
                to.type = vertexTypeOfEdge(t, edgeNode$type), 
                width = w) {
                stratumFrom <- retStratum(f, from.type)
                stratumTo <- retStratum(t, to.type)
                reverse <- edgeNode$reverse
                display <- displayEdge(edgeNode, f, t, from.type, 
                  to.type)
                if ((stratumFrom != 0) || (stratumTo != 0) || 
                  !oriented) {
                  for (l in 1:length(edgeNode$edges)) if (stratumFrom == 
                    stratumTo) 
                    tkitemconfigure(canvas, edgeNode$edges[[l]], 
                      arrow = "none")
                  else tkitemconfigure(canvas, edgeNode$edges[[l]], 
                    arrow = "last")
                  reverse <- (stratumFrom > stratumTo)
                }
                if (display) {
                  diff <- posTo - posFrom
                  l <- sqrt(sum(diff^2))
                  posTo <- posTo - diff * min(2 * w, l)/l
                  posFrom <- posFrom + diff * min(2 * w, l)/l
                  label <- retEdgeLabel(edgeNode, edgeNode$label.number, 
                    f, t, edge.type = edgeNode$type)
                  tkitemconfigure(canvas, edgeNode$label, text = label)
                }
                else {
                  posTo <- c(0, 0)
                  posFrom <- c(0, 0)
                  tkitemconfigure(canvas, edgeNode$label, text = "")
                }
                g <- function(pos, ll) {
                  dxy <- tkcoords(canvas, edgeNode$tags[[ll]])
                  dxy <- apply(matrix(as.numeric(dxy), ncol = 2, 
                    byrow = 2), 2, mean)
                  dxy <- pos[1:2] - dxy
                  tkmove(canvas, edgeNode$tags[[ll]], dxy[1], 
                    dxy[2])
                }
                pos <- (posFrom + posTo)/2
                if (length(edgeNode$tags) > 0) 
                  for (ll in 1:length(edgeNode$tags)) g(pos, 
                    ll)
                f <- function(posFrom, posTo, ll) {
                  if (reverse) 
                    tkcoords(canvas, edgeNode$edges[[ll]], posTo[1], 
                      posTo[2], posFrom[1], posFrom[2])
                  else tkcoords(canvas, edgeNode$edges[[ll]], 
                    posFrom[1], posFrom[2], posTo[1], posTo[2])
                }
                if (length(edgeNode$edges) == 1) 
                  f(posFrom, posTo, 1)
                else {
                  d <- posFrom - posTo
                  ld <- sqrt(sum(d[1:2]^2))
                  e <- width * d[1:2]/ld/2 * (length(edgeNode$edges) - 
                    1)/4
                  d <- width * c(-d[2], d[1])/ld/2 * (length(edgeNode$edges) - 
                    1)
                  if (length(edgeNode$edges) == 2) {
                    f(posFrom[1:2] + d, posTo[1:2] + d, 1)
                    f(posFrom[1:2] - d, posTo[1:2] - d, 2)
                  }
                  else {
                    for (lll in 1:length(edgeNode$edges)) {
                      kk <- lll - (length(edgeNode$edges) + 1)/2
                      f(posFrom[1:2] + kk * d + abs(kk) * e, 
                        posTo[1:2] + kk * d - abs(kk) * e, lll)
                    }
                  }
                }
            }
            "retBlockPos" <- function(i, j) {
                position <- positionsBlocks[i, , j]
                positionsCanvas(project(position))
            }
            "changeBlockCornerPos" <- function(i, A, dxy) {
                db <- toBlockPoints(A, dxy)
                positionsBlocks[i, , 1] <<- findMove(positionsBlocks[i, 
                  , 1], db[1, ])
                positionsBlocks[i, , 2] <<- findMove(positionsBlocks[i, 
                  , 2], db[2, ])
            }
            "changeBlockPos" <- function(i, A, dxy) {
                positionsBlocks[i, , 1] <<- findMove(positionsBlocks[i, 
                  , 1], dxy)
                positionsBlocks[i, , 2] <<- findMove(positionsBlocks[i, 
                  , 2], dxy)
            }
            "inBlock" <- function(position, block) {
                if (is.null(blockList[[block]])) 
                  return(FALSE)
                else {
                  block.position <- t(positionsBlocks[block, 
                    , ])
                  if (!all((block.position[1, ] < block.position[2, 
                    ]))) 
                    warning("Invalid block positions")
                  return(all((block.position[1, ] < position) & 
                    (position < block.position[2, ])))
                }
            }
            "retBlockPoints" <- function(i, header = FALSE, box = FALSE, 
                n) {
                A <- positionsBlocks[i, , 1]
                if (header) {
                  if (box) {
                    A <- A + c(1, 1, rep(0, N - 2))
                    B <- A + c(3 * n, 5, rep(0, N - 2))
                  }
                  else {
                    B <- positionsBlocks[i, , 2]
                    A <- A + c(1, 5, rep(0, N - 2))
                    B[1] <- B[1] - 1
                    B[2] <- A[2] + 1
                  }
                }
                else B <- positionsBlocks[i, , 2]
                delta <- c(0, 0, 0)
                position <- matrix(c(c(A[1], A[2], A[3]), c(B[1], 
                  B[2], B[3]) + delta, c(A[1], B[2], A[3]), c(B[1], 
                  A[2], A[3]), c(A[1], A[2], B[3]) + delta, c(A[1], 
                  B[2], B[3]) + delta, c(B[1], A[2], B[3]) + 
                  delta, c(B[1], B[2], A[3])), ncol = 3, byrow = TRUE)
                if (N > 3) 
                  for (i in 4:N) position <- cbind(position, 
                    rep(0, 8))
                positionsCanvas(project(position))
            }
            "toBlockPoints" <- function(n, p) {
                result <- switch(EXPR = paste(n - 1), "0" = c(c(p[1], 
                  p[2], p[3]), c(0, 0, 0)), "1" = c(c(0, 0, 0), 
                  c(p[1], p[2], p[3])), "2" = c(c(p[1], 0, p[3]), 
                  c(0, p[2], 0)), "3" = c(c(0, p[2], p[3]), c(p[1], 
                  0, 0)), "4" = c(c(p[1], p[2], 0), c(0, 0, p[3])), 
                  "5" = c(c(p[1], 0, 0), c(0, p[2], p[3])), "6" = c(c(0, 
                    p[2], 0), c(p[1], 0, p[3])), "7" = c(c(0, 
                    0, p[3]), c(p[1], p[2], 0)))
                result <- matrix(result, ncol = 3, byrow = TRUE)
                if (N > 3) 
                  for (i in 4:N) result <- cbind(result, rep(0, 
                    8))
                return(result)
            }
            "retBlockLabelPos" <- function(i) relativePositionsCanvas(positionsBlockLabels[i, 
                ])
            "addEdgePopups" <- function(canvas, edge, i, f, t, 
                edgePopupMenu, UserMenus, edge.type = "VertexEdge") {
                tkadd(edgePopupMenu, "command", label = paste("Edge from", 
                  retVertexLabel(f), "to", retVertexLabel(t), 
                  "(echo indices)"), command = function() {
                  print("Hej from edge")
                  print(c(f, t))
                })
                tkadd(edgePopupMenu, "command", label = paste("Delete edge (Here: Slave view!)"), 
                  accelerator = "[ double click edge ]", command = function() subDropEdge(i, 
                    f, t, edge.type = edge.type, slave = TRUE))
                tkadd(edgePopupMenu, "command", label = paste("Delete all edges to/from blocks"), 
                  command = function() subDropEdge(i, f, t, from.all = TRUE, 
                    to.all = TRUE, edge.type = edge.type, slave = FALSE))
                propEdgeMenu <- tkmenu(edgePopupMenu, tearoff = FALSE)
                tkadd(propEdgeMenu, "command", label = paste("Open dialog box for slot values"), 
                  command = function() propertyEdge(i, f, t, 
                    edge.type = edge.type)())
                tkadd(propEdgeMenu, "command", label = paste("/ Change edge class"), 
                  command = function() changeEdgeClass(i, f, 
                    t, edge.type = edge.type)())
                tkadd(propEdgeMenu, "command", label = paste("/ Set edge label"), 
                  command = function() {
                    activateEdge(i, from = f, to = t, edge.type = edge.type)()
                    changeEdgeLabel(i, f, t, edge.type = edge.type)()
                  })
                tkadd(propEdgeMenu, "command", label = paste("/ Compute edge label"), 
                  accelerator = "[ click label ]", command = function() {
                    activateEdge(i, from = f, to = t, edge.type = edge.type)()
                    computeEdgeLabel(i, f, t, FALSE, edge.type = edge.type)()
                  })
                tkadd(propEdgeMenu, "command", label = paste("/ Force computation of edge label"), 
                  accelerator = "[ double click label ]", command = function() {
                    activateEdge(i, from = f, to = t, edge.type = edge.type)()
                    computeEdgeLabel(i, f, t, TRUE, edge.type = edge.type)()
                  })
                tkadd(propEdgeMenu, "command", label = paste("/ Delete label of edge"), 
                  accelerator = "[ triple click label ]", command = function() deleteEdgeLabel(i, 
                    f, t, edge.type = edge.type)())
                tkadd(edgePopupMenu, "cascade", label = "Properties", 
                  menu = propEdgeMenu)
                helpEdgeMenu <- tkmenu(edgePopupMenu, tearoff = FALSE)
                tkadd(helpEdgeMenu, "command", label = paste(" - Add edge: Left click the vertices of the edge to add"), 
                  command = function() message("Left click the vertices of the edge to add"))
                tkadd(helpEdgeMenu, "command", label = paste(" - Drag edge: Move edge with two vertices"), 
                  command = function() message("Left click edge and drag edge"))
                tkadd(helpEdgeMenu, "command", label = paste(" - Drag label: Move label of edge"), 
                  command = function() message("Left click edge label and drag label"))
                tkadd(edgePopupMenu, "cascade", label = "Help on edges", 
                  menu = helpEdgeMenu)
                methEdgeMenu <- tkmenu(edgePopupMenu, tearoff = FALSE)
                if (hasMethod("addToPopups", class(edge))) 
                  addToPopups(edge, edge.type, methEdgeMenu, 
                    i, updateArguments, Args)
                tkadd(edgePopupMenu, "cascade", label = "Items by method 'addToPopups'", 
                  menu = methEdgeMenu)
                userEdgeMenu <- tkmenu(edgePopupMenu, tearoff = FALSE)
                UserEdgePopup <- function(item) {
                  force(item)
                  force(f)
                  force(t)
                  force(edge.type)
                  force(edge)
                  function(...) {
                    updateArguments(UserMenus[[item]])
                    j <- which.unordered.edge(c(t, f), edge.type = edge.type)
                    from.type <- vertexTypeOfEdge(f, edge.type, 
                      edge)
                    to.type <- vertexTypeOfEdge(t, edge.type, 
                      edge)
                    UserMenus[[item]]$command(object, retVertexName(f, 
                      from.type), retVertexName(t, to.type), 
                      from = f, to = t, from.type = from.type, 
                      to.type = to.type, edge.index = i, which.edge = j, 
                      edge.type = edge.type, Arguments = Args())
                  }
                }
                if (length(UserMenus) > 0) 
                  for (item in seq(along = UserMenus)) if (names(UserMenus[item]) == 
                    "Edge") 
                    tkadd(userEdgeMenu, "command", label = UserMenus[[item]]$label, 
                      command = UserEdgePopup(item))
                tkadd(edgePopupMenu, "cascade", label = "User defined items", 
                  menu = userEdgeMenu)
            }
            "setEdgePopup" <- function(canvas, edge, line, label, 
                i, f, t, UserMenus, edge.type = "VertexEdge") {
                edgePopupMenu <- tkmenu(canvas, tearoff = FALSE)
                addEdgePopups(canvas, edge, i, f, t, edgePopupMenu, 
                  UserMenus, edge.type)
                tkitembind(canvas, label, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                tkitembind(canvas, label, "<Enter>", function() tkconfigure(canvas, 
                  cursor = "hand1"))
                tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                  cursor = "tcross"))
                tkitembind(canvas, label, "<Button-1>", activateEdge(i, 
                  from = f, to = t, edge.type = edge.type))
                tkitembind(canvas, label, "<B1-Motion>", moveEdgeLabel(i, 
                  f, t, edge.type = edge.type))
                tkitembind(canvas, label, "<ButtonRelease-1>", 
                  computeEdgeLabel(i, f, t, FALSE, edge.type = edge.type))
                tkitembind(canvas, label, "<Double-Button-1>", 
                  computeEdgeLabel(i, f, t, TRUE, edge.type = edge.type))
                tkitembind(canvas, label, "<Triple-Button-1>", 
                  deleteEdgeLabel(i, f, t, edge.type = edge.type))
                tkitembind(canvas, label, "<Shift-1>", changeEdgeClass(i, 
                  f, t, edge.type = edge.type))
                tkitembind(canvas, label, "<Button-3>", callPopup(i, 
                  edgePopupMenu))
                tkitembind(canvas, line, "<Option-1>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "option-1", 
                  color = "DarkGreen"))
                tkitembind(canvas, line, "<Shift-1>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "shift-1", 
                  color = "DarkGreen"))
                tkitembind(canvas, line, "<Control-1>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "control-1", 
                  color = "SeaGreen"))
                tkitembind(canvas, line, "<Shift-Control-1>", 
                  activateEdge(i, from = f, to = t, edge.type, 
                    hit.type = "shift-control-1", color = "LightSeaGreen"))
                tkitembind(canvas, line, "<Option-3>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "option-3", 
                  color = "LightGreen"))
                tkitembind(canvas, line, "<Shift-3>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "shift-3", 
                  color = "LightGreen"))
                tkitembind(canvas, line, "<Control-3>", activateEdge(i, 
                  from = f, to = t, edge.type, hit.type = "control-3", 
                  color = "SpringGreen"))
                tkitembind(canvas, line, "<Shift-Control-3>", 
                  activateEdge(i, from = f, to = t, edge.type, 
                    hit.type = "shift-control-3", color = "LimeGreen"))
                tkitembind(canvas, line, "<Button-1>", activateEdge(i, 
                  from = f, to = t, edge.type = edge.type))
                tkitembind(canvas, line, "<Double-Button-1>", 
                  deleteEdge(i, f, t, edge.type = edge.type))
                tkitembind(canvas, line, "<B1-Motion>", moveEdge(i, 
                  f, t, edge.type = edge.type))
                tkitembind(canvas, line, "<Button-3>", callPopup(i, 
                  edgePopupMenu))
            }
            "drawEdge" <- function(edge, i, edgecolor = "black", 
                lower = FALSE, edge.type = "VertexEdge", newE = FALSE) {
                tag <- getTag(edge.type, i)
                type.negative <- ifelse(edge.type == "BlockEdge", 
                  "ClosedBlock", ifelse(edge.type == "FactorEdge", 
                    "Factor", "Extra"))
                useMethod <- FALSE
                if (!is.null(edge)) 
                  useMethod <- hasMethod("draw", class(edge))
                if (useMethod) {
                  position <- edge.positions(i, type.negative = type.negative, 
                    edge.type = edge.type)
                  strata <- edge.strata(i, type.negative = type.negative, 
                    edge.type = edge.type)
                  x <- lapply(position, function(e) e[1])
                  y <- lapply(position, function(e) e[2])
                  result <- draw(edge, canvas, position, x, y, 
                    stratum = strata, w = edge@width * Scale, 
                    color = edge@color, font.edge.label = font.edge.label, 
                    background = background)
                }
                else {
                  f <- from(i, edge.type = edge.type)
                  t <- to(i, edge.type = edge.type)
                  from.type <- vertexTypeOfEdge(f, edge.type, 
                    edge)
                  to.type <- vertexTypeOfEdge(t, edge.type, edge)
                  posFrom <- retVertexPos(f, from.type)
                  posTo <- retVertexPos(t, to.type)
                  stratumFrom <- retStratum(f, from.type)
                  stratumTo <- retStratum(t, to.type)
                  if (stratumFrom == stratumTo) 
                    arrowhead = "none"
                  else if (stratumFrom < stratumTo) 
                    arrowhead = "last"
                  else arrowhead = "first"
                  E <- getEdges(edge.type = edge.type)[[i]]
                  line <- tkcreate(canvas, "line", posFrom[1], 
                    posFrom[2], posTo[1], posTo[2], arrow = arrowhead, 
                    width = E@width, fill = E@color)
                  label.position <- (posFrom + posTo)/2
                  pos <- label.position + rep(0, N)
                  txt <- E@label
                  label <- tkcreate(canvas, "text", pos[1], pos[2], 
                    text = txt, anchor = "nw", font = font.edge.label, 
                    activefill = "DarkSlateGray")
                  result <- list(list(line = list(line), from = f, 
                    to = t, label = label, label.position = label.position))
                }
                for (k in 1:length(result)) {
                  positionsEdgeLabels <<- rbind(positionsEdgeLabels, 
                    rep(0, N))
                  tkaddtag(canvas, tag, "withtag", result[[k]]$label)
                  for (l in 1:length(result[[k]]$lines)) tkaddtag(canvas, 
                    tag, "withtag", result[[k]]$lines[[l]])
                  if (length(result[[k]]$tags) > 0) 
                    for (l in 1:length(result[[k]]$tags)) tkaddtag(canvas, 
                      tag, "withtag", result[[k]]$tags[[l]])
                  f <- result[[k]]$from
                  t <- result[[k]]$to
                  edgeNode <- list(nr = i, type = edge.type, 
                    to = t, tag = tag, reverse = FALSE, edges = result[[k]]$lines, 
                    tags = result[[k]]$tags, label = result[[k]]$label, 
                    label.number = nrow(positionsEdgeLabels))
                  setEdgeItem(f, edge.type = edge.type, c(edgeItem(f, 
                    edge.type = edge.type), list(edgeNode)))
                  from.type <- vertexTypeOfEdge(f, edge.type, 
                    edge)
                  to.type <- vertexTypeOfEdge(t, edge.type, edge)
                  posFrom <- retVertexPos(f, from.type)
                  posTo <- retVertexPos(t, to.type)
                  setEdgeCoords(edgeNode, posFrom, posTo, f, 
                    t, from.type, to.type, width = w)
                  if (newE) {
                    tkitemconfigure(canvas, result[[k]]$label, 
                      text = edge@label)
                  }
                  edgeNode <- list(nr = i, type = edge.type, 
                    to = f, tag = tag, reverse = TRUE, edges = result[[k]]$lines, 
                    tags = result[[k]]$tags, label = result[[k]]$label, 
                    label.number = nrow(positionsEdgeLabels))
                  setEdgeItem(t, edge.type = edge.type, c(edgeItem(t, 
                    edge.type = edge.type), list(edgeNode)))
                  for (l in 1:length(result[[k]]$lines)) setEdgePopup(canvas, 
                    edge, result[[k]]$lines[[l]], result[[k]]$label, 
                    i, f, t, UserMenus, edge.type = edge.type)
                  if (length(result[[k]]$tags) > 0) 
                    for (l in 1:length(result[[k]]$tags)) setEdgePopup(canvas, 
                      edge, result[[k]]$tags[[l]], result[[k]]$label, 
                      i, f, t, UserMenus, edge.type = edge.type)
                }
            }
            tkcoordsBlock <- function(i, color = "black", lower = FALSE) {
                tkcoordsRectangleLine <- function(line, i, A, 
                  B, positions, color = "black", width = 1) {
                  posA <- positions[A, ]
                  posB <- positions[B, ]
                  tkcoords(canvas, line, posA[1], posA[2], posB[1], 
                    posB[2])
                }
                tkcoordsCornerLine <- function(line, i, A, posA, 
                  posB, color = "black", width = 1) {
                  diff <- posB - posA
                  l <- sqrt(sum(diff^2))
                  posB <- posA + diff * min(30, l)/l
                  posA <- posA - diff * min(w/2, l)/l
                  tkcoords(canvas, line, posA[1], posA[2], posB[1], 
                    posB[2])
                }
                tkcoordsRectangleCorner <- function(line, i, 
                  A, B, C, D, positions, color = "black", width = 2) {
                  posA <- positions[A, ]
                  tkcoordsCornerLine(line[[1]], i, A, posA, positions[B, 
                    ], color, width)
                  tkcoordsCornerLine(line[[2]], i, A, posA, positions[C, 
                    ], color, width)
                  if (!is.null(transformation)) 
                    tkcoordsCornerLine(line[[3]], i, A, posA, 
                      positions[D, ], color, width)
                }
                "tkcoordsRectangle" <- function(rectangle, i, 
                  positions, color = "black", width = 1) {
                  line <- rectangle$Lines
                  tkcoordsRectangleLine(line[[1]], i, 1, 3, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[2]], i, 4, 8, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[3]], i, 1, 4, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[4]], i, 3, 8, positions, 
                    color, width)
                  if (!is.null(transformation)) {
                    tkcoordsRectangleLine(line[[5]], i, 5, 6, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[6]], i, 7, 2, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[7]], i, 5, 7, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[8]], i, 6, 2, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[9]], i, 1, 5, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[10]], i, 3, 6, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[11]], i, 4, 7, 
                      positions, color, width)
                    tkcoordsRectangleLine(line[[12]], i, 8, 2, 
                      positions, color, width)
                  }
                  corner <- rectangle$Corners
                  tkcoordsRectangleCorner(corner[[1]], i, 1, 
                    3, 4, 5, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[2]], i, 8, 
                    4, 3, 2, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[3]], i, 4, 
                    1, 8, 7, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[4]], i, 3, 
                    8, 1, 6, positions, color, width + 2)
                  if (!is.null(transformation)) {
                    tkcoordsRectangleCorner(corner[[5]], i, 5, 
                      6, 7, 1, positions, color, width + 2)
                    tkcoordsRectangleCorner(corner[[6]], i, 2, 
                      7, 6, 8, positions, color, width + 2)
                    tkcoordsRectangleCorner(corner[[7]], i, 7, 
                      5, 2, 4, positions, color, width + 2)
                    tkcoordsRectangleCorner(corner[[8]], i, 6, 
                      2, 5, 3, positions, color, width + 2)
                  }
                }
                "tkcoordsBar" <- function(line, i, positions, 
                  color = "black", width = 1) {
                  tkcoordsRectangleLine(line[[1]], i, 1, 3, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[2]], i, 4, 8, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[3]], i, 1, 4, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[4]], i, 3, 8, positions, 
                    color, width)
                }
                positions <- retBlockPoints(i)
                if (!is.null(openBlockItem(i)$canvas)) 
                  tkcoords(canvas, openBlockItem(i)$canvas, positions[1, 
                    1], positions[1, 2], positions[8, 1], positions[8, 
                    2])
                if (!is.null(openBlockItem(i)$rectangle)) 
                  tkcoordsRectangle(openBlockItem(i)$rectangle, 
                    i, positions, color = color, width = 1)
                txt <- blockList[[i]]@label
                positions <- retBlockPoints(i, header = TRUE, 
                  n = nchar(txt))
                if (!is.null(openBlockItem(i)$bar)) 
                  tkcoordsBar(openBlockItem(i)$bar, i, positions, 
                    color = color, width = 1)
                pos <- retBlockPos(i, 1) + c(8, 4, 0)
                if (!is.null(openBlockItem(i)$label)) 
                  tkcoords(canvas, openBlockItem(i)$label, pos[1], 
                    pos[2])
            }
            "drawBlock" <- function(block, i, color = "Grey", 
                box = FALSE, lower = TRUE, setTag = TRUE) {
                "drawRectangleLine" <- function(i, A, B, positions, 
                  tag, color = "black", width = 1) {
                  posA <- positions[A, ]
                  posB <- positions[B, ]
                  line <- tkcreate(canvas, "line", posA[1], posA[2], 
                    posB[1], posB[2], width = width, fill = color)
                  tkaddtag(canvas, tag, "withtag", line)
                  tkitembind(canvas, line, "<B1-Motion>", moveBlockLine(i, 
                    A, B))
                  tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                    cursor = "arrow"))
                  if ((A == 1) && (B == 3)) 
                    cursor <- "left_side"
                  else if ((A == 4) && (B == 8)) 
                    cursor <- "right_side"
                  else if ((A == 1) && (B == 4)) 
                    cursor <- "top_side"
                  else if ((A == 3) && (B == 8)) 
                    cursor <- "bottom_side"
                  else if ((A == 5) && (B == 6)) 
                    cursor <- "left_side"
                  else if ((A == 7) && (B == 2)) 
                    cursor <- "right_side"
                  else if ((A == 5) && (B == 7)) 
                    cursor <- "top_side"
                  else if ((A == 6) && (B == 2)) 
                    cursor <- "bottom_side"
                  else if ((A == 1) && (B == 5)) 
                    cursor <- "top_side"
                  else if ((A == 3) && (B == 6)) 
                    cursor <- "bottom_side"
                  else if ((A == 4) && (B == 7)) 
                    cursor <- "top_side"
                  else if ((A == 8) && (B == 2)) 
                    cursor <- "bottom_side"
                  tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                    cursor = cursor))
                  return(line)
                }
                "drawCornerLine" <- function(i, A, posA, posB, 
                  tag, color = "black", width = 1) {
                  diff <- posB - posA
                  l <- sqrt(sum(diff^2))
                  posB <- posA + diff * min(25, l)/l
                  posA <- posA - diff * min(w/2, l)/l
                  line <- tkcreate(canvas, "line", posA[1], posA[2], 
                    posB[1], posB[2], width = width, fill = color)
                  tkaddtag(canvas, tag, "withtag", line)
                  tkitembind(canvas, line, "<B1-Motion>", moveBlockPoint(i, 
                    A))
                  tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                    cursor = "arrow"))
                  if ((A == 1) || (A == 5)) 
                    cursor <- "top_left_corner"
                  else if ((A == 8) || (A == 2)) 
                    cursor <- "bottom_right_corner"
                  else if ((A == 4) || (A == 7)) 
                    cursor <- "top_right_corner"
                  else if ((A == 3) || (A == 6)) 
                    cursor <- "bottom_left_corner"
                  tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                    cursor = cursor))
                  return(line)
                }
                "drawRectangleCorner" <- function(i, A, B, C, 
                  D, positions, tag, color = "black", width = 2) {
                  posA <- positions[A, ]
                  l <- vector("list", 3)
                  l[[1]] <- drawCornerLine(i, A, posA, positions[B, 
                    ], tag, color, width)
                  l[[2]] <- drawCornerLine(i, A, posA, positions[C, 
                    ], tag, color, width)
                  if (!is.null(transformation)) 
                    l[[3]] <- drawCornerLine(i, A, posA, positions[D, 
                      ], tag, color, width)
                  return(l)
                }
                "drawRectangle" <- function(i, positions, tag, 
                  color = "black", width = 1) {
                  l <- vector("list", 12)
                  l[[1]] <- drawRectangleLine(i, 1, 3, positions, 
                    tag, color, width)
                  l[[2]] <- drawRectangleLine(i, 4, 8, positions, 
                    tag, color, width)
                  l[[3]] <- drawRectangleLine(i, 1, 4, positions, 
                    tag, color, width)
                  l[[4]] <- drawRectangleLine(i, 3, 8, positions, 
                    tag, color, width)
                  if (!is.null(transformation)) {
                    l[[5]] <- drawRectangleLine(i, 5, 6, positions, 
                      tag, color, width)
                    l[[6]] <- drawRectangleLine(i, 7, 2, positions, 
                      tag, color, width)
                    l[[7]] <- drawRectangleLine(i, 5, 7, positions, 
                      tag, color, width)
                    l[[8]] <- drawRectangleLine(i, 6, 2, positions, 
                      tag, color, width)
                    l[[9]] <- drawRectangleLine(i, 1, 5, positions, 
                      tag, color, width)
                    l[[10]] <- drawRectangleLine(i, 3, 6, positions, 
                      tag, color, width)
                    l[[11]] <- drawRectangleLine(i, 4, 7, positions, 
                      tag, color, width)
                    l[[12]] <- drawRectangleLine(i, 8, 2, positions, 
                      tag, color, width)
                  }
                  c <- vector("list", 8)
                  c[[1]] <- drawRectangleCorner(i, 1, 3, 4, 5, 
                    positions, tag, color, width + 2)
                  c[[2]] <- drawRectangleCorner(i, 8, 4, 3, 2, 
                    positions, tag, color, width + 2)
                  c[[3]] <- drawRectangleCorner(i, 4, 1, 8, 7, 
                    positions, tag, color, width + 2)
                  c[[4]] <- drawRectangleCorner(i, 3, 8, 1, 6, 
                    positions, tag, color, width + 2)
                  if (!is.null(transformation)) {
                    c[[5]] <- drawRectangleCorner(i, 5, 6, 7, 
                      1, positions, tag, color, width + 2)
                    c[[6]] <- drawRectangleCorner(i, 2, 7, 6, 
                      8, positions, tag, color, width + 2)
                    c[[7]] <- drawRectangleCorner(i, 7, 5, 2, 
                      4, positions, tag, color, width + 2)
                    c[[8]] <- drawRectangleCorner(i, 6, 2, 5, 
                      3, positions, tag, color, width + 2)
                  }
                  return(list(Lines = l, Corners = c))
                }
                "drawBar" <- function(i, positions, tag, box = FALSE, 
                  color = "black", width = 1) {
                  l <- vector("list", 4)
                  l[[1]] <- drawRectangleLine(i, 1, 3, positions, 
                    tag, color, width)
                  l[[2]] <- drawRectangleLine(i, 4, 8, positions, 
                    tag, color, width)
                  l[[3]] <- drawRectangleLine(i, 1, 4, positions, 
                    tag, color, width)
                  l[[4]] <- drawRectangleLine(i, 3, 8, positions, 
                    tag, color, width)
                  return(l)
                }
                tag <- getTag("block", i, setTag = setTag)
                positions <- retBlockPoints(i)
                posA <- positions[1, ]
                posB <- positions[8, ]
                popupitems <- NULL
                blockcanvas <- NULL
                if (drawBlockBackground) 
                  if (is.null(transformation)) {
                    blockcanvas <- tkcreate(canvas, "rectangle", 
                      posA[1], posA[2], posB[1], posB[2], fill = color(block))
                    tkaddtag(canvas, tag, "withtag", blockcanvas)
                    popupitems <- append(popupitems, list(blockcanvas))
                  }
                if (drawBlockFrame) 
                  Rectangle <- drawRectangle(i, positions, tag, 
                    color = color, width = 1)
                else Rectangle <- NULL
                txt <- blockLabels[i]
                positions <- retBlockPoints(i, header = TRUE, 
                  box = box, n = nchar(txt))
                if (drawBlockFrame) {
                  Bar <- drawBar(i, positions, tag, box = box, 
                    color = color, width = 2)
                  popupitems <- append(popupitems, Bar)
                }
                else Bar <- NULL
                posA <- retBlockPos(i, 1)
                pos <- posA + c(8, 4, 0)
                label <- tkcreate(canvas, "text", pos[1], pos[2], 
                  text = txt, anchor = "nw", font = font.block, 
                  activefill = "DarkSlateGray")
                setOpenBlockItem(i, list(tag = tag, rectangle = Rectangle, 
                  canvas = blockcanvas, bar = Bar, label = label, 
                  block = i))
                setNodePopup(canvas, blockList[[i]], tag, popupitems, 
                  label, i, "OpenBlock", UserMenus)
            }
            addNodePopups <- function(canvas, vertex, i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor"), nodePopupMenu, UserNodePopupItems) {
                label <- retVertexLabel(i, vertex.type)
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(nodePopupMenu, "command", label = paste("Vertex", 
                    label, "(echo index)"), command = function() print(paste("Hej from vertex", 
                    label, "with index", i)))
                else if ((vertex.type == "OpenBlock") || (vertex.type == 
                  "ClosedBlock")) 
                  tkadd(nodePopupMenu, "command", label = paste("Block", 
                    label, "(echo index)"), command = function() print(paste("Hej from block", 
                    label, "with index", i)))
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(nodePopupMenu, "command", label = paste("Highlight for adding edge"), 
                    accelerator = "[ Click vertex ]", command = function() {
                      subActivateVertex(i, color = "green", vertex.type = vertex.type)
                      message("Click the other vertex")
                    })
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(nodePopupMenu, "command", label = paste("Highlight block for adding edges"), 
                    accelerator = "[ Click block ]", command = function() {
                      subActivateVertex(i, color = "green", vertex.type = "ClosedBlock")
                      message("Click vertex or block")
                    })
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(nodePopupMenu, "command", label = paste("Add edge after highlighting with selecting class"), 
                    command = newEdge(i, vertex.type = "Vertex", 
                      slave = FALSE, selectClass = TRUE))
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(nodePopupMenu, "command", label = paste("Add edge after highlighting (Here: Slave view!)"), 
                    accelerator = "[ Click vertex ]", command = newEdge(i, 
                      vertex.type = "Vertex", slave = TRUE))
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(nodePopupMenu, "command", label = paste("Adds edges from/to block after highlight (Here: Slaves!)"), 
                    accelerator = "[ Click block ]", command = newEdge(i, 
                      vertex.type = "ClosedBlock", slave = TRUE))
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) {
                }
                else if ((vertex.type == "OpenBlock")) {
                  tkadd(nodePopupMenu, "command", label = paste("Mark vertices of block"), 
                    command = function() {
                      markVerticesOfBlock(i, descendants = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste(" --- and descendants"), 
                    command = function() {
                      markVerticesOfBlock(i, slave = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste("Undisplay frames of block"), 
                    command = function() {
                      undisplayBlock(i, descendants = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste(" --- and descendants"), 
                    command = function() {
                      undisplayBlock(i, slave = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste("'Delete' (close) block"), 
                    command = function() {
                      removeBlock(i, descendants = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste(" --- and descendants"), 
                    command = function() {
                      removeBlock(i, slave = FALSE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste("New sub block"), 
                    accelerator = "(~ <F4>)", command = function() {
                      position <- position(blockList[[i]])
                      position <- apply(position, 1, mean)
                      position <- matrix(c(position - 10, position + 
                        10), nrow = 2, byrow = TRUE)
                      new.Block(position, get.name = TRUE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste("Minimize (shade) block"), 
                    accelerator = "[ Double click block head ]", 
                    command = function() {
                      closeBlock(i)()
                    })
                  tkadd(nodePopupMenu, "command", label = paste("Maximize block"), 
                    command = function() {
                      zoomPositions <- positionsBlocks[i, , ]
                      zoomPositions[, 1] <- zoomPositions[, 1] - 
                        2
                      zoomPositions[, 2] <- zoomPositions[, 2] + 
                        2
                      zoomPositions <<- zoomPositions
                      subUpdateGraphWindow("Maximize", redrawVertices = TRUE, 
                        all.blockframes = TRUE)
                    })
                  tkadd(nodePopupMenu, "command", label = paste("Redraw full graph"), 
                    command = function() {
                      zoomPositions <<- NULL
                      subUpdateGraphWindow("Redraw", redrawVertices = TRUE, 
                        all.blockframes = TRUE)
                    })
                }
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(nodePopupMenu, "command", label = paste("Open block"), 
                    accelerator = "[ Double click minimized block ]", 
                    command = function() {
                      openBlock(i)()
                    })
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(nodePopupMenu, "command", label = paste("Delete vertex (Here: Slave view!)"), 
                    command = function() subDropVertex(i, vertex.type = vertex.type, 
                      slave = TRUE))
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) {
                }
                propNodeMenu <- tkmenu(nodePopupMenu, tearoff = FALSE)
                tkadd(propNodeMenu, "command", label = paste("Open dialog box for slot values"), 
                  command = function() propertyNode(i, vertex.type = vertex.type)())
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Change label"), 
                    accelerator = "[ Double click label ]", command = changeVertexLabel(i, 
                      vertex.type = vertex.type))
                else if ((vertex.type == "OpenBlock")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Change label of block"), 
                    command = changeVertexLabel(i, vertex.type = "ClosedBlock"))
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Change label"), 
                    accelerator = "[ Double click label of minimized block ]", 
                    command = changeVertexLabel(i, vertex.type = "ClosedBlock"))
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Delete vertex label"), 
                    command = deleteVertexLabel(i, vertex.type = "Vertex"))
                else if ((vertex.type == "OpenBlock")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Delete label of block"), 
                    command = deleteVertexLabel(i, vertex.type = "ClosedBlock"))
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(propNodeMenu, "command", label = paste("/ Delete label of block"), 
                    command = deleteVertexLabel(i, vertex.type = "ClosedBlock"))
                tkadd(nodePopupMenu, "cascade", label = "Properties", 
                  menu = propNodeMenu)
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) {
                }
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) {
                }
                helpNodeMenu <- tkmenu(nodePopupMenu, tearoff = FALSE)
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(helpNodeMenu, "command", label = paste(" - Delete vertex"), 
                    accelerator = "[ Double click vertex ]", 
                    command = function() {
                    })
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) {
                }
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag vertex: Move vertex"), 
                    command = function() {
                    })
                else if ((vertex.type == "OpenBlock")) {
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag block head:    Move block"), 
                    command = function() {
                    })
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag block corner:  Resize block"), 
                    command = function() {
                    })
                }
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag block:  Move minimized block"), 
                    command = function() {
                    })
                if ((vertex.type == "Vertex") || (vertex.type == 
                  "Factor")) 
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag label:    Move vertex label"), 
                    command = function() {
                    })
                else if ((vertex.type == "OpenBlock")) {
                }
                else if ((vertex.type == "ClosedBlock")) 
                  tkadd(helpNodeMenu, "command", label = paste(" - Drag label:   Move label of minimized block"), 
                    command = function() {
                    })
                tkadd(nodePopupMenu, "cascade", label = "Help on node", 
                  menu = helpNodeMenu)
                methNodeMenu <- tkmenu(nodePopupMenu, tearoff = FALSE)
                if (hasMethod("addToPopups", class(vertex))) 
                  addToPopups(vertex, vertex.type, methNodeMenu, 
                    i, updateArguments, Args)
                tkadd(nodePopupMenu, "cascade", label = "Items by method 'addToPopups'", 
                  menu = methNodeMenu)
                userNodeMenu <- tkmenu(nodePopupMenu, tearoff = FALSE)
                NodePopup <- function(UserNodePopupItems, item, 
                  vertex.type) {
                  force(UserNodePopupItems)
                  force(item)
                  force(vertex.type)
                  function(...) {
                    updateArguments(UserNodePopupItems[[item]], 
                      blocks = TRUE)
                    UserNodePopupItems[[item]]$command(object, 
                      retVertexName(i, vertex.type), type = vertex.type, 
                      index = i, Arguments = Args())
                  }
                }
                if (length(UserNodePopupItems) > 0) 
                  for (item in seq(along = UserNodePopupItems)) if ((names(UserNodePopupItems[item]) == 
                    vertex.type)) 
                    tkadd(userNodeMenu, "command", label = UserNodePopupItems[[item]]$label, 
                      command = NodePopup(UserNodePopupItems, 
                        item, vertex.type))
                tkadd(nodePopupMenu, "cascade", label = "User defined items", 
                  menu = userNodeMenu)
            }
            setNodePopup <- function(canvas, vertex, tag, result, 
                label, i, vertex.type, UserNodePopupItems) {
                f <- function(item, i, label = FALSE) {
                  tkitembind(canvas, item, "<Leave>", function() tkconfigure(canvas, 
                    cursor = "arrow"))
                  if (label) {
                    if (vertex.type == "ClosedBlock") 
                      tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                        cursor = "diamond_cross"))
                    else tkitembind(canvas, item, "<Enter>", 
                      function() tkconfigure(canvas, cursor = "hand2"))
                  }
                  else if (vertex.type == "ClosedBlock") 
                    tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                      cursor = "cross"))
                  else tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                    cursor = "crosshair"))
                  if (label) 
                    tkitembind(canvas, item, "<Button-1>", newEdge(i, 
                      vertex.type, slave = FALSE))
                  else tkitembind(canvas, item, "<Button-1>", 
                    newEdge(i, vertex.type, slave = FALSE))
                  tkitembind(canvas, item, "<Button-2>", function(...) {
                    print("--2--")
                  })
                  tkitembind(canvas, item, "<Up>", function(...) {
                    function(...) print("--UP%--")
                  })
                  tkitembind(canvas, item, "<Down>", function(...) {
                    print("--Down%--")
                  })
                  tkitembind(canvas, item, "<Left>", function(...) {
                    print("--Left%--")
                  })
                  tkitembind(canvas, item, "<Right>", function(...) {
                    print("--Right%--")
                  })
                  tkitembind(canvas, item, "<Home>", function(...) {
                    print("--PgUp%--")
                  })
                  tkitembind(canvas, item, "<End>", function(...) {
                    print("--PgDn%--")
                  })
                  tkitembind(canvas, item, "<Delete>", function(...) {
                    print("--Delete%--")
                  })
                  tkitembind(canvas, item, "<F1>", function(...) {
                    print("--F1%--")
                  })
                  tkitembind(canvas, item, "<Alt-1>", function(...) {
                    print("--A1%--")
                  })
                  tkitembind(canvas, item, "<Alt_L>", function(...) {
                    print("--A%--")
                  })
                  tkitembind(canvas, item, "<Option-1>", activateVertex(i, 
                    vertex.type, hit.type = "option-1", color = "DarkGreen"))
                  tkitembind(canvas, item, "<Shift-1>", activateVertex(i, 
                    vertex.type, hit.type = "shift-1", color = "DarkGreen"))
                  tkitembind(canvas, item, "<Control-1>", activateVertex(i, 
                    vertex.type, hit.type = "control-1", color = "SeaGreen"))
                  tkitembind(canvas, item, "<Shift-Control-1>", 
                    activateVertex(i, vertex.type, hit.type = "shift-control-1", 
                      color = "LightSeaGreen"))
                  tkitembind(canvas, item, "<Option-3>", activateVertex(i, 
                    vertex.type, hit.type = "option-3", color = "LightGreen"))
                  tkitembind(canvas, item, "<Shift-3>", activateVertex(i, 
                    vertex.type, hit.type = "shift-3", color = "LightGreen"))
                  tkitembind(canvas, item, "<Control-3>", activateVertex(i, 
                    vertex.type, hit.type = "control-3", color = "SpringGreen"))
                  tkitembind(canvas, item, "<Shift-Control-3>", 
                    activateVertex(i, vertex.type, hit.type = "shift-control-3", 
                      color = "LimeGreen"))
                  if (label) 
                    tkitembind(canvas, item, "<B1-Motion>", moveVertexLabel(i, 
                      vertex.type))
                  else tkitembind(canvas, item, "<B1-Motion>", 
                    moveVertex(i, vertex.type))
                  if (label) 
                    tkitembind(canvas, item, "<Double-Button-1>", 
                      changeVertexLabel(i, vertex.type))
                  else if (vertex.type == "ClosedBlock") 
                    tkitembind(canvas, item, "<Double-Button-1>", 
                      openBlock(i))
                  else tkitembind(canvas, item, "<Double-Button-1>", 
                    undisplayVertex(i, vertex.type, slave = FALSE))
                  if (label) 
                    tkitembind(canvas, item, "<Triple-Button-1>", 
                      deleteVertexLabel(i, vertex.type))
                  tkitembind(canvas, item, "<Button-3>", callPopup(i, 
                    nodePopupMenu))
                  tkaddtag(canvas, tag, "withtag", item)
                }
                blockitembind <- function(item, label = FALSE) {
                  tkitembind(canvas, item, "<Leave>", function() tkconfigure(canvas, 
                    cursor = "arrow"))
                  if (label) 
                    tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                      cursor = "left_ptr"))
                  else tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                    cursor = "right_ptr"))
                  tkitembind(canvas, item, "<B1-Motion>", moveBlock(i, 
                    1))
                  tkitembind(canvas, item, "<Double-Button-1>", 
                    closeBlock(i))
                  tkitembind(canvas, item, "<Button-3>", callPopup(i, 
                    nodePopupMenu))
                  tkaddtag(canvas, tag, "withtag", item)
                }
                nodePopupMenu <- tkmenu(canvas, tearoff = FALSE)
                addNodePopups(canvas, vertex, i, vertex.type, 
                  nodePopupMenu, UserNodePopupItems)
                if (vertex.type == "OpenBlock") {
                  blockitembind(label, label = TRUE)
                  if (!is.null(result)) 
                    if (length(result) > 0) 
                      for (k in seq(length(result))) blockitembind(result[[k]])
                }
                else {
                  f(label, i, TRUE)
                  if (!is.null(result$dynamic)) 
                    if (length(result$dynamic) > 0) 
                      for (k in seq(length(result$dynamic))) f(result$dynamic[[k]], 
                        i, FALSE)
                  if (!is.null(result$fixed)) 
                    if (length(result$fixed) > 0) 
                      for (k in seq(length(result$fixed))) f(result$fixed[[k]], 
                        i, FALSE)
                }
            }
            subDrawVertex <- function(vertex, i, w = w, vertexcolor = vertexcolor, 
                vertex.type = ifelse(i > 0, "Vertex", "Factor"), 
                setTag = TRUE) {
                tag <- getTag(vertex.type, i, setTag = setTag)
                pos <- retVertexPos(i, vertex.type)
                if (hasMethod("draw", class(vertex))) 
                  dot <- draw(vertex, canvas, pos, x = pos[1], 
                    y = pos[2], stratum = retStratum(i, vertex.type = vertex.type), 
                    w = w * Scale, color = vertexcolor, background = background)
                else {
                  s <- w * sqrt(4/pi) * Scale
                  p <- tkcreate(canvas, "oval", pos[1] - s, pos[2] - 
                    s, pos[1] + s, pos[2] + s, fill = vertexcolor, 
                    activefill = "OrangeRed")
                  dot <- list(dynamic = list(p), fixed = NULL)
                }
                label <- tkcreate(canvas, "text", pos[1] + w, 
                  pos[2], text = retVertexLabel(i, vertex.type), 
                  anchor = "nw", font = font.vertex.label, activefill = "DarkSlateGray")
                if (debug.strata && (vertex.type != "Factor") && 
                  (vertex.type != "Extra")) {
                  strata <- retStratum(i, vertex.type)
                  block <- retBlockIndex(i, vertex.type)
                  color <- myColor(strata)
                  numbers <- tkcreate(canvas, "text", pos[1] - 
                    4 * w, pos[2] - 4 * w, text = paste(i, strata, 
                    block, sep = "."), fill = color, anchor = "nw", 
                    font = "12x30", activefill = "DarkSlateGray")
                  tkaddtag(canvas, tag, "withtag", numbers)
                }
                else numbers <- NULL
                if (vertex.type != "OpenBlock") 
                  setNodePopup(canvas, vertex, tag, dot, label, 
                    i, vertex.type, UserMenus)
                else setNodePopup(canvas, vertex, tag, dot, label, 
                  i, vertex.type, UserMenus)
                return(list(tag = tag, dot = dot, label = label, 
                  numbers = numbers))
            }
            drawVertex <- function(i, w = w, vertexcolor = vertexcolor, 
                vertex.type = ifelse(i > 0, "Vertex", "Factor"), 
                setTag = TRUE) {
                if (vertex.type == "ClosedBlock") 
                  itemsClosedBlocks[[i]] <<- subDrawVertex(blockList[[i]], 
                    i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type, 
                    setTag = setTag)
                else if (vertex.type == "Vertex") 
                  itemsVertices[[i]] <<- subDrawVertex(vertexList[[i]], 
                    i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type, 
                    setTag = setTag)
                else if (vertex.type == "Factor") 
                  itemsFactors[[-i]] <<- subDrawVertex(factorVertexList[[-i]], 
                    i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type, 
                    setTag = setTag)
                else if (vertex.type == "Extra") 
                  itemsExtras[[i]] <<- subDrawVertex(extraList[[i]], 
                    i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type, 
                    setTag = setTag)
            }
            clearSelectedVertices <- function() {
                if (length(selectedNodes) > 0) {
                  lapply(selectedNodes, function(k) setVertexColor(k$index, 
                    color = "red", vertex.type = k$node.type))
                  selectedNodes <<- list()
                }
            }
            setActivatedVertex <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor"), hit.type = "none") {
                if (hit.type == "none") 
                  activatedNode <<- list(number = i, vertex.type = vertex.type)
                if (!((i == 0) && (hit.type == "none"))) {
                  a <- list(index = i, node.type = vertex.type, 
                    hit.type = hit.type)
                  if (!any(unlist(lapply(selectedNodes, function(i) all(unlist(i) == 
                    unlist(a)))))) 
                    selectedNodes <<- append(list(a), selectedNodes)
                }
            }
            retActivatedVertex <- function() return(activatedNode[[1]])
            retActivatedVertexVertex.Type <- function() return(activatedNode[[2]])
            deActivateVertex <- function(i, color = retVertexColor(i, 
                vertex.type), vertex.type = ifelse(i > 0, "Vertex", 
                "Factor"), new.edge = FALSE) {
                if (length(selectedNodes) > 0) {
                  same <- (retActivatedVertex() == i)
                  x <- lapply(selectedNodes, function(k) if ((k$index == 
                    i) && (k$node.type == vertex.type) && ((new.edge && 
                    (k$hit.type == "none")) || (!new.edge) || 
                    same)) {
                    setVertexColor(i, color = color, vertex.type = vertex.type)
                    return(TRUE)
                  }
                  else return(FALSE))
                  selectedNodes <<- selectedNodes[!unlist(x)]
                }
                if ((retActivatedVertex() == i) && (retActivatedVertexVertex.Type() == 
                  vertex.type)) {
                  setActivatedVertex(0, "Null")
                  setVertexColor(i, color = color, vertex.type = vertex.type)
                  return(TRUE)
                }
                else return(FALSE)
            }
            subActivateVertex <- function(i, color = "green", 
                vertex.type = ifelse(i > 0, "Vertex", "Factor"), 
                hit.type = "none", new.edge = FALSE) {
                if (!(hit.type == "none")) {
                  setActivatedVertex(i, vertex.type, hit.type = hit.type)
                  setVertexColor(i, color = color, vertex.type = vertex.type)
                  return(TRUE)
                }
                else if (!deActivateVertex(i, "cyan", vertex.type, 
                  new.edge = new.edge)) 
                  if ((retActivatedVertex() == 0) && (vertex.type != 
                    "Extra")) {
                    setActivatedVertex(i, vertex.type)
                    setVertexColor(i, color = color, vertex.type = vertex.type)
                    return(TRUE)
                  }
                  else return(FALSE)
                else return(TRUE)
            }
            activateVertex <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor"), color = "green", hit.type = "none") {
                force(i)
                force(vertex.type)
                force(hit.type)
                force(color)
                function(...) subActivateVertex(i, color = color, 
                  vertex.type = vertex.type, hit.type = hit.type)
            }
            clearSelectedEdges <- function() {
                if (length(selectedEdges) > 0) {
                  lapply(selectedEdges, function(k) setEdgeColor(k$index, 
                    color = NULL, edge.type = k$edge.type))
                  selectedEdges <<- list()
                }
            }
            setActivatedEdge <- function(i, from = -1, to = -1, 
                edge.type = ifelse(i > 0, "Edge", "Factor"), 
                hit.type = "none") {
                if (hit.type == "none") 
                  activatedEdge <<- list(number = i, edge.type = edge.type)
                if (!((i == 0) && (hit.type == "none"))) {
                  a <- list(index = i, from = from, to = to, 
                    edge.type = edge.type, hit.type = hit.type)
                  if (!any(unlist(lapply(selectedEdges, function(i) all(unlist(i) == 
                    unlist(a)))))) 
                    selectedEdges <<- append(list(a), selectedEdges)
                }
            }
            subActivateEdge <- function(i, from = -1, to = -1, 
                edge.type = "VertexEdge", color = "green", hit.type = "none") {
                if ((hit.type != "none")) {
                  setActivatedEdge(i, from = from, to = to, edge.type, 
                    hit.type = hit.type)
                  if (i > 0) 
                    setEdgeColor(i, edge.type = edge.type, color)
                }
                else {
                  if (activatedEdge$number > 0) {
                    setEdgeColor(activatedEdge$number, edge.type = activatedEdge$edge.type, 
                      color = NULL)
                    if ((i != 0) && (length(selectedEdges) > 
                      0)) {
                      same <- (retActivatedEdge() == i)
                      new.edge <- TRUE
                      x <- lapply(selectedEdges, function(k) if ((k$hit.type == 
                        "none")) {
                        if (k$index != 0) 
                          setEdgeColor(k$index, color = NULL, 
                            edge.type = k$edge.type)
                        return(TRUE)
                      }
                      else return(FALSE))
                      assign("x", x, pos = 1)
                      selectedEdges <<- selectedEdges[!unlist(x)]
                    }
                  }
                  setActivatedEdge(i, from = from, to = to, edge.type, 
                    hit.type = hit.type)
                  if (i > 0) 
                    setEdgeColor(i, edge.type = edge.type, "Green")
                }
            }
            activateEdge <- function(i, from = -1, to = -1, edge.type = "VertexEdge", 
                color = "green", hit.type = "none") {
                force(i)
                force(from)
                force(to)
                force(edge.type)
                force(hit.type)
                force(color)
                function(...) subActivateEdge(i, from = from, 
                  to = to, color = color, edge.type = edge.type, 
                  hit.type = hit.type)
            }
            retActivatedEdge <- function(edge.type = "VertexEdge") return(activatedEdge$number)
            updateSelectedEdges <- function(R, vertexEdges, edge.type = "VertexEdge", 
                setVertex = FALSE, updateBE = FALSE) {
                if (updateBE) {
                  updateBlockEdges()
                  blockEdgesUpdate()
                }
                setModel(R$object, R, "updateSelectedEdges", 
                  edgeList = vertexEdges, factorVertexList = R$FactorVertices, 
                  factorEdgeList = R$FactorEdges, visible.Vertices = R$VisibleVertices, 
                  visible.Blocks = R$VisibleBlocks, extraList = R$ExtraList, 
                  extraEdgeList = R$ExtraEdges)
                activateEdge(0, edge.type = NULL)()
                if (setVertex) 
                  setActivatedVertex(0, "Vertex")
                clearSelectedVertices()
                clearSelectedEdges()
                clearFactorEdges()
                clearExtraEdges()
                update.edge.labels()
                if (!is.null(R$FactorVertices) && !is.null(R$FactorEdges)) 
                  drawFactors(R$FactorEdges, R$FactorVertices)
            }
            drawResult <- function(newEdges, R, slave, txt, Arguments = Args()) {
                Edges <- extractEdgesResult(R, newEdges, TRUE, 
                  txt)
                if (is.null(Edges)) 
                  Edges <- .emptyDgList("dg.VertexEdgeList")
                if (slave) {
                  drawModel(frameModels = Frame.Models, frameViews = Frame.Views, 
                    graphWindow = NULL, edgeList = Edges, oriented = oriented, 
                    factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                    visibleVertices = R$VisibleVertices, visibleBlocks = R$VisibleBlocks, 
                    blockEdgeList = R$BlockEdges, extraList = R$ExtraVertices, 
                    extraEdgeList = R$ExtraEdges, object = R$object, 
                    viewType = viewType, title = "Default", Arguments = Arguments)
                }
                else {
                  setModel(R$object, R, txt, edgeList = Edges, 
                    blockEdgeList = R$BlockEdges, factorVertexList = R$FactorVertices, 
                    factorEdgeList = R$FactorEdges, visible.Vertices = R$VisibleVertices, 
                    visible.Blocks = R$VisibleBlocks, extraList = R$ExtraList, 
                    extraEdgeList = R$ExtraEdges)
                  redrawView(frameModels = Frame.Models, frameViews = Frame.Views, 
                    graphWindow = GraphWindow, edgeList = Edges, 
                    factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                    visibleVertices = R$VisibleVertices, visibleBlocks = R$VisibleBlocks, 
                    blockEdgeList = R$BlockEdges, extraList = R$ExtraVertices, 
                    extraEdgeList = R$ExtraEdges, viewType = viewType, 
                    title = title, Arguments = Arguments)
                }
            }
            subUpdateGraphWindow <- function(txt = "", redrawVertices = FALSE, 
                raiseEdges = FALSE, updateEdges = FALSE, all.blockframes = FALSE, 
                blockframes = NULL) {
                pos <- NULL
                update.edges <- TRUE
                if (!.IsEmpty(blockList)) 
                  for (i in seq(along = blockList)) {
                    if ((closedBlock[i] || hiddenBlock[i]) && 
                      (i %in% visibleBlocks)) {
                      if (all(positionsClosedBlocks[i, ] < rep(-100, 
                        N))) {
                        deleteBlock(i)
                        visible.Blocks <- returnVisibleBlocks()
                        visible.Blocks <- visible.Blocks[visible.Blocks != 
                          i]
                        setVisibleBlocks(visible.Blocks)
                        openBlock(i, update = FALSE)()
                      }
                      else {
                        pos <- retVertexPos(i, "ClosedBlock")
                        update.edges <- updateEdges || is.element(i, 
                          blockframes)
                        if (!is.null(itemsClosedBlocks[[i]])) {
                          pos <- retVertexPos(i, "ClosedBlock")
                          xy <- tkcoords(canvas, itemsClosedBlocks[[i]]$tag)
                          xy <- apply(matrix(as.numeric(xy), 
                            ncol = 2, byrow = 2), 2, mean)
                          if (!any(is.nan(xy)) && (length(xy) > 
                            0)) {
                            dxy <- findDifference(pos, c(xy, 
                              rep(0, N - 2)))
                            l <- sum(dxy[1:2]^2)
                            if (is.numeric(l)) {
                              if (l > 0) 
                                update.edges <- TRUE
                            }
                            else warning(paste("Invalid length: ", 
                              l))
                            tkmove(canvas, itemsClosedBlocks[[i]]$tag, 
                              dxy[1], dxy[2])
                          }
                          posLabel <- pos + retBlockLabelPos(i)
                          tkcoords(canvas, itemsClosedBlocks[[i]]$label, 
                            posLabel[1], posLabel[2])
                          tkitemconfigure(canvas, itemsClosedBlocks[[i]]$label, 
                            text = retVertexLabel(i, vertex.type = "ClosedBlock"))
                        }
                      }
                      if (!is.null(pos) && !is.null(blockEdgeList)) 
                        if ((update.edges || raiseEdges) && (length(itemsBlockEdges[[i]]) > 
                          0)) 
                          for (e in itemsBlockEdges[[i]]) if (!(is.null(e))) 
                            if (TRUE) {
                              from.type <- vertexTypeOfEdge(-i, 
                                e$type)
                              to.type <- vertexTypeOfEdge(e$to, 
                                e$type)
                              posTo <- retVertexPos(e$to, to.type)
                              setEdgeCoords(e, pos, posTo, i, 
                                e$to, from.type, to.type)
                              l <- sqrt(sum((posTo - pos)^2))
                              if (l < 1) 
                                setEdgeLabel(e, " ", e$label.number, 
                                  f = i, permanent = FALSE)
                              else {
                                posLabel <- (pos + posTo)/2 + 
                                  retEdgeLabelPos(e$label.number, 
                                    i, e$to)
                                tkcoords(canvas, e$label, posLabel[1], 
                                  posLabel[2])
                              }
                            }
                    }
                    else if (all.blockframes || (is.element(i, 
                      blockframes))) {
                      if (all(positionsClosedBlocks[i, ] < rep(-100, 
                        N))) {
                        deleteBlock(i)
                      }
                      else {
                        tkitemconfigure(canvas, itemsOpenBlocks[[i]]$label, 
                          text = retVertexLabel(i, vertex.type = "ClosedBlock"))
                        tkcoordsBlock(i, lower = FALSE)
                      }
                    }
                  }
                if (!is.null(itemsExtras)) 
                  for (i in seq(along = itemsExtras)) if (!is.null(itemsExtras[[i]]) && 
                    !is.null(itemsExtras[[i]][[1]])) {
                    if (redrawVertices) {
                      tkdelete(canvas, vertexItem(i, vertex.type = "Extra")$tag)
                      drawVertex(i, w = w, vertexcolor = vertexColor, 
                        vertex.type = "Extra", setTag = FALSE)
                      vertexColor <- retVertexColor(i, vertex.type = "Extra")
                      setVertexColor(i, color = vertexColor, 
                        vertex.type = "Extra")
                    }
                    else {
                      pos <- retVertexPos(i, "Extra")
                      xy <- tkcoords(canvas, itemsExtras[[i]]$tag)
                      xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                        byrow = 2), 2, mean)
                      if (!any(is.nan(xy)) && (length(xy) > 0)) {
                        dxy <- findDifference(pos, c(xy, rep(0, 
                          N - 2)))
                        tkmove(canvas, itemsExtras[[i]]$tag, 
                          dxy[1], dxy[2])
                      }
                      posLabel <- retLabelPos(i, vertex.type = "Extra")
                      xyl <- as.numeric(tkcoords(canvas, itemsExtras[[i]]$l))
                      tkitemconfigure(canvas, itemsExtras[[i]]$l, 
                        text = retVertexLabel(i, vertex.type = "Extra"))
                      vertexColor <- retVertexColor(i, vertex.type = "Extra")
                      setVertexColor(i, color = vertexColor, 
                        vertex.type = "Extra")
                      if (!any(is.nan(xyl)) && (length(xyl) > 
                        0)) {
                        dxy <- findDifference(posLabel, c(xyl, 
                          rep(0, N - 2)))
                        tkmove(canvas, itemsExtras[[i]]$l, dxy[1], 
                          dxy[2])
                      }
                    }
                    if (!is.null(pos) && (update.edges || raiseEdges) && 
                      (length(itemsExtraEdges[[i]]) > 0)) 
                      for (e in itemsExtraEdges[[i]]) if (!(is.null(e))) 
                        if (TRUE) {
                          ii <- -i
                          from.type <- vertexTypeOfEdge(ii, e$type)
                          to.type <- vertexTypeOfEdge(e$to, e$type)
                          pos.To <- retVertexPos(e$to, to.type)
                          setEdgeCoords(e, pos, pos.To, ii, e$to, 
                            from.type, to.type)
                          l <- sqrt(sum((pos.To - pos)^2))
                          if (l < 1) 
                            setEdgeLabel(e, " ", e$label.number, 
                              f = ii, edge.type = "ExtraEdge", 
                              permanent = FALSE)
                          else {
                            for (l in 1:length(e$edges)) tkitemraise(canvas, 
                              e$edges[[l]])
                            if (length(e$tags) > 0) 
                              for (l in 1:length(e$tags)) tkitemraise(canvas, 
                                e$tags[[l]])
                            tkitemraise(canvas, e$label)
                            posLabel <- (pos + pos.To)/2 + retEdgeLabelPos(e$label.number, 
                              ii, e$to)
                            display <- displayEdge(e, f = ii, 
                              e$to)
                            if (display) {
                              label <- retEdgeLabel(e, e$label.number, 
                                f = ii)
                              setEdgeLabel(e, label, e$label.number, 
                                f = ii, edge.type = "ExtraEdge", 
                                permanent = FALSE)
                            }
                            tkcoords(canvas, e$label, posLabel[1], 
                              posLabel[2])
                          }
                        }
                  }
                if (!is.null(itemsFactors)) 
                  for (i in seq(along = itemsFactors)) if (!is.null(itemsFactors[[i]]) && 
                    !is.null(itemsFactors[[i]][[1]])) {
                    vertex.indices <- factorVertexList[[i]]@vertex.indices
                    setFactorVertexPosition(i, vertex.indices)
                    if (redrawVertices) {
                      tkdelete(canvas, vertexItem(-i, vertex.type = "Factor")$tag)
                      drawVertex(-i, w = w, vertexcolor = vertexColor, 
                        vertex.type = "Factor", setTag = FALSE)
                      vertexColor <- retVertexColor(-i, vertex.type = "Factor")
                      setVertexColor(-i, color = vertexColor, 
                        vertex.type = "Factor")
                    }
                    else {
                      pos <- retVertexPos(-i, "Factor")
                      xy <- tkcoords(canvas, itemsFactors[[i]]$tag)
                      xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                        byrow = 2), 2, mean)
                      if (!any(is.nan(xy)) && (length(xy) > 0)) {
                        dxy <- findDifference(pos, c(xy, rep(0, 
                          N - 2)))
                        tkmove(canvas, itemsFactors[[i]]$tag, 
                          dxy[1], dxy[2])
                      }
                      posLabel <- retLabelPos(-i, vertex.type = "Factor")
                      xyl <- as.numeric(tkcoords(canvas, itemsFactors[[i]]$l))
                      tkitemconfigure(canvas, itemsFactors[[i]]$l, 
                        text = retVertexLabel(-i, vertex.type = "Factor"))
                      vertexColor <- retVertexColor(-i, vertex.type = "Factor")
                      setVertexColor(-i, color = vertexColor, 
                        vertex.type = "Factor")
                      if (!any(is.nan(xyl)) && (length(xyl) > 
                        0)) {
                        dxy <- findDifference(posLabel, c(xyl, 
                          rep(0, N - 2)))
                        tkmove(canvas, itemsFactors[[i]]$l, dxy[1], 
                          dxy[2])
                      }
                    }
                  }
                for (i in seq(along = itemsVertices)) if (!is.null(itemsVertices[[i]]) && 
                  !is.null(itemsVertices[[i]][[1]])) {
                  pos <- retVertexPos(i, "Vertex")
                  update.edges <- updateEdges || redrawVertices
                  if (!closedVertex[i]) {
                    if (redrawVertices) {
                      tkdelete(canvas, vertexItem(i)$tag)
                      drawVertex(i, w = w, vertexcolor = vertexColor, 
                        vertex.type = "Vertex", setTag = FALSE)
                      vertexColor <- retVertexColor(i, vertex.type = "Vertex")
                      setVertexColor(i, color = vertexColor, 
                        vertex.type = "Vertex")
                    }
                    else {
                      xy <- tkcoords(canvas, itemsVertices[[i]]$tag)
                      xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                        byrow = 2), 2, mean)
                      if (!(all(is.na(xy)))) {
                        if (!any(is.nan(xy)) && (length(xy) > 
                          0)) {
                          dxy <- findDifference(pos, c(xy, rep(0, 
                            N - 2)))
                          ll <- sum(dxy[1:2]^2)
                          if (is.numeric(ll)) {
                            if ((length(ll) > 0) && (ll > 0)) 
                              update.edges <- TRUE
                          }
                          else warning(paste("Invalid length: ", 
                            ll))
                          tkmove(canvas, itemsVertices[[i]]$tag, 
                            dxy[1], dxy[2])
                        }
                        posLabel <- retLabelPos(i, vertex.type = "Vertex")
                        xyl <- as.numeric(tkcoords(canvas, itemsVertices[[i]]$l))
                        tkitemconfigure(canvas, itemsVertices[[i]]$l, 
                          text = retVertexLabel(i, vertex.type = "Vertex"))
                        vertexColor <- retVertexColor(i, vertex.type = "Vertex")
                        setVertexColor(i, color = vertexColor, 
                          vertex.type = "Vertex")
                        if (!any(is.nan(xyl)) && (length(xyl) > 
                          0)) {
                          dxy <- findDifference(posLabel, c(xyl, 
                            rep(0, N - 2)))
                          tkmove(canvas, itemsVertices[[i]]$l, 
                            dxy[1], dxy[2])
                        }
                        if (debug.strata) {
                          strata <- retStratum(i, vertex.type = "Vertex")
                          block <- retBlockIndex(i, vertex.type = "Vertex")
                          color <- myColor(strata)
                          tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                            text = paste(i, strata, block, sep = "."))
                          tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                            fill = color)
                        }
                      }
                    }
                  }
                  else update.edges <- TRUE
                  if (!is.null(pos) && (update.edges || raiseEdges) && 
                    (length(itemsEdges[[i]]) > 0)) 
                    for (e in itemsEdges[[i]]) if (!(is.null(e))) 
                      if (TRUE) {
                        from.type <- vertexTypeOfEdge(i, e$type)
                        to.type <- vertexTypeOfEdge(e$to, e$type)
                        pos.To <- retVertexPos(e$to, to.type)
                        setEdgeCoords(e, pos, pos.To, i, e$to, 
                          from.type, to.type)
                        l <- sqrt(sum((pos.To - pos)^2))
                        if (l < 1) 
                          setEdgeLabel(e, " ", e$label.number, 
                            f = i, permanent = FALSE)
                        else {
                          for (l in 1:length(e$edges)) tkitemraise(canvas, 
                            e$edges[[l]])
                          if (length(e$tags) > 0) 
                            for (l in 1:length(e$tags)) tkitemraise(canvas, 
                              e$tags[[l]])
                          tkitemraise(canvas, e$label)
                          posLabel <- (pos + pos.To)/2 + retEdgeLabelPos(e$label.number, 
                            i, e$to)
                          display <- displayEdge(e, f = i, e$to)
                          if (display) {
                            label <- retEdgeLabel(e, e$label.number, 
                              f = i)
                            setEdgeLabel(e, label, e$label.number, 
                              f = i, permanent = FALSE)
                          }
                          tkcoords(canvas, e$label, posLabel[1], 
                            posLabel[2])
                        }
                      }
                }
            }
            setUpdateVertices <- function(txt = "") {
                updateCountVerticesMain <<- updateCountVerticesMain + 
                  1
                updateCountVertices <<- updateCountVerticesMain
            }
            setUpdatePositions <- function(txt = "") {
                updateCountPositionsMain <<- updateCountPositionsMain + 
                  1
                updateCountPositions <<- updateCountPositionsMain
            }
            setUpdateBlocks <- function(txt = "") {
                updateCountBlocksMain <<- updateCountBlocksMain + 
                  1
                updateCountBlocks <<- updateCountBlocksMain
            }
            setUpdateBlockEdges <- function(txt = "", local = TRUE) {
                updateCountBlockEdgesMain <<- updateCountBlockEdgesMain + 
                  1
                if (!local) 
                  updateCountBlockEdges <<- updateCountBlockEdgesMain
            }
            setUpdateAll <- function(txt = "") {
                updateCountVerticesMain <<- updateCountVerticesMain + 
                  1
                updateCountPositionsMain <<- updateCountPositionsMain + 
                  1
                updateCountBlocksMain <<- updateCountBlocksMain + 
                  1
                updateCountBlockEdgesMain <<- updateCountBlockEdgesMain + 
                  1
                updateCountVertices <<- updateCountVerticesMain
                updateCountPositions <<- updateCountPositionsMain
                updateCountBlocks <<- updateCountBlocksMain
                updateCountBlockEdges <<- updateCountBlockEdgesMain
            }
            subUpdatePositions <- function(txt = "") {
                testUpdateModel()
                n <- length(vertexList)
                m <- length(itemsVertices)
                if (n > m) 
                  for (i in seq(n - m)) {
                    closedVertex <<- c(closedVertex, FALSE)
                    itemsVertices <<- append(itemsVertices, list(NULL))
                    itemsEdges <<- append(itemsEdges, list(NULL))
                  }
                n <- length(blockList)
                m <- length(itemsClosedBlocks)
                if (n > m) {
                  for (i in seq(n - m)) {
                    itemsBlockEdges <<- append(itemsBlockEdges, 
                      list(NULL))
                    itemsClosedBlocks <<- append(itemsClosedBlocks, 
                      list(NULL))
                    itemsOpenBlocks <<- append(itemsOpenBlocks, 
                      list(NULL))
                    closedBlock <<- c(closedBlock, FALSE)
                    hiddenBlock <<- c(hiddenBlock, FALSE)
                    visibleBlocks <- unique(sort(c(visibleBlocks, 
                      m + i)))
                    visibleBlocks <<- visibleBlocks[visibleBlocks != 
                      0]
                  }
                  Arguments <- Args()
                  redrawView(frameModels = Frame.Models, frameViews = Frame.Views, 
                    graphWindow = GraphWindow, Arguments = Arguments)
                }
                all.blockframes <- updateCountBlocks < updateCountBlocksMain
                updateEdges <- updateCountBlockEdges < updateCountBlockEdgesMain
                updateVertices <- updateCountVertices < updateCountVerticesMain
                updatePositions <- updateCountPositions < updateCountPositionsMain
                if (updateCountBlockEdges < updateCountBlockEdgesMain) 
                  updateAllBlockIndices()
                if (updatePositions || updateVertices || updateEdges || 
                  all.blockframes) 
                  subUpdateGraphWindow(txt, updateEdges = updateEdges, 
                    redrawVertices = updateVertices, all.blockframes = all.blockframes)
                if (updateEdges) 
                  updateBlockEdges()
                updateCountVertices <<- updateCountVerticesMain
                updateCountPositions <<- updateCountPositionsMain
                updateCountBlocks <<- updateCountBlocksMain
                updateCountBlockEdges <<- updateCountBlockEdgesMain
            }
            updatePositions <- function(txt = "") {
                force(txt)
                function(...) {
                  subUpdatePositions(txt)
                }
            }
            tkdeleteRectangleCorner <- function(line) {
                tkdelete(canvas, line[[1]])
                tkdelete(canvas, line[[2]])
                if (!is.null(transformation)) 
                  tkdelete(canvas, line[[3]])
            }
            tkdeleteRectangle <- function(rectangle) {
                line <- rectangle$Lines
                tkdelete(canvas, line[[1]])
                tkdelete(canvas, line[[2]])
                tkdelete(canvas, line[[3]])
                tkdelete(canvas, line[[4]])
                if (!is.null(transformation)) {
                  tkdelete(canvas, line[[5]])
                  tkdelete(canvas, line[[6]])
                  tkdelete(canvas, line[[7]])
                  tkdelete(canvas, line[[8]])
                  tkdelete(canvas, line[[9]])
                  tkdelete(canvas, line[[10]])
                  tkdelete(canvas, line[[11]])
                  tkdelete(canvas, line[[12]])
                }
                corner <- rectangle$Corners
                tkdeleteRectangleCorner(corner[[1]])
                tkdeleteRectangleCorner(corner[[2]])
                tkdeleteRectangleCorner(corner[[3]])
                tkdeleteRectangleCorner(corner[[4]])
                if (!is.null(transformation)) {
                  tkdeleteRectangleCorner(corner[[5]])
                  tkdeleteRectangleCorner(corner[[6]])
                  tkdeleteRectangleCorner(corner[[7]])
                  tkdeleteRectangleCorner(corner[[8]])
                }
            }
            tkdeleteBar <- function(line) {
                tkdelete(canvas, line[[1]])
                tkdelete(canvas, line[[2]])
                tkdelete(canvas, line[[3]])
                tkdelete(canvas, line[[4]])
            }
            deleteBlock <- function(i) {
                tkdeleteRectangle(openBlockItem(i)$rectangle)
                tkdeleteBar(openBlockItem(i)$bar)
                tkdelete(canvas, openBlockItem(i)$label)
                if (!is.null(openBlockItem(i)$canvas)) 
                  tkdelete(canvas, openBlockItem(i)$canvas)
            }
            subSubDeleteEdge <- function(i, f, t, edge.type = "VertexEdge") {
                E <- getEdges(edge.type = edge.type)[[i]]
                delete <- function(k, edges) {
                  edges <- edgeItem(k, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if ((e$nr == i) && (e$type == edge.type)) 
                        if (e$to > k) {
                          if (debug.edges) {
                            cat("Tkdelete, subSubDeleteEdge: ")
                            for (l in 1:length(e$edges)) cat(paste(e$edges[[l]], 
                              " "))
                            for (l in 1:length(e$tags)) cat(paste(e$tags[[l]], 
                              " "))
                            cat(paste(e$label))
                            cat("\n")
                          }
                          for (l in 1:length(e$edges)) tkdelete(canvas, 
                            e$edges[[l]])
                          for (l in 1:length(e$tags)) tkdelete(canvas, 
                            e$tags[[l]])
                          tkdelete(canvas, e$label)
                        }
                }
                for (j in E@vertex.indices) delete(j, edgeItem(j, 
                  edge.type = edge.type))
                remove <- function(k, edges) if (length(edges) > 
                  0) {
                  result <- NULL
                  for (e in edges) if (!(is.null(e))) 
                    if (!((e$nr == i) && (e$type == edge.type))) 
                      result <- c(result, list(e))
                  if (is.null(result)) 
                    setEdgeItem(k, edge.type = edge.type, list(NULL))
                  else setEdgeItem(k, edge.type = edge.type, 
                    result)
                }
                for (j in E@vertex.indices) remove(j, edgeItem(j, 
                  edge.type = edge.type))
            }
            subSubUndisplayFactorVertex <- function(i, edge.type = "FactorEdge") {
                edges <- edgeItem(i, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) {
                    subSubDeleteEdge(e$nr, i, e$to, edge.type = edge.type)
                    clearEdge(e$nr, edge.type = edge.type)
                  }
                visible.Vertices <- returnVisibleVertices()
                visible.Vertices <- visible.Vertices[visible.Vertices != 
                  i]
                setVisibleVertices(visible.Vertices)
                tkdelete(canvas, vertexItem(i)$tag)
                setVertexItem(i, list(NULL))
            }
            clearFactorEdges <- function() {
                for (f in seq(along = itemsFactors)) subSubUndisplayFactorVertex(-f)
            }
            clearExtraEdges <- function() {
            }
            setVisibleVertices <- function(i) {
                visibleVertices <<- i
                GraphWindow@visibleVertices <<- visibleVertices
            }
            returnVisibleVertices <- function() {
                return(visibleVertices)
            }
            setVisibleBlocks <- function(i) {
                visibleBlocks <<- i
                GraphWindow@visibleBlocks <<- visibleBlocks
            }
            returnVisibleBlocks <- function() {
                return(visibleBlocks)
            }
            subSubUndisplayVertex <- function(i, edge.type = "VertexEdge") {
                edges <- edgeItem(i, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) 
                    if ((e$type == edge.type)) {
                      subSubDeleteEdge(e$nr, i, e$to, edge.type = edge.type)
                      clearEdge(e$nr, edge.type = edge.type)
                    }
                visible.Vertices <- returnVisibleVertices()
                visible.Vertices <- visible.Vertices[visible.Vertices != 
                  i]
                setVisibleVertices(visible.Vertices)
                tkdelete(canvas, vertexItem(i)$tag)
                setVertexItem(i, list(NULL))
            }
            update.edge.labels <- function() {
                subUpdateEdgeLabels <- function(itemsNodes, edge.type = "VertexEdge") for (f in seq(along = itemsNodes)) if (!is.null(itemsNodes[[f]]) && 
                  !is.null(itemsNodes[[f]][[1]])) {
                  if (edge.type != "VertexEdge") 
                    f <- -f
                  edges <- edgeItem(f, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if (e$to < f) {
                        display <- displayEdge(e, f, e$to)
                        if (namesOnEdges && display) {
                          vertexnames <- c(retVertexName(f, vertexTypeOfEdge(f, 
                            e$type)), retVertexName(e$to, vertexTypeOfEdge(e$to, 
                            e$type)))
                          if (e$reverse) 
                            vertexnames <- rev(vertexnames)
                          label <- paste(vertexnames, collapse = "~")
                        }
                        else label <- ""
                        setEdgeLabel(e, label, e$label.number, 
                          f = f, permanent = FALSE)
                        setEdgeWidth(e, 2, e$label.number, f = f)
                      }
                }
                if (updateEdgeLabels) {
                  subUpdateEdgeLabels(itemsVertices, edge.type = "VertexEdge")
                  subUpdateEdgeLabels(itemsFactors, edge.type = "FactorEdge")
                  subUpdateEdgeLabels(itemsExtras, edge.type = "ExtraEdge")
                  subUpdateEdgeLabels(itemsClosedBlocks, edge.type = "BlockEdge")
                }
            }
            updateBlockEdges <- function() {
                Edges <- selectCurrentEdges(omitEdges = FALSE, 
                  edge.type = "VertexEdge")
                edge.list <- lapply(Edges, function(i) i@vertex.indices)
                verticesUpdate()
                NewBlockEdges <- returnBlockEdgeList(edge.list, 
                  vertexList, blockList, color = blockEdgeColor, 
                  visibleBlocks = visibleBlocks, oriented = oriented)
                new.list <- lapply(NewBlockEdges, function(i) {
                  x <- i@vertex.indices
                  names(x) <- NULL
                  x
                })
                old.list <- lapply(getEdges(edge.type = "BlockEdge"), 
                  function(i) {
                    x <- i@vertex.indices
                    names(x) <- NULL
                    x
                  })
                match.old.new <- match(old.list, new.list)
                for (i in seq(along = match.old.new)) if (is.na(match.old.new[i])) 
                  if (all(abs(old.list[[i]]) > 0)) {
                    subSubDeleteEdge(i, old.list[[i]][1], old.list[[i]][2], 
                      edge.type = "BlockEdge")
                    clearEdge(i, edge.type = "BlockEdge")
                  }
                match.new.old <- match(new.list, old.list)
                for (i in seq(along = match.new.old)) if (is.na(match.new.old[i])) {
                  E <- append.edge(NewBlockEdges[[i]], edge.type = "BlockEdge")
                  drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                    edge.type = "BlockEdge")
                }
            }
            deleteAllEdgeLabels <- function() {
                function(...) {
                  g <- function(items, edge.type = "VertexEdge") for (f in seq(along = items)) if (TRUE || 
                    !is.null(items[[f]]) && !is.null(items[[f]][[1]])) {
                    ff <- ifelse(edge.type == "VertexEdge", f, 
                      -f)
                    if (debug.edges) 
                      cat(paste(edge.type, f, ff, ": "))
                    edges <- edgeItem(ff, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) {
                        if (debug.edges) 
                          cat(paste(" ", e$to))
                        if ((e$to < f) || ((ff < 0))) {
                          setEdgeLabel(e, "", e$label.number, 
                            f = ff, permanent = TRUE)
                          setEdgeWidth(e, 2, e$label.number, 
                            f = ff)
                        }
                      }
                    if (debug.edges) 
                      cat("\n")
                  }
                  g(itemsVertices, edge.type = "VertexEdge")
                  g(itemsClosedBlocks, edge.type = "BlockEdge")
                  g(itemsFactors, edge.type = "FactorEdge")
                  g(itemsExtras, edge.type = "ExtraEdge")
                }
            }
            moveEdgesToVertex <- function(X, v, edge.type = "VertexEdge") {
                edges <- edgeItem(v, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) {
                    from.type <- vertexTypeOfEdge(v, e$type)
                    to.type <- vertexTypeOfEdge(e$to, e$type)
                    posTo <- retVertexPos(e$to, to.type)
                    setEdgeCoords(e, X, posTo, v, e$to, from.type, 
                      to.type)
                    pos <- (X + posTo)/2 + retEdgeLabelPos(e$label.number, 
                      v, e$to)
                    tkcoords(canvas, e$label, pos[1], pos[2])
                  }
            }
            moveVerticesInBlock <- function(i, dxy, move.vertices = TRUE) {
                for (v in seq(along = vertexList)) if (is.element(v, 
                  visibleVertices)) {
                  blockIndex <- retBlockIndex(v, vertex.type = "Vertex")
                  if ((blockIndex > 0) && (blockReferences[blockIndex] == 
                    i)) {
                    if (move.vertices) {
                      if (!closedVertex[v]) 
                        tkmove(canvas, vertexItem(v)$tag, dxy[1], 
                          dxy[2])
                      changeVertexPos(v, dxy)
                    }
                    pos <- retVertexPos(v, "Vertex")
                    moveEdgesToVertex(pos, v, edge.type = "VertexEdge")
                  }
                }
            }
            subMoveVertex <- function(i, vertex.type, posFrom, 
                posTo) {
                dxy <- findDifference(posTo, posFrom)
                tag <- vertexItem(i, vertex.type)$tag
                tkmove(canvas, tag, dxy[1], dxy[2])
                tkitemraise(canvas, tag)
                if (debug.strata && (vertex.type != "Factor") && 
                  (vertex.type != "Extra") && (vertex.type != 
                  "ClosedBlock")) {
                  strata <- retStratum(i, vertex.type)
                  block <- retBlockIndex(i, vertex.type)
                  color <- myColor(strata)
                  tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                    text = paste(i, strata, block, sep = "."))
                  tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                    fill = color)
                }
                setVertexPos(i, posTo, dxy, vertex.type)
                if (vertex.type == "ClosedBlock") {
                  moveVerticesInBlock(i, dxy, move.vertices = FALSE)
                  moveEdgesToVertex(posTo, -i, edge.type = "BlockEdge")
                }
                else if (vertex.type == "Vertex") {
                  if (closedVertex[i]) 
                    posTo <- retVertexPos(i, "Vertex")
                  moveEdgesToVertex(posTo, i, edge.type = "VertexEdge")
                }
                else if (vertex.type == "Extra") {
                  posTo <- retVertexPos(i, vertex.type)
                  moveEdgesToVertex(posTo, -i, edge.type = "ExtraEdge")
                }
                else if (vertex.type == "Factor") {
                  moveEdgesToVertex(posTo, i, edge.type = "FactorEdge")
                }
                else {
                  posTo <- retVertexPos(i, vertex.type)
                  moveEdgesToVertex(posTo, i, edge.type = "VertexEdge")
                }
            }
            setFactorVertexPosition <- function(i, vertex.indices) {
                posFrom <- retVertexPos(-i, "Factor")
                positions <- NULL
                for (j in seq(along = vertex.indices)) if (vertex.indices[j] > 
                  0) 
                  positions <- rbind(positions, positionsVertices[vertex.indices[j], 
                    ])
                position <- apply(positions, 2, mean)
                posTo <- positionsCanvas(project(position))
                subMoveVertex(-i, "Factor", posFrom, posTo)
            }
            moveFactorVertex <- function(vertex) {
                if (!is.null(itemsFactors)) 
                  for (i in seq(along = itemsFactors)) if (!is.null(itemsFactors[[i]]) && 
                    !is.null(itemsFactors[[i]][[1]])) {
                    vertex.indices <- factorVertexList[[i]]@vertex.indices
                    if (is.element(vertex, vertex.indices)) 
                      setFactorVertexPosition(i, vertex.indices)
                  }
            }
            moveVertex <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                force(i)
                force(vertex.type)
                function(x, y) {
                  deActivateVertex(i, retVertexColor(i, vertex.type), 
                    vertex.type)
                  posFrom <- retVertexPos(i, vertex.type)
                  posTo <- replaceXY(x, y, posFrom)
                  subMoveVertex(i, vertex.type, posFrom, posTo)
                  moveFactorVertex(i)
                  setUpdatePositions("")
                }
            }
            moveVertexLabel <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                force(i)
                force(vertex.type)
                function(x, y) {
                  deActivateVertex(i, retVertexColor(i, vertex.type), 
                    vertex.type)
                  if (vertex.type == "ClosedBlock") {
                    posFrom <- retVertexPos(i, vertex.type) + 
                      retBlockLabelPos(i)
                    X <- replaceXY(x, y, posFrom)
                    dxy <- findDifference(X, posFrom)
                    tkcoords(canvas, vertexItem(i, vertex.type)$label, 
                      x, y)
                    setLabelPos(i, X, dxy, vertex.type)
                  }
                  else {
                    posFrom <- retLabelPos(i, vertex.type)
                    X <- replaceXY(x, y, posFrom)
                    dxy <- findDifference(X, posFrom)
                    tkmove(canvas, vertexItem(i, vertex.type)$label, 
                      dxy[1], dxy[2])
                    setLabelPos(i, X, dxy, vertex.type)
                  }
                  setUpdatePositions("")
                }
            }
            changeVertexLabel <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                force(i)
                force(vertex.type)
                function(...) {
                  ReturnVal <- modalDialog("Label Entry", "Enter new label", 
                    retVertexLabel(i, vertex.type), top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  setVertexLabel(i, ReturnVal, vertex.type)
                  setUpdatePositions("")
                }
            }
            deleteVertexLabel <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                force(i)
                force(vertex.type)
                function(...) {
                  setVertexLabel(i, "", vertex.type)
                  setUpdatePositions("")
                }
            }
            changeEdgeLabel <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(...) {
                  if (retActivatedEdge() == i) {
                    edges <- edgeItem(f, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) 
                        if (e$nr == i) 
                          if (e$to == t) {
                            label <- paste(edge.names(i, edge.type = edge.type), 
                              collapse = "%")
                            ReturnVal <- modalDialog("Edge label Entry", 
                              "Enter new label", label, top = GraphWindow.top[[1]])
                            if (ReturnVal == "ID_CANCEL") 
                              return()
                            setEdgeLabel(e, ReturnVal, e$label.number, 
                              f = f, permanent = TRUE)
                          }
                  }
                }
            }
            subDropVertex <- function(i, vertex.type = "Vertex", 
                slave = TRUE) {
                tkconfigure(canvas, cursor = "watch")
                tkfocus(GraphWindow.top[[1]])
                redraw <- FALSE
                if (!(viewType == "Simple")) 
                  redraw <- TRUE
                vertexEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                  edge.type = "VertexEdge"), edge.type = "VertexEdge", 
                  copyProperties = FALSE)
                factorEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                  edge.type = "FactorEdge"), edge.type = "FactorEdge", 
                  copyProperties = FALSE)
                extraEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                  edge.type = "ExtraEdge"), edge.type = "ExtraEdge", 
                  copyProperties = FALSE)
                blockEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                  edge.type = "BlockEdge"), edge.type = "BlockEdge", 
                  copyProperties = FALSE)
                if (vertex.type == "selected") {
                  newEdges <- list(vertexEdges = vertexEdges, 
                    factorEdges = factorEdges, extraEdges = extraEdges, 
                    blockEdges = blockEdges)
                }
                else if (vertex.type == "Vertex") {
                  newEdges <- list(vertexEdges = vertexEdges, 
                    factorEdges = factorEdges, extraEdges = extraEdges, 
                    blockEdges = blockEdges)
                }
                else if (vertex.type == "Factor") 
                  newEdges <- list(vertexEdges = vertexEdges, 
                    factorEdges = factorEdges, extraEdges = NULL, 
                    blockEdges = NULL)
                else if (vertex.type == "Extra") 
                  newEdges <- list(vertexEdges = vertexEdges, 
                    factorEdges = NULL, extraEdges = extraEdges, 
                    blockEdges = NULL)
                else if (vertex.type == "closedBlock") 
                  newEdges <- list(vertexEdges = vertexEdges, 
                    factorEdges = NULL, extraEdges = NULL, blockEdges = NULL)
                if (vertex.type == "selected") {
                  redraw <- TRUE
                  message("Selected vertices not added to 'newEdges';")
                  message("Resulting edges should be returned from modifyModel!")
                }
                R <- NULL
                if (is.null(object)) 
                  R <- TRUE
                Arguments <- Args()
                visible.Vertices <- returnVisibleVertices()
                if (i != 0) 
                  visible.Vertices <- visible.Vertices[visible.Vertices != 
                    i]
                if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                  class(object)))) {
                  if (i == 0) 
                    name <- ""
                  else name <- retVertexName(i, vertex.type)
                  R <- modifyModel(object, action = "dropVertex", 
                    name = name, index = i, type = vertex.type, 
                    newEdges = newEdges, visibleVertices = visible.Vertices, 
                    selectedNodes = selectedNodes, selectedEdges = selectedEdges, 
                    Arguments = Arguments)
                }
                if (!is.null(R)) {
                  objectAssign(R)
                  setVisibleVertices(visible.Vertices)
                  if (slave || redraw) 
                    drawResult(newEdges, R, slave, "dropVertex")
                  else {
                    if (i != 0) 
                      subSubUndisplayVertex(i)
                    updateSelectedEdges(R, vertexEdges, edge.type = NULL)
                  }
                }
                else message("Null result in dropVertex")
                updateBlockEdges()
                tkconfigure(canvas, cursor = "arrow")
            }
            undisplayVertex <- function(i, vertex.type = ifelse(i > 
                0, "Vertex", "Factor"), slave = TRUE) {
                force(i)
                force(slave)
                function(...) {
                  deActivateVertex(i, retVertexColor(i, vertex.type), 
                    vertex.type)
                  subDropVertex(i, vertex.type, slave)
                }
            }
            undisplayBlock <- function(i, descendants = TRUE, 
                slave = TRUE, update = TRUE) {
                subUndisplayBlock <- function(i) {
                  deleteBlock(i)
                  visible.Blocks <- returnVisibleBlocks()
                  visible.Blocks <- visible.Blocks[visible.Blocks != 
                    i]
                  setVisibleBlocks(visible.Blocks)
                }
                if (descendants) 
                  for (j in blockList[[i]]@descendants) if ((j != 
                    i) && (j != 0)) {
                    subUndisplayBlock(j)
                    if ((closedBlock[j] || hiddenBlock[j])) 
                      openBlock(j, update = FALSE)()
                  }
                subUndisplayBlock(i)
                if (update) 
                  subUpdateGraphWindow("undisplayBlock", raiseEdges = TRUE, 
                    updateEdges = TRUE)
            }
            removeBlock <- function(i, descendants = TRUE, slave = TRUE, 
                update = TRUE) {
                subRemoveBlock <- function(i) {
                  deleteBlock(i)
                  visible.Blocks <- returnVisibleBlocks()
                  visible.Blocks <- visible.Blocks[visible.Blocks != 
                    i]
                  setVisibleBlocks(visible.Blocks)
                  positionsBlocks[i, , 1] <<- rep(-1000, N)
                  positionsBlocks[i, , 2] <<- rep(-1000, N) + 
                    1e-04
                  positionsClosedBlocks[i, ] <<- rep(-1000, N)
                }
                if (descendants) 
                  for (j in blockList[[i]]@descendants) if ((j != 
                    i) && (j != 0)) {
                    subRemoveBlock(j)
                    if ((closedBlock[j] || hiddenBlock[j])) 
                      openBlock(j, update = FALSE)()
                  }
                subRemoveBlock(i)
                V <- verticesUpdate()
                if (updateAllBlockIndices() || TRUE) 
                  setUpdateBlockEdges("removeBlock")
                subUpdateGraphWindow("removeBlock", redrawVertices = TRUE, 
                  raiseEdges = TRUE, updateEdges = TRUE, all.blockframes = TRUE)
                setUpdateAll("removeBlock")
            }
            markVerticesOfBlock <- function(i, descendants = TRUE, 
                slave = TRUE, update = TRUE) {
                subMarkVerticesOfBlock <- function(i) {
                  subActivateVertex(i, "ClosedBlock", hit.type = "Mark-2", 
                    color = "GreenYellow")
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) {
                    bi <- retBlockIndex(v, vertex.type = "Vertex")
                    if ((bi == i)) {
                      subActivateVertex(v, "Vertex", hit.type = "Mark-2", 
                        color = "GreenYellow")
                      vertices <<- c(vertices, v)
                    }
                  }
                }
                vertices <- numeric(0)
                if (descendants) 
                  for (j in blockList[[i]]@descendants) if ((j != 
                    i) && (j != 0)) {
                    subMarkVerticesOfBlock(j)
                  }
                subMarkVerticesOfBlock(i)
                return(vertices)
            }
            moveEdge <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(x, y) {
                  activateEdge(0, from = f, to = t, edge.type = edge.type)()
                  from.type <- vertexTypeOfEdge(f, edge.type)
                  to.type <- vertexTypeOfEdge(t, edge.type)
                  posFrom <- retVertexPos(f, from.type)
                  posTo <- retVertexPos(t, to.type)
                  pos <- (posFrom + posTo)/2
                  X <- replaceXY(x, y, pos)
                  dxy <- findDifference(X, pos)
                  if (sum(dxy^2) < 0.25^2 * sum((posFrom - posTo)^2)) {
                    fun <- function(ii, vertex.type, position) {
                      if (vertex.type == "ClosedBlock") {
                        if ((closedBlock[-ii]) || hiddenBlock[-ii]) 
                          tkmove(canvas, vertexItem(-ii, vertex.type = vertex.type)$tag, 
                            dxy[1], dxy[2])
                        setVertexPos(-ii, position + dxy, dxy, 
                          vertex.type = vertex.type)
                      }
                      else if (vertex.type == "Extra") {
                        tkmove(canvas, vertexItem(ii, vertex.type = vertex.type)$tag, 
                          dxy[1], dxy[2])
                        setVertexPos(ii, position + dxy, dxy, 
                          vertex.type = vertex.type)
                      }
                      else if (!closedVertex[ii]) {
                        tkmove(canvas, vertexItem(ii)$tag, dxy[1], 
                          dxy[2])
                        setVertexPos(ii, position + dxy, dxy, 
                          vertex.type = vertex.type)
                      }
                      edges <- edgeItem(ii, edge.type = edge.type)
                      if (length(edges) > 0) 
                        for (e in edges) if (!(is.null(e))) {
                          from.type <- vertexTypeOfEdge(ii, e$type)
                          to.type <- vertexTypeOfEdge(e$to, e$type)
                          pos <- retVertexPos(e$to, to.type)
                          setEdgeCoords(e, position, pos, ii, 
                            e$to, from.type, to.type)
                          pos <- (pos + position)/2 + retEdgeLabelPos(e$label.number, 
                            ii, e$to)
                          tkcoords(canvas, e$label, pos[1], pos[2])
                        }
                    }
                    fun(f, from.type, posFrom)
                    fun(t, to.type, posTo)
                  }
                  setUpdatePositions("")
                }
            }
            moveEdgeLabel <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(x, y) {
                  activateEdge(0, from = f, to = t, edge.type = edge.type)()
                  edges <- edgeItem(f, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if (e$nr == i) 
                        if ((e$to == t)) 
                          if (!(e$reverse) || !oriented) {
                            from.type <- vertexTypeOfEdge(f, 
                              edge.type)
                            to.type <- vertexTypeOfEdge(t, edge.type)
                            pos <- (retVertexPos(t, to.type) + 
                              retVertexPos(f, from.type))/2
                            pos <- pos + retEdgeLabelPos(e$label.number, 
                              f, e$to)
                            X <- replaceXY(x, y, pos)
                            dxy <- findDifference(X, pos)
                            tkcoords(canvas, e$label, x, y)
                            setEdgeLabelPos(e, e$label.number, 
                              X, dxy, f = f)
                          }
                }
            }
            subSubAddEdge <- function(f, t, from.type, to.type, 
                edge.type = "VertexEdge", slave = TRUE, edgeClass = NULL) {
                fun <- function(result, ff, tt, edge.type = "VertexEdge") {
                  if (!any(which.edge(c(ff, tt), edge.type = edge.type)) && 
                    (!any(which.edge(c(tt, ff), edge.type = edge.type)) || 
                      oriented)) 
                    return(append(result, list(c(ff, tt))))
                  else return(result)
                }
                redraw <- FALSE
                if (!(viewType == "Simple")) 
                  redraw <- TRUE
                result <- NULL
                if (edge.type == "selected") {
                  redraw <- TRUE
                  message("Selected edges not added to 'newEdges';")
                  message("Resulting edges should be returned from modifyModel!")
                }
                else if (edge.type == "VertexEdge") {
                  result <- fun(result, f, t, edge.type = "VertexEdge")
                }
                else if (edge.type == "extraEdge") {
                  message("No edges to extra vertices!")
                }
                else if (edge.type == "FactorEdge") {
                  message("Resulting edges should be returned from modifyModel!")
                  redraw <- TRUE
                }
                else if (edge.type == "ExtraEdge") {
                  message("Resulting edges should be returned from modifyModel!")
                  redraw <- TRUE
                }
                else if (edge.type == "factorBlockEdge") {
                  message("Resulting edges should be returned from modifyModel!")
                  message("Factoredges are only draw between vertices and factors!")
                  redraw <- TRUE
                }
                else if (edge.type == "BlockEdge") {
                  from.block <- f
                  from.block <- unique(sort(c(from.block, blockList[[from.block]]@descendants)))
                  from.block <- from.block[from.block != 0]
                  to.block <- t
                  to.block <- unique(sort(c(to.block, blockList[[to.block]]@descendants)))
                  to.block <- to.block[to.block != 0]
                  if (from.type == "Vertex") {
                    for (w in seq(along = vertexList)) if (is.element(w, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(w, vertex.type = "Vertex"), 
                        to.block)) 
                        result <- fun(result, f, w, edge.type = "VertexEdge")
                  }
                  else if (to.type == "Vertex") {
                    for (v in seq(along = vertexList)) if (is.element(v, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(v, vertex.type = "Vertex"), 
                        from.block)) 
                        result <- fun(result, v, t, edge.type = "VertexEdge")
                  }
                  else {
                    for (v in seq(along = vertexList)) if (is.element(v, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(v, vertex.type = "Vertex"), 
                        from.block)) 
                        for (w in seq(along = vertexList)) if (is.element(w, 
                          visibleVertices)) 
                          if (is.element(retBlockIndex(w, vertex.type = "Vertex"), 
                            to.block)) 
                            result <- fun(result, v, w, edge.type = "VertexEdge")
                  }
                }
                if (redraw || !is.null(result) || (edge.type == 
                  "selected")) {
                  vertexEdges <- appendToCurrentEdges(omitEdges = FALSE, 
                    new.edge = result, edge.type = "VertexEdge", 
                    edgeClass = edgeClass)
                  newEdges <- list(vertexEdges = vertexEdges, 
                    extraEdges = NULL, factorEdges = NULL, blockEdges = NULL)
                  R <- NULL
                  if (is.null(object)) 
                    R <- TRUE
                  Arguments <- Args()
                  if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                    class(object)))) {
                    if (f == 0) 
                      name.f <- ""
                    else name.f <- retVertexName(f, from.type)
                    if (t == 0) 
                      name.t <- ""
                    else name.t <- retVertexName(t, to.type)
                    R <- modifyModel(object, action = "addEdge", 
                      name.1 = name.f, name.2 = name.t, from = f, 
                      to = t, from.type = from.type, to.type = to.type, 
                      newEdges = newEdges, selectedNodes = selectedNodes, 
                      selectedEdges = selectedEdges, Arguments = Arguments)
                  }
                  if (!is.null(R)) {
                    objectAssign(R)
                    if (slave || redraw) 
                      drawResult(newEdges, R, slave, "addEdge")
                    else {
                      for (i in seq(along = result)) {
                        E <- append.index.edge(result[[i]], edge.type = "VertexEdge", 
                          edgeClass = edgeClass)
                        drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                          edge.type = "VertexEdge")
                      }
                      updateSelectedEdges(R, vertexEdges, edge.type = edge.type, 
                        setVertex = TRUE, updateBE = TRUE)
                    }
                  }
                  else message("Null result in addEdge")
                }
            }
            subAddEdge <- function(f, t, from.type, to.type, 
                edge.type = "VertexEdge", slave = TRUE, edgeClass = NULL) {
                tkconfigure(canvas, cursor = "watch")
                tkfocus(GraphWindow.top[[1]])
                if ((from.type != "Extra") && (to.type != "Extra")) 
                  subSubAddEdge(f, t, from.type, to.type, edge.type = edge.type, 
                    slave = slave, edgeClass = edgeClass)
                if (f != 0) 
                  setVertexColor(f, color = retVertexColor(f, 
                    from.type), from.type)
                if (t != 0) 
                  setVertexColor(t, color = retVertexColor(t, 
                    to.type), to.type)
                setActivatedVertex(0, "Vertex")
                tkconfigure(canvas, cursor = "arrow")
            }
            "subNewBlock" <- function(position, get.name = TRUE, 
                color = blockColors[1]) {
                positionCenter <- apply(position, c(2), mean)
                n <- length(blockList) + 1
                if (get.name) {
                  label <- modalDialog("Name Entry", "Enter name of new block", 
                    paste("Block", n, sep = ""), top = GraphWindow.top[[1]])
                  if (label == "ID_CANCEL") 
                    return()
                }
                else label <- paste("B", n, sep = "")
                stratum <- n
                if (is.null(color)) 
                  if (is.null(blockList)) 
                    color <- "grey"
                  else color <- color(blockList[[1]])
                ancestors <- 0
                for (j in seq(along = blockList)) if (inBlock(positionCenter, 
                  j)) 
                  ancestors <- unique(sort(c(ancestors, j, ancestors(blockList[[j]]))))
                new.block <- newBlock(stratum = stratum, index = -stratum, 
                  position = position, color = color, label = label, 
                  ancestors = ancestors)
                blocksUpdate()
                for (j in seq(along = blockList)) if (j %in% 
                  ancestors) 
                  blockList[[j]]@descendants <<- c(blockList[[j]]@descendants, 
                    n)
                if (is.null(blockList)) {
                  blockList <<- list(new.block)
                  class(blockList) <<- "dg.BlockList"
                  Frame.Models@blocks <<- blockList
                  positionsBlocks <- Positions(blockList)
                  d <- dim(positionsBlocks)
                  positionsBlocks <<- array(positionsBlocks, 
                    dim = c(d[1], d[2]/2, 2))
                  positionsBlockLabels <<- matrix(labelPosition(new.block), 
                    nrow = 1)
                  positionsClosedBlocks <<- matrix(positionCenter, 
                    nrow = 1)
                  blockReferences <<- c(n)
                  blockLabels <<- c(label)
                  strataBlocks <<- c(n)
                  closedBlock <<- c(FALSE)
                  hiddenBlock <<- c(FALSE)
                  visibleBlocks <<- c(n)
                }
                else {
                  blockList <<- append(blockList, list(new.block))
                  class(blockList) <<- "dg.BlockList"
                  Frame.Models@blocks <<- blockList
                  d <- dim(positionsBlocks)
                  pBs <- array(positionsBlocks, dim = c(d[1], 
                    d[2] * 2))
                  pBs <- rbind(pBs, c(position(new.block)))
                  positionsBlocks <<- array(pBs, dim = c(d[1] + 
                    1, d[2], 2))
                  positionsBlockLabels <<- rbind(positionsBlockLabels, 
                    labelPosition(new.block))
                  positionsClosedBlocks <<- rbind(positionsClosedBlocks, 
                    positionCenter)
                  blockReferences <<- c(blockReferences, n)
                  blockLabels <<- c(blockLabels, label)
                  strataBlocks <<- c(strataBlocks, n)
                  closedBlock <<- c(closedBlock, FALSE)
                  hiddenBlock <<- c(hiddenBlock, FALSE)
                  visibleBlocks <<- c(visibleBlocks, n)
                }
            }
            new.Block <- function(position, get.name = FALSE) {
                tkconfigure(canvas, cursor = "watch")
                tkconfigure(GraphWindow.viewLabel[[1]], text = paste(viewType, 
                  " | Working !!!"))
                subNewBlock(position, get.name = get.name)
                n <- length(blockList)
                drawBlock(blockList[[n]], n)
                if (updateAllBlockIndices()) 
                  setUpdateBlockEdges("createNewBlock")
                subUpdateGraphWindow("createNewBlock", redrawVertices = TRUE, 
                  raiseEdges = TRUE, updateEdges = TRUE)
                setUpdateBlocks("")
                subUpdatePositions()
                tkconfigure(GraphWindow.viewLabel[[1]], text = viewType)
                tkconfigure(canvas, cursor = "arrow")
            }
            newBlockXY <- function() {
                function(x, y) {
                  X <- replaceXY(x, y, rep(50, N))
                  position <- c(inversProject(inversCanvasPosition(X)))
                  position <- matrix(c(position - 10, position + 
                    10), nrow = 2, byrow = TRUE)
                  new.Block(position, get.name = FALSE)
                }
            }
            newEdge <- function(i, vertex.type = ifelse(i > 0, 
                "Vertex", "Factor"), slave = TRUE, selectClass = FALSE) {
                force(i)
                force(slave)
                force(vertex.type)
                force(selectClass)
                function(...) {
                  edgeClass = NULL
                  if (selectClass) {
                    ReturnVal <- selectDialog("Select class of edge", 
                      "Select class", edgeClasses[, 1], top = GraphWindow.top[[1]])
                    if ((length(ReturnVal) > 0) && (ReturnVal != 
                      "ID_CANCEL")) 
                      edgeClass <- edgeClasses[ReturnVal, 1]
                  }
                  from.type <- retActivatedVertexVertex.Type()
                  f <- retActivatedVertex()
                  t <- i
                  if (!subActivateVertex(i, "green", vertex.type, 
                    new.edge = TRUE)) 
                    if ((f != 0) && !((f == i) && (from.type == 
                      vertex.type))) {
                      edge.type <- "VertexEdge"
                      if ((vertex.type == "Extra") || (from.type == 
                        "Extra")) 
                        edge.type <- "extraEdge"
                      if ((vertex.type == "Factor") || (from.type == 
                        "Factor")) 
                        if ((edge.type == "extraEdge")) 
                          edge.type <- "factorExtraEdge"
                        else edge.type <- "FactorEdge"
                      if ((vertex.type == "ClosedBlock") || (from.type == 
                        "ClosedBlock")) 
                        if ((edge.type == "extraEdge")) 
                          edge.type <- "blockExtraEdge"
                        else if ((edge.type == "FactorEdge")) 
                          edge.type <- "factorBlockEdge"
                        else edge.type <- "BlockEdge"
                      subAddEdge(f, t, from.type, vertex.type, 
                        edge.type = edge.type, slave = slave, 
                        edgeClass = edgeClass)
                    }
                }
            }
            addLastEdge <- function(vertex.type = ifelse(i > 
                0, "Vertex", "Factor")) {
                function() {
                  n <- length(vertexList)
                  f <- retActivatedVertex()
                  if ((f > 0) && (retActivatedVertexVertex.Type() == 
                    "Vertex")) 
                    setVertexColor(f, color = retVertexColor(i, 
                      vertex.type), vertex.type)
                  setActivatedVertex(n - 1, vertex.type)
                  newEdge(n, vertex.type = "Vertex", slave = FALSE)()
                }
            }
            subSubDropEdge <- function(i, f, t, from.type = vertexTypeOfEdge(f, 
                edge.type), to.type = vertexTypeOfEdge(t, edge.type), 
                edge.type = "VertexEdge", slave = TRUE) {
                redraw <- FALSE
                if (!(viewType == "Simple")) 
                  redraw <- TRUE
                j.g <- FALSE
                if (edge.type == "selected") {
                  redraw <- TRUE
                  message("Selected edges not added to 'newEdges';")
                  message("Resulting edges should be returned from modifyModel!")
                  vertexEdges <- copyCurrentEdges(edge.type = "VertexEdge")
                  newEdges <- list(vertexEdges = vertexEdges, 
                    extraEdges = NULL, factorEdges = NULL, blockEdges = NULL)
                }
                else if (edge.type == "VertexEdge") {
                  j.g <- which.unordered.edge(c(t, f), edge.type = edge.type)
                  vertexEdges <- copyCurrentEdges(omitEdges = j.g, 
                    edge.type = edge.type)
                  newEdges <- list(vertexEdges = vertexEdges, 
                    extraEdges = NULL, factorEdges = NULL, blockEdges = NULL)
                }
                else if (edge.type == "FactorEdge") {
                  message("Resulting edges should be returned from modifyModel!")
                  redraw <- TRUE
                  newEdges <- list(vertexEdges = NULL, extraEdges = NULL, 
                    factorEdges = NULL, blockEdges = NULL)
                }
                else if (edge.type == "ExtraEdge") {
                  message("Resulting edges should be returned from modifyModel!")
                  redraw <- TRUE
                  vertexEdges <- copyCurrentEdges(edge.type = "VertexEdge")
                  newEdges <- list(vertexEdges = NULL, extraEdges = NULL, 
                    factorEdges = NULL, blockEdges = NULL)
                }
                else if ((edge.type == "blockExtraEdge") || (edge.type == 
                  "factorExtraEdge") || (edge.type == "factorBlockEdge")) {
                  message("Not possible: Factoredges are only draw between vertices and factors!")
                  newEdges <- list(vertexEdges = NULL, extraEdges = NULL, 
                    factorEdges = NULL, blockEdges = NULL)
                }
                else if (edge.type == "BlockEdge") {
                  from.block <- -f
                  if (from.block > 0) 
                    from.block <- c(from.block, blockList[[from.block]]@descendants)
                  to.block <- -t
                  if (to.block > 0) 
                    to.block <- c(to.block, blockList[[to.block]]@descendants)
                  if (from.type == "Vertex") {
                    for (w in seq(along = vertexList)) if (is.element(w, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(w, vertex.type = "Vertex"), 
                        to.block)) 
                        j.g <- j.g | which.unordered.edge(c(f, 
                          w), edge.type = "VertexEdge")
                  }
                  else if (to.type == "Vertex") {
                    for (v in seq(along = vertexList)) if (is.element(v, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(v, vertex.type = "Vertex"), 
                        from.block)) 
                        j.g <- j.g | which.unordered.edge(c(v, 
                          t), edge.type = "VertexEdge")
                  }
                  else {
                    for (v in seq(along = vertexList)) if (is.element(v, 
                      visibleVertices)) 
                      if (is.element(retBlockIndex(v, vertex.type = "Vertex"), 
                        from.block)) 
                        for (w in seq(along = vertexList)) if (is.element(w, 
                          visibleVertices)) 
                          if (is.element(retBlockIndex(w, vertex.type = "Vertex"), 
                            to.block)) 
                            if (v != w) 
                              j.g <- j.g | which.unordered.edge(c(v, 
                                w), edge.type = "VertexEdge")
                  }
                  vertexEdges <- copyCurrentEdges(omitEdges = j.g, 
                    edge.type = "VertexEdge")
                  newEdges <- list(vertexEdges = vertexEdges, 
                    extraEdges = NULL, factorEdges = NULL, blockEdges = NULL)
                }
                R <- NULL
                if (is.null(object)) 
                  R <- TRUE
                Arguments <- Args()
                if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                  class(object)))) {
                  if (f == 0) 
                    name.f <- ""
                  else name.f <- retVertexName(f, from.type)
                  if (t == 0) 
                    name.t <- ""
                  else name.t <- retVertexName(t, to.type)
                  R <- modifyModel(object, action = "dropEdge", 
                    name.1 = name.f, name.2 = name.t, from = f, 
                    to = t, from.type = from.type, to.type = to.type, 
                    edge.index = i, newEdges = newEdges, selectedNodes = selectedNodes, 
                    selectedEdges = selectedEdges, Arguments = Arguments)
                }
                if (!is.null(R)) {
                  objectAssign(R)
                  if (slave || redraw) 
                    drawResult(newEdges, R, slave, "dropEdge")
                  else {
                    subSubDeleteEdge(i, f, t, edge.type = edge.type)
                    clearEdge(i, edge.type = edge.type)
                    if (edge.type == "BlockEdge") {
                      for (k in seq(along = j.g)) if (j.g[k]) {
                        subSubDeleteEdge(k, f, t, edge.type = "VertexEdge")
                        clearEdge(k, edge.type = "VertexEdge")
                      }
                    }
                    updateSelectedEdges(R, vertexEdges, edge.type = edge.type, 
                      setVertex = FALSE, updateBE = TRUE)
                  }
                }
                else message("Null result in dropEdge")
            }
            subDropEdge <- function(i, f, t, from.type = vertexTypeOfEdge(f, 
                edge.type), to.type = vertexTypeOfEdge(t, edge.type), 
                from.all = FALSE, to.all = FALSE, edge.type = "VertexEdge", 
                slave = TRUE) {
                tkconfigure(canvas, cursor = "watch")
                tkfocus(GraphWindow.top[[1]])
                if (from.type == "ClosedBlock") 
                  from.block <- f
                else from.block <- retBlockIndex(f, vertex.type = from.type)
                if (to.type == "ClosedBlock") 
                  to.block <- t
                else to.block <- retBlockIndex(t, vertex.type = to.type)
                if (from.all && ((from.type == "ClosedBlock") || 
                  ((from.type == "Vertex")))) {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if ((retBlockIndex(v, vertex.type = "Vertex") == 
                      from.block)) 
                      subDropEdge(NULL, v, t, "Vertex", to.type, 
                        FALSE, to.all, edge.type = edge.type, 
                        slave = slave)
                }
                else if (to.all && ((to.type == "ClosedBlock") || 
                  ((to.type == "Vertex")))) {
                  for (w in seq(along = vertexList)) if (is.element(w, 
                    visibleVertices)) 
                    if ((retBlockIndex(w, vertex.type = "Vertex") == 
                      to.block)) 
                      subDropEdge(NULL, f, w, "Vertex", "Vertex", 
                        FALSE, FALSE, edge.type = edge.type, 
                        slave = slave)
                }
                else {
                  j <- which.unordered.edge(c(t, f), edge.type = edge.type)
                  if (any(j)) {
                    i <- (1:(length(j)))[j]
                    for (j in seq(along = i)) subSubDropEdge(i[j], 
                      f, t, edge.type = edge.type, slave = slave)
                  }
                }
                tkconfigure(canvas, cursor = "arrow")
            }
            deleteEdge <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(...) {
                  if (retActivatedEdge() == i) 
                    subDropEdge(i, f, t, from.all = FALSE, to.all = FALSE, 
                      edge.type = edge.type, slave = FALSE)
                }
            }
            changeEdgeClass <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(...) {
                  ReturnVal <- selectDialog("Test selectDialog Entry", 
                    "Select name", edgeClasses[, 1], top = GraphWindow.top[[1]])
                  if ((length(ReturnVal) > 0) && (ReturnVal != 
                    "ID_CANCEL")) {
                    E <- getEdges(edge.type = edge.type)
                    newEdge <- E[[i]]
                    class(newEdge) <- edgeClasses[ReturnVal, 
                      2]
                    E <- append.edge(newEdge, edge.type = edge.type)
                    subSubDeleteEdge(i, f, t, edge.type = edge.type)
                    clearEdge(i, edge.type = edge.type)
                    drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                      edge.type = edge.type)
                    setModel(object, R = NULL, txt = "changeEdgeClass", 
                      copyProperties = TRUE)
                  }
                }
            }
            propertyEdge <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(...) {
                  E <- edgesUpdate()
                  E <- blockEdgesUpdate()
                  E <- factorEdgesUpdate()
                  E <- extraEdgesUpdate()
                  E <- getEdges(edge.type = edge.type)
                  fixedSlots <- c("vertex.indices", "label.position")
                  if (length(selectedEdges) > 0) 
                    fixedSlots <- c(fixedSlots, "label", "oriented")
                  ReturnVal <- propertyDialog(E[[i]], edgeClasses, 
                    fixedSlots = fixedSlots, top = GraphWindow.top[[1]])
                  if (!is.null(ReturnVal$values)) {
                    E[[i]] <- ReturnVal$object
                    subSubDeleteEdge(i, f, t, edge.type = edge.type)
                    drawEdge(E[[i]], i, lower = TRUE, edge.type = edge.type, 
                      newE = TRUE)
                    if (length(selectedEdges) > 0) {
                      lapply(selectedEdges, function(k) setEdgeColor(k$index, 
                        color = NULL, edge.type = k$edge.type))
                      if (!is.null(ReturnVal$values$oriented)) {
                        message("Change of 'oriented' not implemented; ")
                      }
                      if (!is.null(ReturnVal$values$dash)) 
                        lapply(selectedEdges, function(k) if (k$edge.type == 
                          edge.type) {
                          dash(E[[k$index]]) <<- ReturnVal$values$dash
                          setEdgeDash(k$index, dash = ReturnVal$values$dash, 
                            edge.type = k$edge.type)
                        })
                      if (!is.null(ReturnVal$values$vertex.indices)) {
                        message("Change of 'vertex.indices' not possible; ")
                      }
                      if (!is.null(ReturnVal$values$width)) 
                        lapply(selectedEdges, function(k) if (k$edge.type == 
                          edge.type) {
                          width(E[[k$index]]) <<- ReturnVal$values$width
                          edges <- edgeItem(k$from, edge.type = edge.type)
                          if (length(edges) > 0) 
                            for (e in edges) if (!(is.null(e))) 
                              if (e$nr == k$index) 
                                if (e$to == k$to) 
                                  setEdgeWidth(e, width = ReturnVal$values$width, 
                                    edge.type = k$edge.type)
                        })
                      if (!is.null(ReturnVal$values$color)) 
                        lapply(selectedEdges, function(k) if (k$edge.type == 
                          edge.type) {
                          color(E[[k$index]]) <<- ReturnVal$values$color
                          setEdgeColor(k$index, color = ReturnVal$values$color, 
                            edge.type = k$edge.type)
                        })
                      if (!is.null(ReturnVal$values$label)) {
                        message("Change of 'label' not possible for group of edges; ")
                      }
                      if (!is.null(ReturnVal$values$label.position)) {
                        message("Change of 'label.position' not possible for group of edges; ")
                      }
                      if (!is.null(ReturnVal$values$class)) 
                        lapply(selectedEdges, function(k) if (k$edge.type == 
                          edge.type) {
                          class(E[[k$index]]) <<- ReturnVal$values$class
                          subSubDeleteEdge(k$index, k$from, k$to, 
                            edge.type = k$edge.type)
                          drawEdge(E[[k$index]], k$index, lower = TRUE, 
                            edge.type = k$edge.type)
                        })
                      if (is.null(ReturnVal$values$color)) 
                        clearSelectedEdges()
                      else selectedEdges <<- list()
                    }
                    if (edge.type == "VertexEdge") 
                      GraphWindow@vertexEdges <<- E
                    else if (edge.type == "FactorEdge") 
                      GraphWindow@factorEdges <<- E
                    else if (edge.type == "ExtraEdge") 
                      GraphWindow@extraEdges <<- E
                    else if (edge.type == "BlockEdge") 
                      GraphWindow@blockEdges <<- E
                    setModel(object, R = NULL, txt = "changeEdgeClass", 
                      copyProperties = TRUE)
                  }
                }
            }
            propertyNode <- function(i, vertex.type = "Vertex") {
                force(i)
                force(vertex.type)
                function(...) {
                  f <- function() {
                    if ((length(selectedNodes) > 0) && (length(ReturnVal$values) > 
                      1) && ("color" %in% names(ReturnVal$values))) 
                      message("Only color changed for selected vertices; ")
                    if ("name" %in% names(ReturnVal$values)) {
                      if ((vertex.type == "ClosedBlock") || (vertex.type == 
                        "OpenBlock")) {
                        message("No names for blocks! ; ")
                      }
                      else {
                        message("Names can not be changed, use labels ; ")
                      }
                    }
                    if ("index" %in% names(ReturnVal$values)) {
                      message("The internal index not be changed! ; ")
                    }
                    if ("ancestors" %in% names(ReturnVal$values)) {
                      message("Ancestors should not be set manually; ")
                    }
                    if ("descendants" %in% names(ReturnVal$values)) {
                      message("Descendants should not be set manually; ")
                    }
                    if ("position" %in% names(ReturnVal$values)) {
                      if ((vertex.type == "ClosedBlock") || (vertex.type == 
                        "OpenBlock")) {
                        positionsBlocks[i, , ] <<- ReturnVal$values$position
                        subUpdateGraphWindow("Update from main menu", 
                          redrawVertices = TRUE, raiseEdges = TRUE, 
                          updateEdges = TRUE, all.blockframes = TRUE)
                        setUpdateBlocks("")
                      }
                      else {
                        posFrom <- retVertexPos(i, vertex.type)
                        posTo <- positionsCanvas(ReturnVal$values$position)
                        subMoveVertex(i, vertex.type, posFrom, 
                          posTo)
                        moveFactorVertex(i)
                        setUpdatePositions("")
                      }
                    }
                    if ("blockindex" %in% names(ReturnVal$values)) {
                      if ((vertex.type == "ClosedBlock") || (vertex.type == 
                        "OpenBlock")) {
                      }
                      else {
                        message(paste("Blockindex of vertices only used when positions", 
                          " of vertices relative blocks are ignored ; "))
                        if (FALSE && .IsEmpty(blockList)) 
                          if (vertex.type == "Vertex") {
                            blockindexVertices[i] <<- ReturnVal$blockindex
                          }
                          else if (vertex.type == "Factor") {
                            blockindexFactorVertices[-i] <<- ReturnVal$blockindex
                          }
                          else if (vertex.type == "Extra") {
                            blockindexExtraVertices[i] <<- ReturnVal$blockindex
                          }
                      }
                    }
                    if ("stratum" %in% names(ReturnVal$values)) {
                      if ((vertex.type == "ClosedBlock") || (vertex.type == 
                        "OpenBlock")) {
                        strataBlocks[abs(i)] <<- ReturnVal$values$stratum
                        if (updateAllBlockIndices()) 
                          setUpdateBlockEdges("propertyDialog: Block stratum")
                        subUpdateGraphWindow("propertyDialog: Block stratum", 
                          updateEdges = TRUE, blockframes = 0)
                        setUpdateBlocks("")
                      }
                      else {
                        message(paste("Stratas of vertices only used when positions", 
                          " of vertices relative blocks are ignored ; "))
                        if (FALSE && .IsEmpty(blockList)) 
                          if (vertex.type == "Vertex") {
                            strataVertices[i] <<- ReturnVal$stratum
                          }
                          else if (vertex.type == "Factor") {
                            strataFactorVertices[-i] <<- ReturnVal$stratum
                          }
                          else if (vertex.type == "Extra") {
                            strataExtraVertices[i] <<- ReturnVal$stratum
                          }
                      }
                    }
                    if ("closed" %in% names(ReturnVal$values)) {
                      message("Closed block by mouse interaction; ")
                    }
                    if ("visible" %in% names(ReturnVal$values)) {
                      message("Closed ancestor block by mouse interaction; ")
                    }
                    if ("color" %in% names(ReturnVal$values)) {
                      if (vertex.type != "OpenBlock") 
                        setVertexColor(i, ReturnVal$values$color, 
                          vertex.type, permanent = TRUE)
                      else message("Close and open block to activate color; ")
                      if (length(selectedNodes) > 0) {
                        lapply(selectedNodes, function(k) if ((k$node.type != 
                          "OpenBlock") && (k$node.type != "ClosedBlock")) 
                          setVertexColor(k$index, color = ReturnVal$values$color, 
                            vertex.type = k$node.type, permanent = TRUE))
                        selectedNodes <<- list()
                      }
                      setUpdatePositions("")
                    }
                    if ("label" %in% names(ReturnVal$values)) {
                      setVertexLabel(i, ReturnVal$values$label, 
                        vertex.type)
                      setUpdatePositions("")
                    }
                    if ("label.position" %in% names(ReturnVal$values)) 
                      if (!(vertex.type == "OpenBlock")) {
                        posFrom <- retLabelPos(i, vertex.type)
                        X <- positionsCanvas(ReturnVal$values$label.position)
                        dxy <- findDifference(X, posFrom)
                        tkmove(canvas, vertexItem(i, vertex.type)$label, 
                          dxy[1], dxy[2])
                        setLabelPos(i, X, dxy, vertex.type)
                        setUpdatePositions("")
                      }
                    if ("class" %in% names(ReturnVal$values)) {
                      message("Changing class of variable!")
                      subUpdateGraphWindow("Update from main menu", 
                        redrawVertices = TRUE, raiseEdges = TRUE, 
                        updateEdges = TRUE, all.blockframes = TRUE)
                      setUpdateVertices("")
                    }
                  }
                  if ((vertex.type == "ClosedBlock") || (vertex.type == 
                    "OpenBlock")) {
                    B <- blocksUpdate()
                    block <- blockList[[i]]
                    difficultSlots <- c("ancestors", "descendants", 
                      "closed", "visible")
                    if ((vertex.type == "OpenBlock")) 
                      difficultSlots <- c(difficultSlots, "color", 
                        "label", "label.position")
                    ReturnVal <- propertyDialog(block, NULL, 
                      okReturn = FALSE, fixedSlots = c("index"), 
                      difficultSlots = difficultSlots, top = GraphWindow.top[[1]])
                    if (!is.null(ReturnVal$values)) {
                      Frame.Models@blocks[[abs(i)]] <<- ReturnVal$object
                      blockList[[abs(i)]] <<- ReturnVal$object
                      f()
                    }
                  }
                  else if (vertex.type == "Vertex") {
                    V <- verticesUpdate()
                    vertex <- Frame.Models@vertices[[i]]
                    ReturnVal <- propertyDialog(vertex, vertexClasses, 
                      fixedSlots = c("index", "name"), difficultSlots = c("blockindex", 
                        "stratum"), okReturn = FALSE, top = GraphWindow.top[[1]])
                    if (!is.null(ReturnVal$values)) {
                      Frame.Models@vertices[[i]] <<- ReturnVal$object
                      vertexList[[i]] <<- ReturnVal$object
                      f()
                    }
                  }
                  else if (vertex.type == "Factor") {
                    V <- factorVerticesUpdate()
                    vertex <- GraphWindow@factorVertices[[abs(i)]]
                    ReturnVal <- propertyDialog(vertex, factorClasses, 
                      fixedSlots = c("name", "index", "vertex.indices"), 
                      difficultSlots = c("blockindex", "stratum"), 
                      okReturn = FALSE, top = GraphWindow.top[[1]])
                    if (!is.null(ReturnVal$values)) {
                      GraphWindow@factorVertices[[-i]] <<- ReturnVal$object
                      factorVertexList[[-i]] <<- ReturnVal$object
                      f()
                    }
                  }
                  else if (vertex.type == "Extra") {
                    V <- extraVerticesUpdate()
                    vertex <- GraphWindow@extraVertices[[i]]
                    ReturnVal <- propertyDialog(vertex, NULL, 
                      okReturn = FALSE, fixedSlots = c("name", 
                        "index"), difficultSlots = c("blockindex", 
                        "stratum"), top = GraphWindow.top[[1]])
                    if (!is.null(ReturnVal$values)) {
                      GraphWindow@extraVertices[[i]] <<- ReturnVal$object
                      extraList[[i]] <<- ReturnVal$object
                      f()
                    }
                  }
                }
            }
            computeEdgeLabel <- function(i, f, t, force = FALSE, 
                edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(force)
                force(edge.type)
                function(...) {
                  if (retActivatedEdge() == i) {
                    tkconfigure(canvas, cursor = "watch")
                    tkfocus(GraphWindow.top[[1]])
                    edges <- edgeItem(f, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) 
                        if (e$nr == i) 
                          if (e$to == t) 
                            if (!is.null(object) && (hasMethods || 
                              hasMethod("testEdge", class(object)))) {
                              from.type <- vertexTypeOfEdge(f, 
                                edge.type)
                              to.type <- vertexTypeOfEdge(t, 
                                edge.type)
                              R <- testEdge(object, action = "remove", 
                                name.1 = retVertexName(f, from.type), 
                                name.2 = retVertexName(t, to.type), 
                                from = f, to = t, from.type = from.type, 
                                to.type = to.type, edge.index = i, 
                                force = force, Arguments = Args())
                              if (!is.null(R)) {
                                if ((hasMethods || hasMethod("label", 
                                  class(R)))) 
                                  setEdgeLabel(e, label(R), e$label.number, 
                                    f = f, permanent = TRUE)
                                if ((hasMethods || hasMethod("width", 
                                  class(R)))) 
                                  setEdgeWidth(e, width(R), e$label.number, 
                                    f = f)
                                activateEdge(0, from = f, to = t, 
                                  edge.type = edge.type)()
                              }
                            }
                    tkconfigure(canvas, cursor = "arrow")
                  }
                }
            }
            deleteEdgeLabel <- function(i, f, t, edge.type = "VertexEdge") {
                force(i)
                force(f)
                force(t)
                force(edge.type)
                function(...) {
                  edges <- edgeItem(f, edge.type = edge.type)
                  E <- getEdges(edge.type = edge.type)[[i]]
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if (e$nr == i) {
                        setEdgeLabel(e, "", e$label.number, f = f, 
                          permanent = TRUE)
                        setEdgeWidth(e, E@width, e$label.number, 
                          f = f)
                      }
                }
            }
            moveBlockPoint <- function(i, A) {
                force(i)
                force(A)
                function(x, y) {
                  posTo <- retBlockPoints(i)[A, ]
                  X <- replaceXY(x, y, posTo)
                  dxy <- findDifference(X, posTo)
                  changeBlockCornerPos(i, A, dxy)
                  tkcoordsBlock(i, color = "black", lower = FALSE)
                  if (updateAllBlockIndices()) 
                    setUpdateBlockEdges("moveBlockPoint")
                  subUpdateGraphWindow("moveBlockPoint", blockframes = 0)
                  setUpdateBlocks("")
                }
            }
            moveBlockLine <- function(i, A, B) {
                force(i)
                force(A)
                force(B)
                function(x, y) {
                  if ((A == 1) && (B == 3)) 
                    direction <- 1
                  else if ((A == 4) && (B == 8)) 
                    direction <- 1
                  else if ((A == 1) && (B == 4)) 
                    direction <- 2
                  else if ((A == 3) && (B == 8)) 
                    direction <- 2
                  else if ((A == 5) && (B == 6)) 
                    direction <- 1
                  else if ((A == 7) && (B == 2)) 
                    direction <- 1
                  else if ((A == 5) && (B == 7)) 
                    direction <- 2
                  else if ((A == 6) && (B == 2)) 
                    direction <- 2
                  else if ((A == 1) && (B == 5)) 
                    direction <- 3
                  else if ((A == 3) && (B == 6)) 
                    direction <- 3
                  else if ((A == 4) && (B == 7)) 
                    direction <- 3
                  else if ((A == 8) && (B == 2)) 
                    direction <- 3
                  posA <- retBlockPoints(i)[A, ]
                  posB <- retBlockPoints(i)[B, ]
                  X <- replaceXY(x, y, (posA + posB)/2)
                  Y <- X
                  Y[direction] <- (posA[direction] + posB[direction])/2
                  dxy <- findDifference(X, Y)
                  changeBlockCornerPos(i, A, dxy)
                  tkcoordsBlock(i, color = "black", lower = FALSE)
                  if (updateAllBlockIndices()) 
                    setUpdateBlockEdges("moveBlockLine")
                  subUpdateGraphWindow("moveBlockLine", blockframes = 0)
                  setUpdateBlocks("")
                }
            }
            moveBlock <- function(i, A) {
                force(i)
                force(A)
                Y <- NULL
                function(x, y) {
                  posTo <- retBlockPoints(i)[A, ]
                  if (is.null(Y)) 
                    Y <<- replaceXY(x, y, posTo)
                  else {
                    X <- replaceXY(x, y, posTo)
                    dxy <- findDifference(X, Y)
                    Y <<- X
                    changeBlockPos(i, A, dxy)
                    moveVerticesInBlock(i, dxy)
                    for (j in blockList[[i]]@descendants) if ((j != 
                      0) && (j != i)) {
                      changeBlockPos(j, A, dxy)
                      moveVerticesInBlock(j, dxy)
                      if (!(hiddenBlock[j] || closedBlock[j])) 
                        tkcoordsBlock(j, color = "black", lower = FALSE)
                    }
                    if (updateAllBlockIndices()) 
                      setUpdateBlockEdges("moveBlock")
                    subUpdateGraphWindow("moveBlock", blockframes = 0)
                    tkcoordsBlock(i, color = "black", lower = FALSE)
                    setUpdateBlocks("")
                  }
                }
            }
            hideBlock <- function(i, ancestor, update = TRUE) {
                blockReferences[i] <<- ancestor
                for (v in seq(along = vertexList)) if (is.element(v, 
                  visibleVertices)) 
                  if (retBlockIndex(v, vertex.type = "Vertex") == 
                    i) 
                    setCloseVertex(v, TRUE, "Vertex")
                setHiddenBlock(i, TRUE, update = update)
                for (j in blockList[[i]]@descendants) if ((j != 
                  0) && !hiddenBlock[j]) 
                  hideBlock(j, ancestor, update = FALSE)
            }
            closeBlock <- function(i, update = TRUE) {
                force(i)
                function(...) {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if (retBlockIndex(v, vertex.type = "Vertex") == 
                      i) 
                      setCloseVertex(v, TRUE, "Vertex")
                  for (j in blockList[[i]]@descendants) if ((j != 
                    i) && (j != 0) && !hiddenBlock[j]) 
                    hideBlock(j, i, update = FALSE)
                  setClosedBlock(i, TRUE, update = update)
                  drawVertex(i, w = 10, vertexcolor = "Black", 
                    vertex.type = "ClosedBlock")
                  if (update) 
                    subUpdateGraphWindow("closeBlock", blockframes = i, 
                      updateEdges = TRUE)
                }
            }
            subOpenBlock <- function(i, update = TRUE) {
                blockReferences[i] <<- i
                if ((hiddenBlock[i] && closedBlock[i])) {
                  drawVertex(i, w = 10, vertexcolor = "Black", 
                    vertex.type = "ClosedBlock")
                }
                else {
                  setClosedBlock(i, FALSE, update = update)
                  vertex.type <- "ClosedBlock"
                  deActivateVertex(i, retVertexColor(i, vertex.type), 
                    vertex.type)
                  if (is.element(i, visibleBlocks)) 
                    drawBlock(blockList[[i]], i)
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) {
                    bi <- retBlockIndex(v, vertex.type = "Vertex")
                    if ((bi == i) || (bi == 0)) 
                      setCloseVertex(v, FALSE, "Vertex")
                  }
                  for (j in blockList[[i]]@descendants) if ((j != 
                    0) && (hiddenBlock[j])) 
                    if (!isInClosedBlock(j)) 
                      subOpenBlock(j, update = FALSE)
                }
                setHiddenBlock(i, FALSE, update = FALSE)
            }
            openBlock <- function(i, update = TRUE) {
                force(i)
                force(update)
                function(...) {
                  subOpenBlock(i, update)
                  if (update) 
                    subUpdateGraphWindow("openBlock", raiseEdges = TRUE, 
                      updateEdges = TRUE)
                }
            }
            subNewVertex <- function(position, get.name = TRUE, 
                get.vertex.type = TRUE, get.how.to.compute = TRUE) {
                n <- length(vertexList) + 1
                if (get.name) {
                  ReturnVal <- modalDialog("Name Entry", "Enter name of new variable", 
                    paste("V", n, sep = ""), top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                }
                else ReturnVal <- paste("V", n, sep = "")
                vertextypes <- vertexClasses[, 1]
                vertextypes <- paste(vertextypes)
                vertex.type <- vertextypes[1]
                if (get.vertex.type) {
                  Return.Val <- selectDialog("Variable vertex.type selection entry", 
                    "Select vertex.type", vertextypes, top = GraphWindow.top[[1]])
                  vertexType <- vertextypes[Return.Val]
                }
                vertex <- newVertex(ReturnVal, ReturnVal, vertex.type, 
                  n, position, color = vertexColor, stratum = 0, 
                  vertexClasses = vertexClasses)
                if (get.how.to.compute) {
                  Expression <- modalDialog("Expression Entry", 
                    "Enter expression for computing new variable", 
                    paste("object <<- object"), top = GraphWindow.top[[1]])
                  if (Expression == "ID_CANCEL") 
                    return()
                  else eval(parse(text = Expression))
                }
                else Expression <- ReturnVal
                verticesUpdate()
                vertexList <<- append(vertexList, list(vertex))
                class(vertexList) <<- "dg.VertexList"
                Frame.Models@vertices <<- vertexList
                positionsVertices <<- rbind(positionsVertices, 
                  position(vertex))
                positionsLabels <<- rbind(positionsLabels, position(vertex))
                Labels <<- c(Labels, Expression)
                closedVertex <<- c(closedVertex, FALSE)
                colorsVertices <<- c(colorsVertices, vertexColor)
                blocksVertices <<- c(blocksVertices, 0)
                strataVertices <<- c(strataVertices, 0)
                namesVertices <<- c(namesVertices, ReturnVal)
                itemsVertices <<- append(itemsVertices, list(NULL))
                itemsEdges <<- append(itemsEdges, list(NULL))
                updateAllBlockIndices()
            }
            subAddVertex <- function(index, vertex.type = "Vertex", 
                slave = TRUE) {
                tkconfigure(canvas, cursor = "watch")
                tkfocus(GraphWindow.top[[1]])
                redraw <- FALSE
                if (!(viewType == "Simple")) 
                  redraw <- TRUE
                vertexEdges <- appendToCurrentEdges(omitEdges = FALSE, 
                  new.edge = NULL, edge.type = "VertexEdge")
                newEdges <- list(vertexEdges = vertexEdges, factorEdges = getEdges(edge.type = "FactorEdge"), 
                  extraEdges = getEdges(edge.type = "ExtraEdge"), 
                  blockEdges = getEdges(edge.type = "BlockEdge"))
                if (vertex.type == "selected") {
                  redraw <- TRUE
                  message("Selected vertices not added to 'newEdges';")
                  message("Resulting edges should be returned from modifyModel!")
                }
                R <- NULL
                if (is.null(object)) 
                  R <- TRUE
                Arguments <- Args()
                visible.Vertices <- returnVisibleVertices()
                if (index != 0) 
                  visible.Vertices <- c(visible.Vertices, index)
                if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                  class(object)))) {
                  if (index == 0) 
                    name <- ""
                  else name <- retVertexName(index, vertex.type)
                  R <- modifyModel(object, action = "addVertex", 
                    name = name, index = index, type = vertex.type, 
                    newEdges = newEdges, visibleVertices = visible.Vertices, 
                    selectedNodes = selectedNodes, selectedEdges = selectedEdges, 
                    Arguments = Arguments)
                }
                if (!is.null(R)) {
                  objectAssign(R)
                  setVisibleVertices(visible.Vertices)
                  if (slave || redraw) 
                    drawResult(newEdges, R, slave, "addVertex")
                  else {
                    drawVertex(index, w = w, vertexcolor = vertexColor, 
                      vertex.type = "Vertex")
                    updateSelectedEdges(R, vertexEdges, edge.type = NULL)
                  }
                }
                else message("Null result in addVertex")
                tkconfigure(canvas, cursor = "arrow")
            }
            newVertexXY <- function() {
                function(x, y) {
                  X <- replaceXY(x, y, rep(50, N))
                  position <- c(inversProject(inversCanvasPosition(X)))
                  subNewVertex(position, get.name = FALSE, get.vertex.type = FALSE, 
                    get.how.to.compute = FALSE)
                  n <- length(vertexList)
                  subAddVertex(n, slave = FALSE)
                  tkfocus(canvas)
                }
            }
            zoom <- function(xy, factor) {
                f <- function(title, x) print(paste(title, paste(x, 
                  collapse = ", ")))
                if (is.null(xy)) 
                  xy <- c(width, height, rep(100, N - 2))/2
                X <- xy
                if (is.null(xy)) {
                  xy <- c(width, height, rep(100, N - 2))/2
                }
                else if (!is.null(zoomCenter)) 
                  X <- (xy - zoomCenter)/Scale + zoomCenter
                Scale <<- Scale * factor
                xy <- round(xy)
                for (i in seq(along = GraphWindow.tags)) {
                  tkitemscale(canvas, GraphWindow.tags[[i]], 
                    xy[1], xy[2], factor, factor)
                }
                if (debug.position) {
                  f("xy: ", xy)
                  f("X: ", X)
                  f("Old zoomCenter: ", zoomCenter)
                  f("Old upperLeft: ", upperLeft)
                  f("Old lowerRight: ", lowerRight)
                }
                upperLeft <<- upperLeft - (upperLeft - X)/factor
                lowerRight <<- lowerRight - (lowerRight - X)/factor
                newCenter <- (lowerRight + upperLeft)/2
                if (!is.null(zoomCenter)) 
                  if ((sum(zoomCenter - newCenter)^2) > 0.1) 
                    zoomCenterSet <<- TRUE
                zoomCenter <<- newCenter
                if (debug.position) {
                  f("New zoomCenter: ", zoomCenter)
                  f("New upperLeft: ", upperLeft)
                  f("New lowerRight: ", lowerRight)
                }
                if (zoomCenterSet) 
                  subUpdateGraphWindow("Update from zoom", redrawVertices = TRUE, 
                    raiseEdges = TRUE, updateEdges = TRUE, all.blockframes = TRUE)
                setSrcLabel(GraphWindow.viewLabel[[1]])
            }
            zoomIn <- function() {
                function(x, y) {
                  xy <- replaceXY(x, y, rep(50, N))
                  zoom(xy, sqrt(sqrt(2)))
                }
            }
            zoomOut <- function() {
                function(x, y) {
                  xy <- replaceXY(x, y, rep(50, N))
                  zoom(xy, 1/sqrt(sqrt(2)))
                }
            }
            resizeCanvas <- function() {
                function(x, y, ...) {
                }
            }
            initFactorVariables <- function(factorVertexList) {
                if (length(factorVertexList) > 0) {
                  itemsFactors <<- vector("list", length(factorVertexList))
                  itemsFactorEdges <<- vector("list", length(factorVertexList))
                  namesFactorVertices <<- Names(factorVertexList)
                  positionsFactorVertices <<- Positions(factorVertexList)
                  positionsFactorLabels <<- positionsFactorVertices
                  positionsFactorLabels[, 1] <<- positionsFactorLabels[, 
                    1] + 0.1 * w
                  factorLabels <<- Labels(factorVertexList)
                  colorsFactorVertices <<- Colors(factorVertexList)
                  blocksFactorVertices <<- rep(0, length(factorVertexList))
                  strataFactorVertices <<- rep(0, length(factorVertexList))
                  if (!is.matrix(positionsFactorVertices)) 
                    warning("Positions of factor-vertices should have same number of coordinates")
                  else if (!(dim(positionsFactorVertices)[2] == 
                    dim(positionsVertices)[2])) 
                    warning("Factor-vertices should have same number of coordinates as vertices")
                }
            }
            initExtraVariables <- function(extraList) {
                if (length(extraList) > 0) {
                  itemsExtras <<- vector("list", length(extraList))
                  itemsExtraEdges <<- vector("list", length(extraList))
                  namesExtraVertices <<- Names(extraList)
                  positionsExtraVertices <<- Positions(extraList)
                  positionsExtraLabels <<- positionsExtraVertices
                  positionsExtraLabels[, 1] <<- positionsExtraLabels[, 
                    1] + 0.1 * w
                  extraLabels <<- Labels(extraList)
                  colorsExtraVertices <<- Colors(extraList)
                  strataExtraVertices <<- rep(0, length(extraList))
                  blocksExtraVertices <<- rep(0, length(extraList))
                  if (!is.matrix(positionsExtraVertices)) 
                    warning("Positions of extra-vertices should have same number of coordinates")
                  else if (!(dim(positionsExtraVertices)[2] == 
                    dim(positionsVertices)[2])) 
                    warning("Extra-vertices should have same number of coordinates as vertices")
                }
            }
            replaceBlocks <- function(blockList) {
                if (!is.null(blockList)) {
                  positionsBlocks <- Positions(blockList)
                  d <- dim(positionsBlocks)
                  positionsBlocks <<- array(positionsBlocks, 
                    dim = c(d[1], d[2]/2, 2))
                  positionsClosedBlocks <- matrix(rep(NA, N * 
                    length(blockList)), ncol = N)
                  positionsClosedBlocks <<- apply(positionsBlocks, 
                    c(1, 2), mean)
                  blockReferences <<- 1:length(blockList)
                  positionsBlockLabels <<- matrix(rep(0, N * 
                    length(blockList)), ncol = N)
                  blockLabels <<- Labels(blockList)
                  strataBlocks <<- Strata(blockList)
                  setUpdateAll()
                }
            }
            replaceVertices <- function(vertexList) {
                if (length(vertexList) > 0) {
                  namesVertices <<- Names(vertexList)
                  positionsVertices <<- Positions(vertexList)
                  if (is.matrix(positionsVertices)) {
                    positionsLabels <<- positionsVertices
                    positionsLabels[, 1] <<- positionsLabels[, 
                      1] + 0.1 * w
                    Labels <<- Labels(vertexList)
                    colorsVertices <<- Colors(vertexList)
                    blocksVertices <<- Blockindices(vertexList)
                    strataVertices <<- Strata(vertexList)
                    setUpdateVertices()
                    setUpdatePositions()
                  }
                  if (!is.matrix(positionsVertices)) 
                    warning("Positions of extra-vertices should have same number of coordinates;")
                  else if (!(dim(positionsVertices)[2] == N)) 
                    warning("New vertices should have same number of coordinates as old vertices;")
                }
            }
            drawFactors <- function(X.FactorEdges, X.FactorVertices) {
                factorEdgeList <<- X.FactorEdges
                GraphWindow@factorEdges <<- factorEdgeList
                factorVertexList <<- X.FactorVertices
                GraphWindow@factorVertices <<- factorVertexList
                initFactorVariables(factorVertexList)
                for (i in seq(along = factorEdgeList)) {
                  f <- factorEdgeList[[i]]@vertex.indices[1]
                  t <- factorEdgeList[[i]]@vertex.indices[2]
                  E <- append.index.edge(c(f, t), edge.type = "FactorEdge")
                  drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                    edge.type = "FactorEdge")
                }
                if (length(factorVertexList) > 0) 
                  for (i in seq(along = factorVertexList)) drawVertex(-i, 
                    w = w, vertexcolor = vertexColor, vertex.type = "Factor")
            }
            setMainMenu <- function() {
                topMenu <- tkmenu(GraphWindow.top[[1]])
                tkconfigure(GraphWindow.top[[1]], menu = topMenu)
                fileMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(fileMenu, "command", label = "Save as Postscript ... ", 
                  command = function() {
                    fileName <- tclvalue(tkgetSaveFile(initialfile = "dynamicGraph.ps", 
                      filetypes = "{{Postsript Files} {.ps}}"))
                    if (!nchar(fileName)) 
                      tkmessageBox(message = "No file was selected!")
                    else tkpostscript(canvas, file = fileName, 
                      width = width, height = height)
                  })
                tkadd(fileMenu, "command", label = "Quit", command = function() tkdestroy(GraphWindow.top[[1]]))
                tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
                userMenu <- tkmenu(topMenu, tearoff = FALSE)
                UserMainMenu <- function(i) {
                  force(i)
                  function(...) {
                    updateArguments(UserMenus[[i]], blocks = TRUE)
                    arguments <- Args()
                    UserMenus[[i]]$command(object, Arguments = arguments)
                  }
                }
                if (length(UserMenus) > 0) 
                  for (i in seq(along = UserMenus)) if (names(UserMenus[i]) == 
                    "MainUser") 
                    tkadd(userMenu, "command", label = UserMenus[[i]]$label, 
                      command = UserMainMenu(i))
                tkadd(topMenu, "cascade", label = "User", menu = userMenu)
                variableMenu <- tkmenu(topMenu, tearoff = FALSE)
                selectVariableDialog <- function() {
                  vertexlabels <- Labels[visibleVertices]
                  ReturnVal <- selectDialog("Variable vertex.type selection entry", 
                    "Select vertex.type", vertexlabels, top = GraphWindow.top[[1]])
                  if ((length(ReturnVal) > 0) && (ReturnVal != 
                    "ID_CANCEL")) {
                    variableChoice <- vertexlabels[ReturnVal]
                    j <- (1:length(Labels))[Labels == variableChoice]
                    subActivateVertex(j, color = "green", vertex.type = "Vertex")
                    msg <- paste("Click other vertex to add edge to ", 
                      variableChoice, sep = "")
                    tkmessageBox(message = msg)
                  }
                }
                tkadd(variableMenu, "command", label = "Highlight vertex (for adding edge)", 
                  command = function() selectVariableDialog())
                selectOtherDialog <- function(slave = TRUE) {
                  "not.in" <- function(x, l = max(x)) {
                    y <- rep(TRUE, l)
                    y[x] <- FALSE
                    return((1:l)[y])
                  }
                  vertexnames <- Names(vertexList)[not.in(visibleVertices, 
                    length(vertexList))]
                  ReturnVal <- selectDialog("Variable vertex.type selection entry", 
                    "Select vertex.type", vertexnames, top = GraphWindow.top[[1]])
                  if ((length(ReturnVal) > 0) && (ReturnVal != 
                    "ID_CANCEL")) {
                    variableChoice <- vertexnames[ReturnVal]
                    index <- nameToVertexIndex(variableChoice, 
                      vertexList)
                    if (length(vertexnames) > 0) 
                      subAddVertex(index, slave = slave)
                  }
                }
                tkadd(variableMenu, "command", label = "Select vertex among variables not displayed (slave)", 
                  command = function() selectOtherDialog())
                tkadd(variableMenu, "command", label = "Select vertex among variables not displayed (here)", 
                  command = function() selectOtherDialog(slave = FALSE))
                tkadd(variableMenu, "command", label = paste("'addVertex', selected vertices and edges"), 
                  command = function() subAddVertex(0, vertex.type = "selected", 
                    slave = FALSE))
                tkadd(variableMenu, "command", label = paste("'dropVertex', selected vertices and edges"), 
                  command = function() subDropVertex(0, vertex.type = "selected", 
                    slave = FALSE))
                tkadd(variableMenu, "command", label = "Create new variable (not displayed before selected)", 
                  accelerator = "(~ <F3>)", command = function() subNewVertex(rep(0, 
                    N)))
                tkadd(topMenu, "cascade", label = "Variables", 
                  menu = variableMenu)
                expVarMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(variableMenu, "cascade", label = "Export & show", 
                  menu = expVarMenu)
                edgesMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(edgesMenu, "command", label = " ... ", 
                  command = function() {
                  })
                tkadd(edgesMenu, "command", label = "Delete all edge labels", 
                  accelerator = "<F6>", command = function() {
                    deleteAllEdgeLabels()()
                  })
                tkadd(edgesMenu, "command", label = paste("'addEdge', selected vertices and edges"), 
                  command = function() subAddEdge(0, 0, "none", 
                    "none", edge.type = "selected", slave = FALSE, 
                    edgeClass = edgeClass))
                tkadd(edgesMenu, "command", label = paste("'dropEdge', selected vertices and edges"), 
                  command = function() subSubDropEdge(0, 0, 0, 
                    "none", "none", edge.type = "selected", slave = FALSE))
                expEdgesMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(edgesMenu, "cascade", label = "Export & show", 
                  menu = expEdgesMenu)
                tkadd(topMenu, "cascade", label = "Edges", menu = edgesMenu)
                generatorsMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(generatorsMenu, "command", label = " ... e.i. factors ", 
                  command = function() {
                  })
                expGenMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(generatorsMenu, "cascade", label = "Export & show", 
                  menu = expGenMenu)
                tkadd(topMenu, "cascade", label = "Generators", 
                  menu = generatorsMenu)
                blocksMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(blocksMenu, "command", label = " ... ", 
                  command = function() {
                  })
                tkadd(blocksMenu, "command", label = "Add new block", 
                  accelerator = "(~ <F4>)", command = function() {
                    n <- length(blockList)
                    position <- rep(0, N) + n
                    position <- matrix(c(position - 10, position + 
                      10), nrow = 2, byrow = TRUE)
                    new.Block(position)
                  })
                expBlockMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(blocksMenu, "cascade", label = "Export & show", 
                  menu = expBlockMenu)
                tkadd(topMenu, "cascade", label = "Blocks", menu = blocksMenu)
                graphMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(graphMenu, "command", label = "Make slave window: Same model", 
                  command = function() {
                    updateArguments(NULL, edges = FALSE, blocks = TRUE)
                    edge.List <- currentEdges(edge.type = "VertexEdge")
                    blockEdgeList <- currentEdges(edge.type = "BlockEdge")
                    factorEdgeList <- currentEdges(edge.type = "FactorEdge")
                    extraEdgeList <- currentEdges(edge.type = "ExtraEdge")
                    Arguments <- Args()
                    redrawView(frameModels = Frame.Models, frameViews = Frame.Views, 
                      graphWindow = NULL, edgeList = edge.List, 
                      oriented = oriented, blockEdgeList = blockEdgeList, 
                      factorVertexList = factorVertexList, factorEdgeList = factorEdgeList, 
                      visibleVertices = visibleVertices, visibleBlocks = visibleBlocks, 
                      extraList = extraList, extraEdgeList = extraEdgeList, 
                      viewType = viewType, title = "Default", 
                      Arguments = Arguments)
                  })
                tkadd(graphMenu, "command", label = "Make slave window: Copy model", 
                  command = function() {
                    updateArguments(NULL, edges = FALSE, blocks = TRUE)
                    edge.List <- currentEdges(edge.type = "VertexEdge")
                    blockEdgeList <- currentEdges(edge.type = "BlockEdge")
                    factorEdgeList <- currentEdges(edge.type = "FactorEdge")
                    extraEdgeList <- currentEdges(edge.type = "ExtraEdge")
                    Arguments <- Args()
                    drawModel(frameModels = Frame.Models, frameViews = NULL, 
                      graphWindow = NULL, edgeList = edge.List, 
                      oriented = oriented, blockEdgeList = blockEdgeList, 
                      factorVertexList = factorVertexList, factorEdgeList = factorEdgeList, 
                      visibleVertices = visibleVertices, visibleBlocks = visibleBlocks, 
                      extraList = extraList, extraEdgeList = extraEdgeList, 
                      object = object, viewType = viewType, title = "Default", 
                      Arguments = Arguments)
                  })
                tkadd(graphMenu, "command", label = "Set 'viewType', class of graph window", 
                  command = function() {
                    Arguments <- Args()
                    ReturnVal <- selectDialog("Test selectDialog Entry", 
                      "Select name", viewClasses[, 1], top = GraphWindow.top[[1]])
                    if ((length(ReturnVal) > 0) && (ReturnVal != 
                      "ID_CANCEL")) {
                      setModel(object, NULL, "SetClassOfView", 
                        setUpdate = FALSE)
                      class(GraphWindow) <<- viewClasses[ReturnVal, 
                        2]
                      viewType <<- viewClasses[ReturnVal, 1]
                      tkconfigure(GraphWindow.viewLabel[[1]], 
                        text = viewType)
                      updateModel()
                    }
                  })
                refreshMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(refreshMenu, "command", label = "Refresh view (set positions as 'stored')", 
                  command = function() {
                    subUpdateGraphWindow("Update from main menu", 
                      redrawVertices = TRUE, raiseEdges = TRUE, 
                      updateEdges = TRUE, all.blockframes = TRUE)
                    updateArguments(NULL, edges = TRUE, blocks = TRUE)
                  })
                tkadd(refreshMenu, "command", label = "Redraw graph window (more refreshing)", 
                  command = function() {
                    Arguments <- Args()
                    redrawView(frameModels = Frame.Models, frameViews = Frame.Views, 
                      graphWindow = GraphWindow, Arguments = Arguments)
                  })
                tkadd(graphMenu, "cascade", label = "Refresh", 
                  menu = refreshMenu)
                transformationMenu <- tkmenu(topMenu, tearoff = FALSE, 
                  postcommand = function() nullTrans <<- tclVar(is.null(transformation)))
                nullTrans <- tclVar(is.null(transformation))
                tkadd(transformationMenu, "radiobutton", label = "Reset (enable) transformation", 
                  variable = nullTrans, value = 0, command = function() setTransformation(diag(1, 
                    N)))
                tkadd(transformationMenu, "radiobutton", label = "Disable rotation", 
                  variable = nullTrans, value = 1, command = function() setTransformation(NULL))
                tkadd(graphMenu, "cascade", label = "Rotation: The transformation", 
                  menu = transformationMenu)
                tkadd(graphMenu, "command", label = "Close", 
                  accelerator = "(~ [X])", command = function() destroyView()())
                if (permitZoom) {
                  tkadd(graphMenu, "command", label = "Zoom in", 
                    accelerator = "(~ <F1>)", command = function() zoom(zoomCenter, 
                      2^0.25))
                  tkadd(graphMenu, "command", label = "Zoom out", 
                    accelerator = "(~ <F2>)", command = function() zoom(zoomCenter, 
                      2^-0.25))
                }
                nameLabelMenu <- tkmenu(topMenu, tearoff = FALSE, 
                  postcommand = function() useNames <<- tclVar(useNamesForLabels))
                useNames <- tclVar(useNamesForLabels)
                tkadd(nameLabelMenu, "radiobutton", label = "Labels is labels", 
                  variable = useNames, value = 0, command = function() {
                    useNamesForLabels <<- FALSE
                    subUpdateGraphWindow("useNamesForLabels", 
                      redrawVertices = TRUE, raiseEdges = FALSE, 
                      updateEdges = FALSE, all.blockframes = FALSE)
                  })
                tkadd(nameLabelMenu, "radiobutton", label = "Use names for labels", 
                  variable = useNames, value = 1, command = function() {
                    useNamesForLabels <<- TRUE
                    subUpdateGraphWindow("useNamesForLabels", 
                      redrawVertices = TRUE, raiseEdges = FALSE, 
                      updateEdges = FALSE, all.blockframes = FALSE)
                  })
                tkadd(graphMenu, "cascade", label = "Names for labels", 
                  menu = nameLabelMenu)
                tkadd(topMenu, "cascade", label = "Graph", menu = graphMenu)
                exportMenu <- tkmenu(topMenu, tearoff = FALSE)
                expGraphMenu <- tkmenu(topMenu, tearoff = FALSE)
                tkadd(graphMenu, "cascade", label = "Export & show", 
                  menu = expGraphMenu)
                argsExport <- function() {
                  ReturnVal <- modalDialog("Args Name Entry", 
                    "Enter name for Args", "Args", top = GraphWindow.top[[1]])
                  updateArguments(NULL, edges = TRUE, blocks = TRUE)
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, Args(), pos = 1)
                }
                tkadd(expGraphMenu, "command", label = "Assign 'Args' in .GlobalEnv", 
                  command = function() argsExport())
                tkadd(exportMenu, "command", label = "Assign 'Args' in .GlobalEnv", 
                  command = function() argsExport())
                updateAllPositionsMenu <- function() {
                  subUpdateAllFrames("Positions", txt = "Update")
                  Str(Frame.Models, excludeSlots = TRUE)
                }
                tkadd(expGraphMenu, "command", label = "Update positions of all views of 'frameModels'", 
                  command = function() updateAllPositionsMenu())
                tkadd(exportMenu, "command", label = "Update positions of all views of 'frameModels'", 
                  command = function() updateAllPositionsMenu())
                updateAllArgumentsMenu <- function() {
                  subUpdateAllFrames("Arguments", menuItem = NULL, 
                    vertices = TRUE, edges = TRUE, blocks = TRUE)
                  Str(Frame.Models, excludeSlots = TRUE)
                }
                tkadd(expGraphMenu, "command", label = "Update all slots of 'frameModels'", 
                  command = function() updateAllArgumentsMenu())
                tkadd(exportMenu, "command", label = "Update all slots of 'frameModels'", 
                  command = function() updateAllArgumentsMenu())
                latticeModelsExport <- function() {
                  ReturnVal <- modalDialog("Lattice for Models Name Entry", 
                    "Enter name for lattice for models", "frameModels", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  updateArguments(NULL, edges = TRUE, blocks = TRUE)
                  Str(Frame.Models, excludeSlots = TRUE)
                  message("Consider: \"Update all slots of 'frameModels'\" before 'export'")
                  assign(ReturnVal, Frame.Models, pos = 1)
                }
                tkadd(expGraphMenu, "command", label = "Assign 'frameModels' in .GlobalEnv", 
                  command = function() latticeModelsExport())
                tkadd(exportMenu, "command", label = "Assign 'frameModels' in .GlobalEnv", 
                  command = function() latticeModelsExport())
                latticeGraphsExport <- function() {
                  ReturnVal <- modalDialog("Lattice for Views Name Entry", 
                    "Enter name for lattice for views", "frameViews", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  updateArguments(NULL, edges = TRUE, blocks = TRUE)
                  Str(Frame.Views, excludeSlots = c("class", 
                    "label", "label.position"))
                  message("Consider: \"Update all slots of 'frameModels'\" ")
                  assign(ReturnVal, Frame.Views, pos = 1)
                }
                tkadd(expGraphMenu, "command", label = "Assign 'frameViews' in .GlobalEnv", 
                  command = function() latticeGraphsExport())
                tkadd(exportMenu, "command", label = "Assign 'frameViews' in .GlobalEnv", 
                  command = function() latticeGraphsExport())
                graphWindowExport <- function() {
                  ReturnVal <- modalDialog("GraphWindow Name Entry", 
                    "Enter name for graphWindow", "graphWindow", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  updateArguments(NULL, edges = TRUE, blocks = TRUE)
                  Str(GraphWindow, excludeSlots = c("class", 
                    "label", "label.position"))
                  assign(ReturnVal, GraphWindow, pos = 1)
                }
                tkadd(expGraphMenu, "command", label = "Assign 'graphWindow' in .GlobalEnv", 
                  command = function() graphWindowExport())
                tkadd(exportMenu, "command", label = "Assign 'graphWindow' in .GlobalEnv", 
                  command = function() graphWindowExport())
                objectExport <- function() {
                  ReturnVal <- modalDialog("Object Name Entry", 
                    "Enter name for object", "object", top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, object, pos = 1)
                }
                tkadd(exportMenu, "command", label = "Assign 'object' in .GlobalEnv", 
                  command = function() objectExport())
                tkadd(expGraphMenu, "command", label = "Assign 'object' in .GlobalEnv", 
                  command = function() objectExport())
                transformationExport <- function() {
                  ReturnVal <- modalDialog("Transformation Name Entry", 
                    "Enter name for transformation", "transformation", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, transformation, pos = 1)
                }
                tkadd(exportMenu, "command", label = "Assign 'transformation' in .GlobalEnv", 
                  command = function() transformationExport())
                tkadd(expGraphMenu, "command", label = "Assign 'transformation' in .GlobalEnv", 
                  command = function() transformationExport())
                topExport <- function() {
                  ReturnVal <- modalDialog("Top Name Entry", 
                    "Enter name for top", "top", top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, GraphWindow.top, pos = 1)
                }
                tkadd(exportMenu, "command", label = "Assign 'top' in .GlobalEnv", 
                  command = function() topExport())
                tkadd(expGraphMenu, "command", label = "Assign 'top' in .GlobalEnv", 
                  command = function() topExport())
                canvasExport <- function() {
                  ReturnVal <- modalDialog("Canvas Name Entry", 
                    "Enter name for canvas", "canvas", top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, GraphWindow.canvas, pos = 1)
                }
                tkadd(exportMenu, "command", label = "Assign 'canvas' in .GlobalEnv", 
                  command = function() canvasExport())
                tkadd(expGraphMenu, "command", label = "Assign 'canvas' in .GlobalEnv", 
                  command = function() canvasExport())
                viewLabelExport <- function() {
                  ReturnVal <- modalDialog("ViewLabel Name Entry", 
                    "Enter name for viewLabel", "viewLabel", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  assign(ReturnVal, GraphWindow.viewLabel, pos = 1)
                }
                tkadd(exportMenu, "command", label = "Assign 'viewLabel' in .GlobalEnv", 
                  command = function() viewLabelExport())
                tkadd(expGraphMenu, "command", label = "Assign 'viewLabel' in .GlobalEnv", 
                  command = function() viewLabelExport())
                tkadd(expVarMenu, "command", label = "'print(selectedNodesMatrix(selectedNodes()))'", 
                  command = function() print(selectedNodesMatrix()))
                verticesExport <- function() {
                  ReturnVal <- modalDialog("Vertices Name Entry", 
                    "Enter name for vertices", "vertexList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  vertices <- verticesUpdate()
                  assign(ReturnVal, vertexList, pos = 1)
                }
                tkadd(expVarMenu, "command", label = "Assign 'vertexList' in .GlobalEnv", 
                  command = function() verticesExport())
                tkadd(exportMenu, "command", label = "Assign 'vertexList' in .GlobalEnv", 
                  command = function() verticesExport())
                tkadd(expVarMenu, "command", label = "'print(asDataFrame(vertexList))'", 
                  command = function() print(asDataFrame(vertexList)))
                tkadd(expEdgesMenu, "command", label = "'print(selectedEdgesMatrix(selectedEdges()))'", 
                  command = function() print(selectedEdgesMatrix()))
                edgesExport <- function() {
                  ReturnVal <- modalDialog("Edges Name Entry", 
                    "Enter name for Edges", "edgeList", top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  edge.List <- edgesUpdate()
                  assign(ReturnVal, edge.List, pos = 1)
                }
                tkadd(expEdgesMenu, "command", label = "Assign 'edgeList' in .GlobalEnv", 
                  command = function() edgesExport())
                tkadd(exportMenu, "command", label = "Assign 'edgeList' in .GlobalEnv", 
                  command = function() edgesExport())
                tkadd(expEdgesMenu, "command", label = "'print(asDataFrame(edgeList))'", 
                  command = function() print(asDataFrame(edgeList)))
                extraVerticesExport <- function() {
                  ReturnVal <- modalDialog("ExtraVertices Name Entry", 
                    "Enter name for the extravertices", "extraList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  extra.List <- extraVerticesUpdate()
                  assign(ReturnVal, extra.List, pos = 1)
                }
                tkadd(expEdgesMenu, "command", label = "Assign 'extraList' in .GlobalEnv", 
                  command = function() extraVerticesExport())
                tkadd(exportMenu, "command", label = "Assign 'extraList' in .GlobalEnv", 
                  command = function() extraVerticesExport())
                tkadd(expEdgesMenu, "command", label = "'print(asDataFrame(extraList))'", 
                  command = function() print(asDataFrame(extraList)))
                extraEdgesExport <- function() {
                  ReturnVal <- modalDialog("ExtraEdges Name Entry", 
                    "Enter name for the extraedges", "extraEdgeList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  extra.Edge.List <- extraEdgesUpdate()
                  assign(ReturnVal, extra.Edge.List, pos = 1)
                }
                tkadd(expEdgesMenu, "command", label = "Assign 'extraEdgeList' in .GlobalEnv", 
                  command = function() extraEdgesExport())
                tkadd(exportMenu, "command", label = "Assign 'extraEdgeList' in .GlobalEnv", 
                  command = function() extraEdgesExport())
                tkadd(expEdgesMenu, "command", label = "'print(asDataFrame(extraEdgeList))'", 
                  command = function() print(asDataFrame(extraEdgeList)))
                factorVerticesExport <- function() {
                  ReturnVal <- modalDialog("FactorVertices Name Entry", 
                    "Enter name for the factorvertices", "factorVertexList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  factorVertex.List <- factorVerticesUpdate()
                  assign(ReturnVal, factorVertex.List, pos = 1)
                }
                tkadd(expGenMenu, "command", label = "Assign 'factorVertexList' in .GlobalEnv", 
                  command = function() factorVerticesExport())
                tkadd(exportMenu, "command", label = "Assign 'factorVertexList' in .GlobalEnv", 
                  command = function() factorVerticesExport())
                tkadd(expGenMenu, "command", label = "'print(asDataFrame(factorVertexList))'", 
                  command = function() print(asDataFrame(factorVertexList)))
                factorEdgesExport <- function() {
                  ReturnVal <- modalDialog("FactorEdges Name Entry", 
                    "Enter name for the factoredges", "factorEdgeList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  factor.Edge.List <- factorEdgesUpdate()
                  assign(ReturnVal, factor.Edge.List, pos = 1)
                }
                tkadd(expGenMenu, "command", label = "Assign 'factorEdgeList' in .GlobalEnv", 
                  command = function() factorEdgesExport())
                tkadd(exportMenu, "command", label = "Assign 'factorEdgeList' in .GlobalEnv", 
                  command = function() factorEdgesExport())
                tkadd(expGenMenu, "command", label = "'print(asDataFrame(factorEdgeList))'", 
                  command = function() print(asDataFrame(factorEdgeList)))
                blockListExport <- function() {
                  ReturnVal <- modalDialog("Blocklist Name Entry", 
                    "Enter name for the blocklist", "blockList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  block.List <- blocksUpdate()
                  assign(ReturnVal, block.List, pos = 1)
                }
                tkadd(expBlockMenu, "command", label = "Assign 'blockList' in .GlobalEnv", 
                  command = function() blockListExport())
                tkadd(exportMenu, "command", label = "Assign 'blockList' in .GlobalEnv", 
                  command = function() blockListExport())
                tkadd(expBlockMenu, "command", label = "'print(asDataFrame(blockList))'", 
                  command = function() print(asDataFrame(blockList)))
                blockTreeExport <- function() {
                  ReturnVal <- modalDialog("Blocktree Name Entry", 
                    "Enter name for the blocktree", "blockTree", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  blocksUpdate()
                  assign(ReturnVal, blockTree, pos = 1)
                }
                tkadd(expBlockMenu, "command", label = "Assign 'blockTree' in .GlobalEnv", 
                  command = function() blockTreeExport())
                tkadd(exportMenu, "command", label = "Assign 'blockTree' in .GlobalEnv", 
                  command = function() blockTreeExport())
                blockEdgesExport <- function() {
                  ReturnVal <- modalDialog("BlockEdges Name Entry", 
                    "Enter name for the blockedges", "blockEdgeList", 
                    top = GraphWindow.top[[1]])
                  if (ReturnVal == "ID_CANCEL") 
                    return()
                  blockEdge.List <- blockEdgesUpdate()
                  assign(ReturnVal, blockEdge.List, pos = 1)
                }
                tkadd(expBlockMenu, "command", label = "Assign 'blockEdgeList' in .GlobalEnv", 
                  command = function() blockEdgesExport())
                tkadd(exportMenu, "command", label = "Assign 'blockEdgeList' in .GlobalEnv", 
                  command = function() blockEdgesExport())
                tkadd(expBlockMenu, "command", label = "'print(asDataFrame(blockEdgeList))'", 
                  command = function() print(asDataFrame(blockEdgeList)))
                tkadd(topMenu, "cascade", label = "Export", menu = exportMenu)
            }
            top <- NULL
            canvas <- NULL
            viewLabel <- NULL
            tags <- NULL
            updateCountVertices <- updateCountVerticesMain
            updateCountPositions <- updateCountPositionsMain
            updateCountBlocks <- updateCountBlocksMain
            updateCountBlockEdges <- updateCountBlockEdgesMain
            if (setUpdateCountModelMain) 
                updateCountModelMain <<- updateCountModelMain + 
                  1
            updateCountModel <- updateCountModelMain
            args <- list(...)
            extract <- function(x, y = x, z = "$", a = "graphComponents", 
                b = paste(a, z, y, collapse = "")) eval(parse(text = paste(c("if (is.null(", 
                x, ") && !is.null(", b, ")) ", x, " <<- ", b), 
                collapse = "")))
            if (!is.null(graphComponents)) {
                X <- matrix(c("viewType", "viewType", "oriented", 
                  "oriented", "edgeList", "vertexEdges", "edgeList", 
                  "graphEdges", "factorVertexList", "factorVertices", 
                  "factorEdgeList", "factorEdges", "blockEdgeList", 
                  "blockEdges", "extraEdgeList", "extraEdges", 
                  "visibleVertices", "visibleVertices", "extraList", 
                  "extraVertices"), ncol = 2, byrow = TRUE)
                if (is.list(graphComponents)) {
                  apply(X, 1, function(i) extract(i[1], i[2]))
                  if ((is.null(visibleBlocks)) && (!is.null(graphComponents$visibleBlocks) || 
                    (length(graphComponents$visibleBlocks) == 
                      0))) 
                    visibleBlocks <- graphComponents$visibleBlocks
                }
                else if (is.object(graphComponents)) {
                  apply(X[-2, ], 1, function(i) extract(i[1], 
                    i[2], z = "@"))
                  if ((is.null(visibleBlocks)) && (!is.null(graphComponents@visibleBlocks) || 
                    (length(graphComponents@visibleBlocks) == 
                      0))) 
                    visibleBlocks <- graphComponents@visibleBlocks
                }
            }
            Arguments <- args$Arguments
            if (!is.null(Arguments)) {
                X <- matrix(c("frameModels", "frameModels", "frameViews", 
                  "frameViews", "edgeList", "edgeList", "oriented", 
                  "oriented", "blockEdgeList", "blockEdgeList", 
                  "factorVertexList", "factorVertexList", "factorEdgeList", 
                  "factorEdgeList", "visibleVertices", "visibleVertices", 
                  "extraEdgeList", "extraEdgeList", "extraList", 
                  "extraList", "top", "top", "canvas", "canvas", 
                  "viewLabel", "viewLabel", "tags", "tags", "viewType", 
                  "viewType", "title", "title", "transformation", 
                  "transformation", "width", "width", "height", 
                  "height", "w", "w", "vertexColor", "vertexColor", 
                  "extraVertexColor", "extraVertexColor", "edgeColor", 
                  "edgeColor", "factorVertexColor", "factorVertexColor", 
                  "factorEdgeColor", "factorEdgeColor", "extraEdgeColor", 
                  "extraEdgeColor", "blockEdgeColor", "blockEdgeColor", 
                  "blockColors", "blockColors", "background", 
                  "background", "returnLink", "returnLink"), 
                  ncol = 2, byrow = TRUE)
                apply(X, 1, function(i) extract(i[1], i[2], a = "Arguments"))
                if ((is.null(visibleBlocks)) && (!is.null(Arguments$visibleBlocks) || 
                  (length(Arguments$visibleBlocks) == 0))) 
                  visibleBlocks <- Arguments$visibleBlocks
            }
            if (is.null(width)) 
                width <- 400
            if (is.null(height)) 
                height <- 400
            if (is.null(w)) 
                w <- 6
            if (is.null(oriented)) 
                oriented <- FALSE
            if (!is.null(args$vertexList)) {
                vertexList <<- args$vertexList
                replaceVertices(vertexList)
            }
            if (!is.null(args$blockList)) {
                blockList <<- args$blockList
                replaceBlocks(blockList)
            }
            if (is.null(args$blockList) && !is.null(args$blockTree)) {
                blockList <<- blockTreeToList(args$blockTree)
                replaceBlocks(blockList)
            }
            if (is.null(visibleVertices)) 
                visibleVertices <- 1:length(vertexList)
            if (is.null(visibleBlocks)) 
                visibleBlocks <- 1:length(blockList)
            zoomPositions <- NULL
            Scale <- 1
            zoomCenterSet <- FALSE
            zoomCenter <- NULL
            upperLeft <- c(min.x, min.y, rep(0, N - 2))
            lowerRight <- c(max.x, max.y, rep(100, N - 2))
            if (is.null(vertexColor)) 
                vertexColor <- "blue"
            if (is.null(extraVertexColor)) 
                extraVertexColor <- "black"
            if (is.null(edgeColor)) 
                edgeColor <- "blue"
            if (is.null(factorVertexColor)) 
                factorVertexColor <- "green"
            if (is.null(factorEdgeColor)) 
                factorEdgeColor <- "green"
            if (is.null(extraEdgeColor)) 
                extraEdgeColor <- "peru"
            if (is.null(blockEdgeColor)) 
                blockEdgeColor <- "default"
            if (is.null(blockColors)) 
                blockColors <- NULL
            if (is.null(background)) 
                background <- "white"
            if (is.null(extraList)) 
                extraList <- .emptyDgList("dg.VertexList")
            if (is.null(edgeList)) 
                edgeList <- .emptyDgList("dg.VertexEdgeList")
            if (is.null(factorEdgeList)) 
                factorEdgeList <- .emptyDgList("dg.FactorEdgeList")
            if (is.null(extraEdgeList)) 
                extraEdgeList <- .emptyDgList("dg.ExtraEdgeList")
            if (is.null(blockEdgeList)) 
                blockEdgeList <- .emptyDgList("dg.BlockEdgeList")
            if (redraw) 
                graphWindow <- NULL
            GraphWindow.top <- list(top)
            GraphWindow.canvas <- list(canvas)
            GraphWindow.viewLabel <- list(viewLabel)
            GraphWindow.tags <- tags
            if (is.numeric(frameViews)) 
                Frame.Views <- Frame.Models@models[[frameViews]]
            else Frame.Views <- frameViews
            if (is.null(graphWindow)) {
                ArgWindow <- FALSE
                m <- Frame.Views@index
                n <- 1
                if (!.IsEmpty(Frame.Models@models[[m]]@graphs)) 
                  n <- length(Frame.Models@models[[m]]@graphs) + 
                    1
                GraphWindow <- newGraph(viewType, visibleVertices, 
                  visibleBlocks, extraList, edgeList, blockEdgeList, 
                  factorVertexList, factorEdgeList, extraEdgeList, 
                  background = background, title = ifelse(title != 
                    "Default", title, paste(n, m, sep = "@")), 
                  index = n, width = width, height = height)
                if (returnLink) {
                  GraphWindow@top <- GraphWindow.top
                  GraphWindow@canvas <- GraphWindow.canvas
                  GraphWindow@viewLabel <- GraphWindow.viewLabel
                  GraphWindow@tags <- GraphWindow.tags
                }
            }
            else {
                ArgWindow <- TRUE
                if (is.numeric(graphWindow)) 
                  GraphWindow <- Frame.Views@graphs[[graphWindow]]
                else GraphWindow <- graphWindow
                if (!.IsEmpty(GraphWindow@top) && (length(GraphWindow@top) > 
                  0) && !is.null(GraphWindow@top[[1]])) {
                  GraphWindow.top <- GraphWindow@top
                  GraphWindow.canvas <- GraphWindow@canvas
                  GraphWindow.viewLabel <- GraphWindow@viewLabel
                  GraphWindow.tags <- GraphWindow@tags
                }
                m <- Frame.Views@index
                n <- graphWindow@index
                GraphWindow@id <- GraphWindow@id + 1
                if (!is.null(GraphWindow.top[[1]])) {
                  GraphWindow@visibleVertices <- .nullToEmpty(visibleVertices)
                  GraphWindow@visibleBlocks <- .nullToEmpty(visibleBlocks)
                  GraphWindow@extraVertices <- .nullToList(extraList, 
                    type = "dg.VertexList")
                  GraphWindow@vertexEdges <- edgeList
                  GraphWindow@factorVertices <- .nullToList(factorVertexList, 
                    type = "dg.FactorVertexList")
                  GraphWindow@factorEdges <- factorEdgeList
                  GraphWindow@extraEdges <- extraEdgeList
                  GraphWindow@blockEdges <- blockEdgeList
                  deleteTags()
                  GraphWindow.tags <- list(NULL)
                  if (returnLink) 
                    GraphWindow@tags <- GraphWindow.tags
                }
                else {
                  GraphWindow <- newGraph(viewType, visibleVertices, 
                    visibleBlocks, extraList, edgeList, blockEdgeList, 
                    factorVertexList, factorEdgeList, extraEdgeList, 
                    background = background, title = ifelse(title != 
                      "Default", title, paste(n, m, sep = "@")), 
                    id = GraphWindow@id, index = n, width = width, 
                    height = height)
                }
            }
            activatedNode <- list(number = 0, vertex.type = "Null")
            selectedNodes <- list()
            activatedEdge <- list(number = 0, edge.type = NULL)
            selectedEdges <- list()
            itemsFactors <- NULL
            itemsFactorEdges <- NULL
            namesFactorVertices <- NULL
            itemsExtraEdges <- NULL
            positionsFactorVertices <- NULL
            positionsFactorLabels <- NULL
            factorLabels <- NULL
            colorsFactorVertices <- NULL
            strataFactorVertices <- NULL
            blocksFactorVertices <- NULL
            if (FALSE && !is.null(factorVertexList)) {
                namesFactorVertices <- Names(factorVertexList)
                positionsFactorVertices <- Positions(factorVertexList)
                positionsFactorLabels <- NULL
                factorLabels <- Labels(factorVertexList)
                colorsFactorVertices <- Colors(factorVertexList)
                strataFactorVertices <- Strata(factorVertexList)
                blocksFactorVertices <- Blockindices(factorVertexList)
            }
            itemsExtras <- NULL
            itemsExtraEdges <- NULL
            namesExtraVertices <- NULL
            positionsExtraVertices <- NULL
            positionsExtraLabels <- NULL
            extraLabels <- NULL
            colorsExtraVertices <- NULL
            strataExtraVertices <- NULL
            blocksExtraVertices <- NULL
            if (FALSE && !is.null(extraList)) {
                namesExtraVertices <- Names(extraList)
                positionsExtraVertices <- Positions(extraList)
                positionsExtraLabels <- NULL
                extraLabels <- Labels(extraList)
                colorsExtraVertices <- Colors(extraList)
                strataExtraVertices <- Strata(extraList)
                blocksExtraVertices <- Blockindices(extraList)
            }
            Angle <- 10
            canvas <- GraphWindow.canvas[[1]]
            itemsEdges <- vector("list", length(vertexList))
            itemsVertices <- vector("list", length(vertexList))
            itemsOpenBlocks <- vector("list", length(blockList))
            itemsClosedBlocks <- vector("list", length(blockList))
            positionsEdgeLabels <- NULL
            closedVertex <- rep(FALSE, length(vertexList))
            closedBlock <- rep(FALSE, length(blockList))
            hiddenBlock <- rep(FALSE, length(blockList))
            if (!.IsEmpty(blockList)) {
                if (!is.null(Arguments) && !is.null(Arguments$closedBlock)) 
                  closedBlock <- Arguments$closedBlock
                else closedBlock <- Closed(blockList)
                hiddenBlock <- closedBlock
                for (i in seq(along = blockList)) hiddenBlock[i] <- isInClosedBlock(i)
                if (!is.null(Arguments) && !is.null(Arguments$hiddenBlock)) {
                  hiddenBlock <- hiddenBlock | Arguments$hiddenBlock
                }
                visibleBlocks <- unique(sort(visibleBlocks))
                visibleBlocks <- visibleBlocks[visibleBlocks != 
                  0]
                for (i in seq(along = vertexList)) {
                  s <- blocksVertices[i]
                  if (s %in% visibleBlocks) 
                    closedVertex[i] <- closedBlock[s] || hiddenBlock[s]
                }
                if (!is.null(visibleBlocks) && length(visibleBlocks) == 
                  0) 
                  hiddenBlock <- hiddenBlock
            }
            itemsBlockEdges <- list(NULL)
            if (!.IsEmpty(blockList)) {
                itemsBlockEdges <- vector("list", length(blockList))
            }
            initFactorVariables(factorVertexList)
            initExtraVariables(extraList)
            if (!.IsEmpty(blockList)) 
                for (i in seq(along = blockList)) if (is.na(hiddenBlock[i])) 
                  message("hiddenBlock missing!!")
                else if (!hiddenBlock[i]) 
                  if (is.element(i, visibleBlocks)) 
                    if (is.na(closedBlock[i])) 
                      message("closedBlock missing!!")
                    else if (closedBlock[i]) 
                      drawVertex(i, w = 10, vertexcolor = "Black", 
                        vertex.type = "ClosedBlock")
                    else drawBlock(blockList[[i]], i)
            for (i in seq(along = edgeList)) drawEdge(edgeList[[i]], 
                i, edge.type = "VertexEdge")
            for (i in seq(along = factorEdgeList)) drawEdge(factorEdgeList[[i]], 
                i, edge.type = "FactorEdge")
            for (i in seq(along = extraEdgeList)) drawEdge(extraEdgeList[[i]], 
                i, edge.type = "ExtraEdge")
            for (i in seq(along = blockEdgeList)) drawEdge(blockEdgeList[[i]], 
                i, edge.type = "BlockEdge")
            for (i in seq(along = vertexList)) if (is.element(i, 
                visibleVertices)) 
                if (!closedVertex[i]) 
                  drawVertex(i, w = w, vertexcolor = vertexColor, 
                    vertex.type = "Vertex")
            if (length(factorVertexList) > 0) 
                for (i in seq(along = factorVertexList)) drawVertex(-i, 
                  w = w, vertexcolor = vertexColor, vertex.type = "Factor")
            if (length(extraList) > 0) 
                for (i in seq(along = extraList)) drawVertex(i, 
                  w = w, vertexcolor = vertexColor, vertex.type = "Extra")
            if (initialWindow) 
                update.edge.labels()
            setMainMenu()
            tkbind(canvas, "<B3-Motion>", doHandRotate())
            tkbind(canvas, "<F12>", rockPlot(k = 1))
            tkbind(canvas, "<F1>", zoomIn())
            tkbind(canvas, "<F2>", zoomOut())
            tkbind(canvas, "<F3>", newVertexXY())
            tkbind(canvas, "<F4>", newBlockXY())
            tkbind(canvas, "<F5>", addLastEdge())
            tkbind(canvas, "<F6>", deleteAllEdgeLabels())
            tkbind(canvas, "<F7>", function(...) {
                print("<<F7>>")
            })
            tkbind(canvas, "<F12>", function(...) {
                print("<<F12>>")
            })
            tkbind(canvas, "<Up>", keyRotate(v = 1, sign = 1))
            tkbind(canvas, "<Down>", keyRotate(v = 1, sign = -1))
            tkbind(canvas, "<Left>", keyRotate(v = 2, sign = 1))
            tkbind(canvas, "<Right>", keyRotate(v = 2, sign = -1))
            tkbind(canvas, "<Prior>", keyRotate(v = 3, sign = 1))
            tkbind(canvas, "<Next>", keyRotate(v = 3, sign = -1))
            tkbind(canvas, "<Destroy>", function() tkdestroy(GraphWindow.top[[1]]))
            tkbind(canvas, "<Home>", function(...) {
                print("<<Fn-PgUp>>")
            })
            tkbind(canvas, "<End>", function(...) {
                print("<<Fn-PgDn>>")
            })
            tkbind(canvas, "<Alt_L>", function(...) {
                print("<<A>>")
            })
            tkbind(canvas, "<Delete>", function(...) {
                print("<<Delete>>")
            })
            tkbind(canvas, "<Pause>", function(...) {
                print("<<Pause>>")
            })
            tkbind(canvas, "<space>", function(...) {
                print("<<space>>")
            })
            tkbind(canvas, "<Tab>", function(...) {
                print("<<Tab>>")
            })
            tkbind(canvas, "<slash>", function(...) {
                print("<<slash>>")
            })
            tkbind(canvas, "<less>", function(...) {
                print("<<less>>")
            })
            tkbind(canvas, "<greater>", function(...) {
                print("<<greater>>")
            })
            tkbind(canvas, "<Escape>", function(...) {
                print("<<Escape>>")
            })
            tkbind(canvas, "<Shift-Up>", function(...) {
                print("<<Shift-Up>>")
            })
            tkbind(canvas, "<Shift-Down>", function(...) {
                print("<<Shift-Down>>")
            })
            tkbind(canvas, "<minus>", function(...) {
                print("<<minus>>")
            })
            tkbind(canvas, "<plus>", function(...) {
                print("<<plus>>")
            })
            tkbind(canvas, "<backslash>", function(...) {
                print("--backslash--")
            })
            tkbind(canvas, "<Alt-1>", function(...) {
                print("--A1#--")
            })
            if (debug.update) 
                tkbind(GraphWindow.viewLabel[[1]], "<Enter>", 
                  function() print(viewType))
            tkbind(canvas, "<Configure>", resizeCanvas())
            if (enterLeaveUpdate) {
                tkbind(canvas, "<Enter>", updatePositions("Enter"))
                if (updateAllViews) 
                  tkbind(canvas, "<Leave>", UpdateAllFrames("Positions", 
                    "Leave"))
                else tkbind(canvas, "<Leave>", updatePositions("Leave"))
            }
            m <- Frame.Views@index
            if (as.logical(returnLink) && (as.numeric(returnLink) < 
                2)) {
                GraphWindow@update <- update
            }
            if (!redraw) 
                if (!ArgWindow) {
                  if (.IsEmpty(Frame.Models@models[[m]]@graphs)) {
                    Frame.Views@graphs <<- list(GraphWindow)
                    Frame.Models@models[[m]] <<- Frame.Views
                  }
                  else {
                    Frame.Views@graphs <<- append(Frame.Models@models[[m]]@graphs, 
                      list(GraphWindow))
                    Frame.Models@models[[m]] <<- Frame.Views
                  }
                }
                else {
                  Frame.Views@graphs[[n]] <<- GraphWindow
                  Frame.Models@models[[m]]@graphs[[n]] <<- GraphWindow
                }
            if (returnNull) 
                return(NULL)
            else if (returnFrameModel) 
                invisible(Frame.Models)
            else invisible(GraphWindow)
        }
        updateCountModelMain <- 0
        if (is.numeric(frameViews)) 
            Frame.Views <- Frame.Models@models[[frameViews]]
        else Frame.Views <- frameViews
        if (is.null(Frame.Views)) {
            ArgFrameViews <- FALSE
            m <- 1
            if (!.IsEmpty(Frame.Models@models)) 
                m <- length(Frame.Models@models) + 1
            Title <- title
            if (is.object(object) && ("name" %in% slotNames(object))) 
                Title <- paste(title, object@name)
            Frame.Views <- .newDynamicGraphModelObject(object, 
                title = Title, index = m)
            if (as.logical(returnLink) && (as.numeric(returnLink) < 
                2)) {
                Frame.Views@redrawView <- redrawView
                message(paste("Since 'returnLink = TRUE' the workspace saved", 
                  "with the returned object will occupy space !!!"))
            }
            else message("(( No 'update' and 'overwrite' since 'returnLink = FALSE'))")
        }
        else {
            ArgFrameViews <- TRUE
            m <- Frame.Views@index
            object <- Frame.Views@model[[1]]
        }
        if (returnNewMaster || redraw) {
            graphs <- Frame.Views@graphs
            if (!redraw) 
                Frame.Views@graphs <- list()
            if (length(graphs) > 0) 
                for (i in (1:length(graphs))) {
                  R <- redrawView(frameModels = Frame.Models, 
                    frameViews = Frame.Views, graphWindow = graphs[[i]], 
                    edgeList = graphs[[i]]@vertexEdges, oriented = oriented, 
                    blockEdgeList = graphs[[i]]@blockEdges, factorVertexList = graphs[[i]]@factorVertices, 
                    factorEdgeList = graphs[[i]]@factorEdges, 
                    visibleVertices = graphs[[i]]@visibleVertices, 
                    visibleBlocks = graphs[[i]]@visibleBlocks, 
                    extraList = graphs[[i]]@extraVertices, extraEdgeList = graphs[[i]]@extraEdges, 
                    viewType = class(graphs[[i]]), title = title, 
                    transformation = transformation, width = width, 
                    height = height, w = w, vertexColor = vertexColor, 
                    extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                    factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                    blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                    extraEdgeColor = extraEdgeColor, background = background, 
                    returnNewMaster = returnNewMaster, redraw = redraw, 
                    returnLink = returnLink, returnNull = returnNull, 
                    returnFrameModel = FALSE, ...)
                }
        }
        if (!redraw) 
            if (!ArgFrameViews) {
                if (.IsEmpty(Frame.Models@models)) 
                  Frame.Models@models <<- list(Frame.Views)
                else Frame.Models@models <<- append(Frame.Models@models, 
                  list(Frame.Views))
            }
            else {
                Frame.Models@models[[m]] <<- Frame.Views
            }
        if (!redraw && !returnNewMaster) {
            Graph..Window <- redrawView(frameModels = Frame.Models, 
                frameViews = Frame.Views, graphWindow = graphWindow, 
                edgeList = edgeList, oriented = oriented, blockEdgeList = blockEdgeList, 
                factorVertexList = factorVertexList, factorEdgeList = factorEdgeList, 
                visibleVertices = visibleVertices, visibleBlocks = visibleBlocks, 
                extraList = extraList, extraEdgeList = extraEdgeList, 
                viewType = viewType, title = title, transformation = transformation, 
                width = width, height = height, w = w, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, background = background, 
                initialWindow = initialWindow, returnNewMaster = FALSE, 
                redraw = FALSE, returnLink = returnLink, returnNull = returnNull, 
                returnFrameModel = FALSE, ...)
        }
        if (returnFrameModel) 
            invisible(Frame.Models)
        else invisible(Frame.Views)
    }
    Arguments <- list(...)
    Frame.Models <- NULL
    if (!is.null(Arguments$Frame.Models)) 
        Frame.Models <- Arguments$Frame.Models
    if (!is.null(Arguments$frameModels)) 
        Frame.Models <- Arguments$frameModels
    returnNewMaster <- FALSE
    if (!is.null(Arguments$returnNewMaster)) 
        returnNewMaster <- Arguments$returnNewMaster
    redraw <- FALSE
    if (!is.null(Arguments$redraw)) 
        redraw <- Arguments$redraw
    updateCountVerticesMain <- 0
    updateCountPositionsMain <- 0
    updateCountBlocksMain <- 0
    updateCountBlockEdgesMain <- 0
    if (is.null(Frame.Models)) {
        Frame.Models <- .newDynamicGraphObject(vertexList, blocks = blockList, 
            blockTree = blockTree, title = title)
        if (as.logical(returnLink) && (as.numeric(returnLink) < 
            2)) 
            Frame.Models@drawModel <- drawModel
    }
    else {
    }
    if (redraw) {
        vertexList <- Frame.Models@vertices
        blockList <- Frame.Models@blocks
        if (length(blockList) == 0) 
            blockList <- NULL
        blockTree <- Frame.Models@blockTree
    }
    namesVertices <- Names(vertexList)
    positionsVertices <- Positions(vertexList)
    if (is.matrix(positionsVertices)) {
        positionsLabels <- positionsVertices
        positionsLabels[, 1] <- positionsLabels[, 1] + 0.1 * 
            w
        Labels <- Labels(vertexList)
        colorsVertices <- Colors(vertexList)
        blocksVertices <- Blockindices(vertexList)
        strataVertices <- Strata(vertexList)
        N <- ncol(positionsVertices)
        if (is.null(blockList) && !is.null(blockTree)) 
            blockList <- blockTreeToList(blockTree)
        positionsBlock <- NULL
        positionsBlockLabels <- NULL
        positionsClosedBlocks <- NULL
        blockLabels <- NULL
        strataBlocks <- NULL
        if (!.IsEmpty(blockList)) {
            positionsBlocks <- Positions(blockList)
            d <- dim(positionsBlocks)
            positionsBlocks <- array(positionsBlocks, dim = c(d[1], 
                d[2]/2, 2))
            positionsClosedBlocks <- matrix(rep(NA, N * length(blockList)), 
                ncol = N)
            positionsClosedBlocks <- apply(positionsBlocks, c(1, 
                2), mean)
            blockReferences <- 1:length(blockList)
            positionsBlockLabels <- matrix(rep(0, N * length(blockList)), 
                ncol = N)
            blockLabels <- Labels(blockList)
            strataBlocks <- Strata(blockList)
        }
        if (returnNewMaster || redraw) {
            models <- Frame.Models@models
            if (!redraw) 
                Frame.Models@models <- list()
            for (i in (1:length(models))) {
                frameViews <- models[[i]]
                object <- models[[i]]@model
                R <- drawModel(frameModels = Frame.Models, frameViews = frameViews, 
                  graphWindow = NULL, oriented = oriented, object = object, 
                  viewType = NULL, title = title, transformation = transformation, 
                  width = width, height = height, w = w, vertexColor = vertexColor, 
                  extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                  factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                  blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                  extraEdgeColor = extraEdgeColor, background = background, 
                  initialWindow = TRUE, returnNewMaster = returnNewMaster, 
                  redraw = redraw, returnLink = returnLink, returnNull = returnNull, 
                  returnFrameModel = FALSE)
            }
        }
        if (!redraw) {
            Frame..Views <- drawModel(frameModels = Frame.Models, 
                frameViews = NULL, graphWindow = NULL, edgeList = edgeList, 
                oriented = oriented, blockEdgeList = blockEdgeList, 
                factorVertexList = factorVertexList, factorEdgeList = factorEdgeList, 
                visibleVertices = visibleVertices, visibleBlocks = visibleBlocks, 
                extraList = extraList, extraEdgeList = extraEdgeList, 
                object = object, viewType = viewType, title = title, 
                transformation = transformation, width = width, 
                height = height, w = w, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, background = background, 
                initialWindow = TRUE, returnNewMaster = FALSE, 
                redraw = FALSE, returnLink = returnLink, returnNull = returnNull, 
                returnFrameModel = FALSE)
        }
    }
    else {
        Frame.Models <- NULL
        warning("Positions of vertices should have same number of coordinates")
    }
    if (returnNull) 
        return(NULL)
    else invisible(Frame.Models)
}
