"DynamicGraph" <-
function (names = NULL, types = NULL, from = NULL, to = NULL, 
    edge.types = NULL, edge.list = NULL, labels = names, blocks = NULL, 
    block.tree = NULL, oriented = FALSE, factors = NULL, texts = NULL, 
    extra.from = NULL, extra.to = NULL, extra.edge.list = NULL, 
    object = NULL, viewType = "Simple", vertexClasses = validVertexClasses(), 
    factorClasses = validFactorClasses(), edgeClasses = validEdgeClasses(), 
    viewClasses = validViewClasses(), N = 3, drawblocks = TRUE, 
    right.to.left = FALSE, nested.blocks = FALSE, overlaying = TRUE, 
    vertexColor = "red", extraVertexColor = "white", edgeColor = "black", 
    factorVertexColor = "default", factorEdgeColor = "brown", 
    blockEdgeColor = "default", blockColors = NULL, extraEdgeColor = "peru", 
    frameModels = NULL, frameViews = NULL, graphWindow = NULL, 
    addModel = FALSE, addView = FALSE, overwrite = FALSE, returnNewMaster = FALSE, 
    redraw = FALSE, ...) 
{
    two.to.pairs <- function(from, to) {
        edge.list <- vector("list", length(to))
        for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
            to[j])
        return(edge.list)
    }
    check.names <- function(x, label) if (length(x) > 0) 
        if (!is.numeric(x)) {
            x <- unique(sort(x))
            x.indices <- match(x, names)
            if (any(is.na(x.indices))) {
                new.names <- x[is.na(x.indices)]
                warning(paste("Invalid names in '", label, "' : ", 
                  paste(new.names, collapse = ", ")))
                names <<- c(names, new.names)
                types <<- c(types, rep(types[1], length(new.names)))
                vertex.colors <<- c(vertex.colors, rep("magenta", 
                  length(new.names)))
            }
        }
        else if (max(x) > length(names)) {
            warning(paste("Invalid index", max(x), "in '", label, 
                "'"))
            new.names <- paste("v", (length(names) + 1):max(x), 
                sep = "")
            names <<- c(names, new.names)
            types <<- c(types, rep(types[1], length(new.names)))
            vertex.colors <<- c(vertex.colors, rep("magenta", 
                length(new.names)))
        }
    Arguments <- list(...)
    Vertices <- NULL
    if (!is.null(frameModels)) {
        Vertices <- frameModels@vertices
        names <- Names(Vertices)
    }
    else if (!is.null(Arguments$Vertices)) {
        Vertices <- Arguments$Vertices
        names <- Names(Vertices)
    }
    n <- length(names)
    if (is.numeric(names)) 
        names <- paste(names)
    if (is.null(types)) 
        types <- rep("Discrete", n)
    if (!is.null(types) && (length(types) == 1)) 
        types <- rep(types, n)
    if (n != length(types)) 
        stop("Invalid data: Length of types")
    if (length(from) != length(to)) 
        stop("Invalid data: Length of from and to")
    vertex.colors <- rep(vertexColor, length(names))
    check.names(from, "from")
    check.names(to, "to")
    check.names(unlist(edge.list), "edge.list")
    check.names(unlist(factors), "factors")
    if (!is.null(blocks)) {
        names.blocks <- unlist(blocks)
        names.blocks <- names.blocks[grep("Vertices", names(names.blocks))]
        suppressWarnings(x <- as.numeric(names.blocks))
        if (!any(is.na(x))) 
            names.blocks <- x
        if (length(names.blocks) > 0) 
            warning("Did you give a block.tree for blocks?")
        check.names(unlist(blocks), "blocks")
    }
    if (!is.null(block.tree)) {
        names.block.tree <- unlist(block.tree)
        names.block.tree <- names.block.tree[grep("Vertices", 
            names(names.block.tree))]
        suppressWarnings(x <- as.numeric(names.block.tree))
        if (!any(is.na(x))) 
            names.block.tree <- x
        check.names(names.block.tree, "block.tree")
    }
    if (is.null(Vertices)) 
        Vertices <- returnVertexList(names, labels = labels, 
            types = types, N = N, colors = vertex.colors, vertexClasses = vertexClasses)
    X <- c("edgeList", "blockEdgeList", "blockList", "blockTree", 
        "factorVertexList", "factorEdgeList", "extraList", "extraEdgeList")
    if (any(X %in% names(list(...)))) {
        if (!is.null(from)) 
            warning("Argument 'from      ' ignored")
        if (!is.null(to)) 
            warning("Argument 'to        ' ignored")
        if (!is.null(edge.types)) 
            warning("Argument 'edge.types' ignored")
        if (!is.null(edge.list)) 
            warning("Argument 'edge.list ' ignored")
        if (!is.null(blocks)) 
            warning("Argument 'blocks    ' ignored")
        if (!is.null(block.tree)) 
            warning("Argument 'block.tree' ignored")
        if (!is.null(factors)) 
            warning("Argument 'factors   ' ignored")
        if (!is.null(texts)) 
            warning("Argument 'texts     ' ignored")
        if (addModel) {
            if (is.null(frameModels)) 
                warning("'frameModels' should be set;")
            if (is.object(frameModels)) {
                drawModel <- frameModels@drawModel
                if (length(formals(drawModel)) == 0) 
                  drawModel <- Arguments$Arguments$drawModel
            }
            else drawModel <- frameModels$drawModel
            if (!overwrite) {
                frameViews <- NULL
                graphWindow <- NULL
            }
            else {
                if (!is.null(frameViews)) 
                  frameViews@model <- list(object)
            }
            if (length(formals(drawModel)) == 0) {
                warning("Invalid 'drawModel' of argument")
            }
            else drawModel(frameModels = frameModels, frameViews = frameViews, 
                graphWindow = graphWindow, oriented = oriented, 
                object = object, viewType = viewType, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, ...)
        }
        else if (addView) {
            if (is.null(frameModels)) 
                warning("'frameModels' should be set;")
            if (!is.null(object)) 
                warning("'object' ignored!  Did you mean 'addModel'? ; ")
            if (is.object(frameViews)) {
                redrawView <- frameViews@redrawView
                if (length(formals(redrawView)) == 0) 
                  redrawView <- Arguments$Arguments$redrawView
            }
            else redrawView <- frameViews$redrawView
            if (!overwrite) {
                graphWindow <- NULL
            }
            else {
            }
            if (length(formals(redrawView)) == 0) {
                warning("Invalid 'redrawView' of argument")
            }
            else redrawView(frameModels = frameModels, frameViews = frameViews, 
                graphWindow = graphWindow, oriented = oriented, 
                viewType = viewType, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, ...)
        }
        else {
            if (!redraw && !returnNewMaster) 
                frameModels <- NULL
            dynamicGraphMain(Vertices, oriented = oriented, object = object, 
                viewType = viewType, vertexClasses = vertexClasses, 
                factorClasses = factorClasses, edgeClasses = edgeClasses, 
                viewClasses = viewClasses, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, ...)
        }
    }
    else {
        BlockList <- NULL
        BlockTree <- NULL
        if (!is.null(frameModels)) {
            BlockList <- frameModels@blocks
            if (!is.null(BlockList) && ((length(BlockList) == 
                0) || is.null(BlockList[[1]]))) 
                BlockList <- NULL
        }
        else if (!is.null(Arguments$BlockList)) {
            BlockList <- Arguments$BlockList
            if (is.null(Arguments$Vertices)) 
                warning("Argument Vertices should also be given to put verices in blocks!")
        }
        else if (!is.null(frameModels)) 
            BlockTree <- frameModels@blockTree
        else if (!is.null(Arguments$BlockTree)) {
            BlockTree <- Arguments$BlockTree
            if (is.null(Arguments$Vertices)) 
                warning("Argument Vertices should also be given to put verices in blocks!")
        }
        else if (!(is.null(blocks))) {
            result <- setBlocks(blocks, Vertices, right.to.left = right.to.left, 
                nested.blocks = nested.blocks, blockColors = blockColors, 
                N = N)
            if (is.null(Arguments$Vertices)) 
                Vertices <- result$Vertices
            if (drawblocks) 
                BlockList <- result$Blocks
        }
        else if (!(is.null(block.tree))) {
            result <- setTreeBlocks(block.tree, Vertices, root.label = "", 
                N = N, blockColors = blockColors, overlaying = overlaying)
            if (is.null(Arguments$Vertices)) 
                Vertices <- result$Vertices
            if (drawblocks) 
                BlockTree <- result$BlockTree
        }
        if (!addModel && !addView && !is.null(graphWindow)) {
            ExtraVertices = graphWindow@extraVertices
            Edges = graphWindow@vertexEdges
            BlockEdges = graphWindow@blockEdges
            FactorVertices = graphWindow@factorVertices
            FactorEdges = graphWindow@factorEdges
            ExtraEdges = graphWindow@extraEdges
            if (length(ExtraVertices) > 0) 
                if (is.null(ExtraVertices[[1]])) 
                  ExtraVertices <- NULL
            if (length(Edges) > 0) 
                if (is.null(Edges[[1]])) 
                  Edges <- NULL
            if (length(BlockEdges) > 0) 
                if (is.null(BlockEdges[[1]])) 
                  BlockEdges <- NULL
            if (length(FactorVertices) > 0) 
                if (is.null(FactorVertices[[1]])) 
                  FactorVertices <- NULL
            if (length(FactorEdges) > 0) 
                if (is.null(FactorEdges[[1]])) 
                  FactorEdges <- NULL
            if (length(ExtraEdges) > 0) 
                if (is.null(ExtraEdges[[1]])) 
                  ExtraEdges <- NULL
        }
        else {
            if (!is.null(Arguments$ExtraVertices)) 
                ExtraVertices <- Arguments$ExtraVertices
            else if (is.null(texts)) 
                ExtraVertices <- NULL
            else ExtraVertices <- returnVertexList(paste("T", 
                1:length(texts), sep = ""), labels = texts, types = rep("TextVertex", 
                length(texts)), line = TRUE, N = N, colors = rep(extraVertexColor, 
                length(texts)), vertexClasses = vertexClasses)
            if (!is.null(ExtraVertices) && !is.null(Arguments$diagonal)) {
                i <- 1:length(ExtraVertices) - 2
                Positions(ExtraVertices) <- (matrix(c(rep(i * 
                  50, 2), rep(200, 11)), ncol = 3) - 200)/4
            }
            if (!is.null(Arguments$ExtraEdges)) 
                ExtraEdges <- Arguments$ExtraEdges
            else {
                ExtraEdges <- NULL
                if (is.null(extra.edge.list)) 
                  extra.edge.list <- two.to.pairs(extra.from, 
                    extra.to)
                if (!(is.null(extra.edge.list))) 
                  ExtraEdges <- returnExtraEdgeList(extra.edge.list, 
                    Vertices, ExtraVertices, color = extraEdgeColor)
            }
            FactorVertices <- NULL
            FactorEdges <- NULL
            if (!is.null(Arguments$FactorVertices)) {
                FactorVertices <- Arguments$FactorVertices
                if (is.null(Arguments$FactorEdges)) 
                  warning("Argument FactorEdges should also be given with FactorVertices!")
            }
            if (!(is.null(factors))) {
                result <- returnFactorVerticesAndEdges(Vertices, 
                  factors, factorVertexColor = factorVertexColor, 
                  factorEdgeColor = factorEdgeColor, factorClasses = factorClasses)
                if (!is.null(Arguments$FactorVertices)) {
                  FactorVertices <- Arguments$FactorVertices
                }
                else FactorVertices <- result$FactorVertices
                if (!is.null(Arguments$FactorEdges)) 
                  FactorEdges <- Arguments$FactorEdges
                else FactorEdges <- result$FactorEdges
                if ((is.null(from))) {
                  from <- result$PairEdges[, 1]
                  to <- result$PairEdges[, 2]
                }
            }
            if (is.null(edge.list)) 
                edge.list <- two.to.pairs(from, to)
            if (!is.null(Arguments$Edges)) 
                Edges <- Arguments$Edges
            else Edges <- returnEdgeList(edge.list, Vertices, 
                color = edgeColor, oriented = oriented, N = N, 
                types = edge.types, edgeClasses = edgeClasses)
            BlockEdges <- NULL
            if (!is.null(Arguments$BlockEdges)) 
                BlockEdges <- Arguments$BlockEdges
            else if (TRUE && (!is.null(BlockList) || !is.null(BlockTree))) {
                if (!(is.null(factors))) 
                  message("Edges between blocks and factors not implemented!")
                if (is.null(BlockList) && !is.null(BlockTree)) 
                  BlockList <- blockTreeToList(BlockTree)
                if (.IsEmpty(BlockList)) 
                  BlockEdges <- NULL
                else BlockEdges <- returnBlockEdgeList(edge.list, 
                  Vertices, BlockList, color = blockEdgeColor, 
                  oriented = oriented)
            }
        }
        if (addModel) {
            if (is.null(frameModels)) 
                warning("'frameModels' should be set;")
            if (is.object(frameModels)) {
                drawModel <- frameModels@drawModel
                if (length(formals(drawModel)) == 0) 
                  drawModel <- Arguments$Arguments$drawModel
            }
            else drawModel <- frameModels$drawModel
            if (!overwrite) {
                frameViews <- NULL
                graphWindow <- NULL
            }
            if (length(formals(drawModel)) == 0) {
                warning("Invalid 'drawModel' of argument")
            }
            else drawModel(frameModels = frameModels, frameViews = frameViews, 
                graphWindow = graphWindow, edgeList = Edges, 
                blockEdgeList = BlockEdges, oriented = oriented, 
                factorVertexList = FactorVertices, factorEdgeList = FactorEdges, 
                extraList = ExtraVertices, extraEdgeList = ExtraEdges, 
                object = object, viewType = viewType, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, ...)
        }
        else if (addView) {
            if (is.null(frameModels)) 
                warning("'frameModels' should be set;")
            if (is.object(frameViews)) {
                redrawView <- frameViews@redrawView
                if (length(formals(redrawView)) == 0) 
                  redrawView <- Arguments$Arguments$redrawView
            }
            else redrawView <- frameViews$redrawView
            if (length(formals(redrawView)) == 0) {
                warning("Invalid 'redrawView' of argument")
            }
            else redrawView(frameModels = frameModels, frameViews = frameViews, 
                edgeList = Edges, blockEdgeList = BlockEdges, 
                oriented = oriented, factorVertexList = FactorVertices, 
                factorEdgeList = FactorEdges, extraList = ExtraVertices, 
                extraEdgeList = ExtraEdges, object = object, 
                viewType = viewType, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, ...)
        }
        else {
            if (!redraw && !returnNewMaster) 
                frameModels <- NULL
            dynamicGraphMain(Vertices, edgeList = Edges, blockList = BlockList, 
                blockEdgeList = BlockEdges, blockTree = BlockTree, 
                oriented = oriented, factorVertexList = FactorVertices, 
                factorEdgeList = FactorEdges, extraList = ExtraVertices, 
                extraEdgeList = ExtraEdges, object = object, 
                viewType = viewType, vertexClasses = vertexClasses, 
                factorClasses = factorClasses, edgeClasses = edgeClasses, 
                viewClasses = viewClasses, vertexColor = vertexColor, 
                extraVertexColor = extraVertexColor, edgeColor = edgeColor, 
                factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
                blockEdgeColor = blockEdgeColor, blockColors = blockColors, 
                extraEdgeColor = extraEdgeColor, frameModels = frameModels, 
                returnNewMaster = returnNewMaster, redraw = redraw, 
                ...)
        }
    }
}
