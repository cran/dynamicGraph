"DynamicGraph" <-
function (names = NULL, types = NULL, from = NULL, to = NULL, 
    edge.list = NULL, blocks = NULL, block.tree = NULL, oriented = FALSE, 
    factors = NULL, texts = NULL, object = NULL, vertexClasses = validVertexClasses(), 
    factorClasses = validFactorClasses(), N = 3, drawblocks = TRUE, 
    right.to.left = FALSE, nested.blocks = FALSE, overlaying = TRUE, 
    vertexColor = "red", extraVertexColor = "white", edgeColor = "black", 
    factorVertexColor = "default", factorEdgeColor = "brown", 
    blockEdgeColor = "default", blockColors = NULL, ...) 
{
    Arguments <- list(...)
    two.to.pairs <- function(from, to) {
        edge.list <- vector("list", length(to))
        for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
            to[j])
        return(edge.list)
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
    check.names(from, "from")
    check.names(to, "to")
    check.names(unlist(edge.list), "edge.list")
    check.names(unlist(factors), "factors")
    check.names(unlist(blocks), "blocks")
    if (!is.null(block.tree)) {
        names.block.tree <- unlist(block.tree)
        names.block.tree <- names.block.tree[grep("Vertices", 
            names(names.block.tree))]
        suppressWarnings(x <- as.numeric(names.block.tree))
        if (!any(is.na(x))) 
            names.block.tree <- x
        check.names(names.block.tree, "block.tree")
    }
    Vertices <- returnVertexList(names, types = types, N = N, 
        colors = vertex.colors, vertexClasses = vertexClasses)
    if (is.null(texts)) 
        ExtraVertices <- NULL
    else ExtraVertices <- returnVertexList(paste("T", 1:length(texts), 
        sep = ""), labels = texts, types = rep("TextVertex", 
        length(texts)), line = TRUE, N = N, colors = rep(extraVertexColor, 
        length(texts)), vertexClasses = vertexClasses)
    BlockList <- NULL
    BlockTree <- NULL
    BlockEdges <- NULL
    if (!(is.null(blocks))) {
        result <- setBlocks(blocks, Vertices, right.to.left = right.to.left, 
            nested.blocks = nested.blocks, blockColors = blockColors, 
            N = N)
        Vertices <- result$Vertices
        if (drawblocks) 
            BlockList <- result$Blocks
    }
    else if (!(is.null(block.tree))) {
        result <- setTreeBlocks(block.tree, Vertices, root.label = "", 
            N = N, blockColors = blockColors, overlaying = overlaying)
        Vertices <- result$Vertices
        if (drawblocks) 
            BlockTree <- result$BlockTree
    }
    FactorVertices <- NULL
    FactorEdges <- NULL
    if (!(is.null(factors))) {
        result <- returnFactorVerticesAndEdges(Vertices, factors, 
            factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
            factorClasses = factorClasses)
        FactorVertices <- result$FactorVertices
        FactorEdges <- result$FactorEdges
        if ((is.null(from))) {
            from <- result$PairEdges[, 1]
            to <- result$PairEdges[, 2]
        }
    }
    if (is.null(edge.list)) 
        edge.list <- two.to.pairs(from, to)
    Edges <- returnEdgeList(edge.list, Vertices, color = edgeColor, 
        oriented = oriented)
    if (TRUE && (!is.null(BlockList) || !is.null(BlockTree))) {
        if (!(is.null(factors))) 
            message("Edges between blocks and factors not implemented!")
        if (is.null(BlockList) && !is.null(BlockTree)) 
            BlockList <- blockTreeToList(BlockTree)
        BlockEdges <- returnBlockEdgeList(edge.list, Vertices, 
            BlockList, color = blockEdgeColor, oriented = oriented)
    }
    dynamicGraphMain(Vertices, edgeList = Edges, blockList = BlockList, 
        blockEdgeList = BlockEdges, blockTree = BlockTree, oriented = oriented, 
        factorVertexList = FactorVertices, factorEdgeList = FactorEdges, 
        extraList = ExtraVertices, object = object, vertexClasses = vertexClasses, 
        vertexColor = vertexColor, extraVertexColor = extraVertexColor, 
        edgeColor = edgeColor, factorVertexColor = factorVertexColor, 
        factorEdgeColor = factorEdgeColor, blockEdgeColor = blockEdgeColor, 
        blockColors = blockColors, ...)
}
