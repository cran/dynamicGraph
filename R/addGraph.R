"addGraph" <-
function (linkDynamicGraph, from = NULL, to = NULL, edge.list = NULL, 
    Object = NULL, slave = FALSE, oriented = FALSE, edgeColor = "black", 
    factorVertexColor = "default", factorEdgeColor = "brown", 
    blockEdgeColor = "default", title = "Add", returnLink = FALSE, 
    ...) 
{
    if (!((names(linkDynamicGraph)[1] == "redrawGraphWindow") && 
        (class(linkDynamicGraph[[1]]) == "function"))) 
        stop("Invalid reference to master graph")
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
    Vertices <- linkDynamicGraph$vertexList
    names <- Names(Vertices)
    n <- length(names)
    if (is.numeric(names)) 
        names <- paste(names)
    if (length(from) != length(to)) 
        stop("Invalid data: Length of from and to")
    check.names(from, "from")
    check.names(to, "to")
    if (is.null(edge.list)) 
        edge.list <- two.to.pairs(from, to)
    factors <- NULL
    FactorVertices <- NULL
    FactorEdges <- NULL
    if (!(is.null(factors))) {
        message("Not tested!")
        result <- returnFactorVerticesAndEdges(Vertices, factors, 
            factorVertexColor = factorVertexColor, factorEdgeColor = factorEdgeColor, 
            factorClasses = factorClasses)
        FactorVertices <- result$FactorVertices
        FactorEdges <- result$FactorEdges
        if ((is.null(edge.list))) {
            from <- result$PairEdges[, 1]
            to <- result$PairEdges[, 2]
            edge.list <- two.to.pairs(from, to)
        }
    }
    edgeList <- returnEdgeList(edge.list, Vertices, color = edgeColor, 
        oriented = oriented)
    BlockList <- linkDynamicGraph$blockList
    BlockTree <- linkDynamicGraph$blockTree
    BlockEdges <- NULL
    if ((!is.null(BlockList) || !is.null(BlockTree))) {
        message("Not tested!")
        if (!(is.null(factors))) 
            message("Edges between blocks and factors not implemented!")
        if (is.null(BlockList) && !is.null(BlockTree)) 
            BlockList <- blockTreeToList(BlockTree)
        BlockEdges <- returnBlockEdgeList(edge.list, Vertices, 
            BlockList, color = blockEdgeColor, oriented = oriented)
    }
    if (slave) 
        linkDynamicGraph$redrawGraphWindow(graphWindow = NULL, 
            edgeList = edgeList, object = Object, factorVertexList = FactorVertices, 
            factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
            title = title, returnLink = returnLink, ...)
    else linkDynamicGraph$redrawGraphWindow(graphWindow = linkDynamicGraph$graphWindow, 
        edgeList = edgeList, object = Object, factorVertexList = FactorVertices, 
        factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
        title = title, returnLink = returnLink, Arguments = linkDynamicGraph)
}
