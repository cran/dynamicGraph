"returnEdgeList" <-
function (edge.list, vertices, width = 2, color = "DarkSlateGrey", 
    N = 3, oriented = FALSE, types = NULL, edgeClasses = validEdgeClasses()) 
{
    "newVertexEdgeList" <- function(list) return(new("dg.VertexEdgeList", 
        nodeList = list, N = N))
    vertex.names <- Names(vertices)
    if (is.null(edge.list)) 
        edge.list <- vector("list", length = 0)
    n <- length(edge.list)
    if (n == 0) 
        result <- .emptyDgList("dg.VertexEdgeList")
    else {
        result <- vector("list", n)
        for (i in seq(along = edge.list)) {
            edge <- edge.list[[i]]
            if (!is.numeric(edge)) 
                edge <- match(edge, vertex.names)
            m <- length(edge)
            edge.vertices <- vector("list", m)
            for (j in seq(along = edge)) edge.vertices[[j]] <- vertices[[edge[j]]]
            if (is.null(types)) 
                type <- edgeClasses[, 1][1]
            else type <- types[i]
            class(edge.vertices) <- "dg.VertexList"
            result[[i]] <- newVertexEdge(edge, edge.vertices, 
                width = width, color = color, oriented = oriented, 
                N = N, type = type, edgeClasses = edgeClasses)
        }
        class(result) <- "dg.VertexEdgeList"
        names(result) <- Labels(result)
    }
    return(result)
}
