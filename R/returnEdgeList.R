"returnEdgeList" <-
function (edge.list, vertices, width = 2, color = "DarkSlateGrey", 
    oriented = FALSE, type = NULL) 
{
    "newVertexEdgeList" <- function(list) return(new("VertexEdgeListProto", 
        nodeList = list))
    vertex.names <- Names(vertices)
    if (is.null(edge.list)) 
        edge.list <- vector("list", length = 0)
    n <- length(edge.list)
    result <- vector("list", n)
    for (i in seq(along = edge.list)) {
        edge <- edge.list[[i]]
        if (!is.numeric(edge)) 
            edge <- match(edge, vertex.names)
        m <- length(edge)
        edge.vertices <- vector("list", m)
        for (j in seq(along = edge)) edge.vertices[[j]] <- vertices[[edge[j]]]
        result[[i]] <- newVertexEdge(edge, edge.vertices, width = width, 
            color = color, oriented = oriented, type = type)
    }
    names(result) <- Labels(result)
    return(result)
}
