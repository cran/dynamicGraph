"returnFactorEdgeList" <-
function (edge.list, vertices, factorvertices = NULL, width = 2, 
    color = "DarkSlateGrey", type = NULL) 
{
    "newFactorEdgeList" <- function(list) return(new("FactorEdgeListProto", 
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
        for (j in seq(along = edge)) if (edge[j] > 0) 
            edge.vertices[[j]] <- vertices[[edge[j]]]
        else if (edge[j] < 0) 
            edge.vertices[[j]] <- factorvertices[[-edge[j]]]
        result[[i]] <- newFactorEdge(edge, edge.vertices, width = width, 
            color = color, type = type)
    }
    names(result) <- Labels(result)
    return(result)
}
