"returnExtraEdgeList" <-
function (edge.list, vertices, extravertices = NULL, width = 2, 
    color = "DarkSlateGrey", type = NULL) 
{
    "newExtraEdgeList" <- function(list) return(new("dg.ExtraEdgeList", 
        nodeList = list))
    vertex.names <- Names(vertices)
    if (is.null(edge.list)) 
        edge.list <- vector("list", length = 0)
    n <- length(edge.list)
    if (n == 0) 
        result <- .emptyDgList("dg.ExtraEdgeList")
    else {
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
                edge.vertices[[j]] <- extravertices[[-edge[j]]]
            class(edge.vertices) <- "dg.VertexList"
            result[[i]] <- newExtraEdge(edge, edge.vertices, 
                width = width, color = color, type = type)
        }
        class(result) <- "dg.ExtraEdgeList"
        names(result) <- Labels(result)
    }
    return(result)
}
