"newVertexEdge" <-
function (vertex.indices, vertices = NULL, width = 2, color = "DarkOliveGreen", 
    oriented = FALSE, type = "VertexEdge", label = ifelse(is.null(vertices), 
        "", paste(Labels(vertices), collapse = "~")), dash = "", 
    N = 3, edgeClasses = validEdgeClasses()) 
{
    if (!is.na(type) && type == "VertexEdge") 
        prototype <- "dg.VertexEdge"
    else {
        prototype <- "dg.VertexEdge"
        x <- match(type, edgeClasses[, 1])
        if (is.null(x) || all(is.na(x))) 
            x <- match(type, edgeClasses[, 2])
        if (!is.null(x) && !all(is.na(x))) 
            prototype <- paste(edgeClasses[, 2][x])
    }
    result <- new(prototype, vertex.indices = vertex.indices, 
        width = width, color = color, oriented = oriented, label = label, 
        label.position = rep(0, N), dash = dash)
    return(result)
}
