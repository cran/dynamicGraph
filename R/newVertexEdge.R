"newVertexEdge" <-
function (vertex.indices, vertices = NULL, width = 2, color = "DarkOliveGreen", 
    oriented = FALSE, type = NULL, label = ifelse(is.null(vertices), 
        "", paste(Labels(vertices), collapse = "~"))) 
{
    result <- new("VertexEdgeProto", vertex.indices = vertex.indices, 
        width = width, color = color, oriented = oriented, label = label)
    return(result)
}
