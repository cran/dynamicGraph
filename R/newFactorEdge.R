"newFactorEdge" <-
function (node.indices, nodes = NULL, width = 2, color = "DarkOliveGreen", 
    type = NULL, label = ifelse(is.null(nodes), "", paste(Labels(nodes), 
        collapse = "~"))) 
{
    result <- new("FactorEdgeProto", vertex.indices = node.indices, 
        width = width, color = color, label = label)
    return(result)
}
