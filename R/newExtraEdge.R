"newExtraEdge" <-
function (node.indices, nodes = NULL, width = 2, color = "DarkOliveGreen", 
    type = NULL, label = ifelse(is.null(nodes), "", paste(Labels(nodes), 
        collapse = "~")), dash = "") 
{
    result <- new("dg.ExtraEdge", vertex.indices = node.indices, 
        width = width, color = color, label = label, dash = dash)
    return(result)
}
