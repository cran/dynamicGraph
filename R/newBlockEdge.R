"newBlockEdge" <-
function (node.indices, nodes = NULL, width = 2, color = "DarkOliveGreen", 
    oriented = TRUE, type = NULL, label = ifelse(is.null(nodes), 
        "", paste(Labels(nodes), collapse = "~")), dash = "") 
{
    result <- new("dg.BlockEdge", vertex.indices = node.indices, 
        width = width, color = color, oriented = oriented, label = label, 
        dash = dash)
    return(result)
}
