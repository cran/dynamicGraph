"newVertex" <-
function (name, label = name, type = "dg.Vertex", index = 1, 
    position = c(0, 0, 0), blockindex = 0, stratum = 0, color = ifelse(type == 
        "TextVertex", "GhostWhite", "SaddleBrown"), vertexClasses = validVertexClasses()) 
{
    if (!is.na(type) && type == "TextVertex") 
        prototype <- "dg.TextVertex"
    else {
        prototype <- "dg.Vertex"
        x <- match(type, vertexClasses[, 1])
        if (!is.null(x) && !all(is.na(x))) 
            prototype <- paste(vertexClasses[, 2][x])
    }
    if (prototype == "dg.TextVertex") 
        index <- -index
    result <- new(prototype, name = name, label = label, index = index, 
        position = position, label.position = rep(0, length(position)), 
        color = color, blockindex = blockindex, stratum = stratum)
    return(result)
}
