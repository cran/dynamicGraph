"newVertex" <-
function (name, label = name, type = "VertexProto", index = 1, 
    position = c(0, 0, 0), blockindex = 0, stratum = 0, color = ifelse(type == 
        "TextVertex", "GhostWhite", "SaddleBrown"), vertexClasses = validVertexClasses()) 
{
    if (!is.na(type) && type == "TextVertex") 
        prototype <- "TextVertexProto"
    else {
        prototype <- "VertexProto"
        x <- match(type, vertexClasses[, 1])
        if (!is.null(x) && !all(is.na(x))) 
            prototype <- paste(vertexClasses[, 2][x])
    }
    result <- new(prototype, name = name, label = label, index = index, 
        position = position, color = color, blockindex = blockindex, 
        stratum = stratum)
    return(result)
}
