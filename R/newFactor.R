"newFactor" <-
function (vertex.indices, vertices = NULL, name = ifelse(is.null(vertices), 
    "", paste(Labels(vertices), collapse = ":")), label = name, 
    type = "Generator", index = 0, width = 2, color = "default", 
    factorClasses = validFactorClasses()) 
{
    prototype <- "dg.Generator"
    x <- match(type, factorClasses[, 1])
    if (!is.null(x)) 
        prototype <- paste(factorClasses[, 2][x])
    positions <- Positions(vertices)
    if (color == "default") 
        color <- c("yellow", "cyan", "magenta", "blue")[x]
    result <- new(prototype, vertex.indices = vertex.indices, 
        position = apply(positions, 2, mean), index = index, 
        color = color, name = name, label = label)
    return(result)
}
