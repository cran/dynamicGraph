"newBlock" <-
function (stratum = 0, index = 0, position = matrix(rep(0, 6), 
    ncol = 3), visible = TRUE, color = "Grey", label = "Label", 
    label.position = c(0, 0, 0), ancestors = NULL) 
{
    result <- new("BlockProto", stratum = stratum, index = index, 
        position = position, visible = visible, color = color, 
        label = label, label.position = label.position, ancestors = if (is.null(ancestors)) 
            0
        else ancestors)
    return(result)
}
