"validVertexClasses" <-
function () 
{
    result <- cbind(vertexTypes = c("Discrete", "Ordinal", "Continuous"), 
        vertexClasses = c("DiscreteVertexProto", "OrdinalVertexProto", 
            "ContinuousVertexProto"))
    dimnames(result) <- list(result[, 1], c("Label", "Class"))
    return(result)
}
