"validFactorClasses" <-
function () 
{
    result <- cbind(factorTypes = c("Generator", "Discrete generator", 
        "Linear generator", "Quadratic generator"), factorClasses = c("GeneratorProto", 
        "DiscreteGeneratorProto", "LinearGeneratorProto", "QuadraticGeneratorProto"))
    dimnames(result) <- list(result[, 1], c("Label", "Class"))
    return(result)
}
