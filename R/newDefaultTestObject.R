"newDefaultTestObject" <-
function (name) 
{
    df <- round(runif(1, 1, 25))
    message("Just generating a random test!!!!!")
    deviance <- rchisq(1, df)
    p <- 1 - pchisq(deviance, df)
    result <- new("defaultTestObjectProto", df = df, deviance = deviance, 
        p = p)
    return(result)
}
