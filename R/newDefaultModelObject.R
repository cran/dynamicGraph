"newDefaultModelObject" <-
function (name) 
{
    result <- new("defaultModelObjectProto", name = name)
    return(result)
}
