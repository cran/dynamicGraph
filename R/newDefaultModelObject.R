"newDefaultModelObject" <-
function (name) 
{
    result <- new("dg.Model", name = name, extraVertices = .emptyDgList("dg.VertexList"), 
        vertexEdges = .emptyDgList("dg.VertexEdgeList"), blockEdges = .emptyDgList("dg.BlockEdgeList"), 
        factorVertices = .emptyDgList("dg.FactorVertexList"), 
        factorEdges = .emptyDgList("dg.FactorEdgeList"), extraEdges = .emptyDgList("dg.ExtraEdgeList"))
    return(result)
}
