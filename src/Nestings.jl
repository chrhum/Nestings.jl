module Nestings

using Graphs, AlphaStableDistributions
using Lazy: @forward

import Base: eltype, show, ==, map, rand

include("nested.jl")
export Nesting, NestedValues, nest, represent,
    start, nestings, next, 
    depth, indexdata, indexends,
    allnest, allnextunique


include("gumbel.jl")
export Archimedean, Gumbel, Copula, NestedCopula, nestedcopula, 
    generator, inverse, dimension,  upper_taildep, lower_taildep, family

end
