module Nestings

using Graphs, AlphaStableDistributions

import Base: eltype, show, ==, map, rand

include("nested.jl")
export Nesting, NestedValues, nest, represent,
    start, nestings, next, 
    depth, indexdata, indexends,
    allnest, allnextunique


include("gumbel.jl")
export Gumbel, NestedGumbel, 
    generator, invgenerator, num_marginals, sample, uppertaildep

end
