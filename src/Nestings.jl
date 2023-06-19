module Nestings

using Graphs

import Base: eltype, show, ==, map

include("nested.jl")
export Nesting, NestedValues, 
    start, nestings, next, 
    depth, indexdata, indexends,
    allnest, allnextunique

end
