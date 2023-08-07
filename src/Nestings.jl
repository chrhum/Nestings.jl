
"""
A very light framework for handling nested data to facilitate recursive algorithms.

The package provides an abstract type `Nesting` with a light interface 
and a concrete type `NestedValues`.  


# Construction 

An intuitive way to construct `NestedValues` is as follows:  

```jldoctest
julia> z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")
```
# Interface

Every concrete sub-type of `Nestings` needs an implementation of `start` and `nestings.`

```jldoctest
julia> start(z)
"root"

julia> nestings(z)
2-element Vector{NestedValues{String}}:
    NestedValues{String}: ("child1", "grandchild1", "grandchild2")
    NestedValues{String}: child2
```

# Misc

For each `Nesting` a natural indexation of its data is provided:  

```jldoctest
julia> indexdata(z)
Dict{Vector{Int64}, String} with 5 entries:
  []     => "root"
  [1]    => "child1"
  [1, 1] => "grandchild1"
  [1, 2] => "grandchild2"
  [2]    => "child2"
```
An extension of map is definded for `NestedValues`: 

```jldoctest
julia> map(length,z)
NestedValues{Int64}: (4, (6, 11, 11), 6)
```

"""
module Nestings

using Graphs, AlphaStableDistributions
using Lazy: @forward

import Base: eltype, show, ==, map, rand

include("nested.jl")
export Nesting, NestedValues, nest, isnestedtuple, represent,
    start, nestings, next, 
    depth, indexdata, indexends,
    allnest, allnextunique


include("gumbel.jl")
export Archimedean, Gumbel, Copula, NestedCopula, nestedcopula, 
    generator, inverse, inverse_laplace_trafo, dimension,  upper_taildep, lower_taildep, family,
    sample
end
