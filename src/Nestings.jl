
"""
A light framework for handling nested data to facilitate recursive algorithms.

The package provides an abstract type `Nesting` with an interface 
and concrete types `NestedData{T,C}` for nesting data of type `T` under a condition `C`.

`NestedValues{T}` is an alias for nesting data of type `T` 
with no further nesting condition, i.e. `C = Nestings.HasNoConstraint`.

# Construction 

An intuitive way to construct `NestedValues` is as follows:  

```jldoctest
julia> z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")
```
# Interface

Every concrete sub-type of `Nesting` needs an implementation of `start` and `nestings.`

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
An extension of map for `Nesting`: 

```jldoctest
julia> map(length,z)
NestedValues{Int64}: (4, (6, 11, 11), 6)
```
"""
module Nestings

using AlphaStableDistributions
using Lazy: @forward

import Base: eltype, show, ==, map, convert

include("nested.jl")
export Nesting, NestingCondition, NestedData, NestedValues, 
    start, nestings, 
    nest, next, depth, elements, transform, 
    allnested, allnextunique, uniquenext,
    indexdata, indexends
    
include("gumbel.jl")
export Archimedean, Gumbel, Copula, NestedCopula, nestedcopula, 
    generator, inverse, inverse_laplace_trafo, dimension,  upper_taildep, lower_taildep, family,
    sample

end #module
