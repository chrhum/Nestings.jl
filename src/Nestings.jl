
"""
A light framework for handling nested data to facilitate recursive algorithms.

The package provides an abstract type `Nesting` with an interface 
and concrete types `NestedData{T,C}` for nesting data of type `T` under a condition `C`.

`NestedValues{T}` is an alias for nesting data of type `T` 
with no further nesting condition, i.e. `C = Nestings.HasNoConstraint`.

# Construction 

An intuitive way to construct `NestedValues` is as follows:  

```julia
julia> z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")
```
# Interface

Every concrete sub-type of `Nesting` needs an implementation of `start` and `nestings.`

```julia; setup = :(z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2"))
julia> start(z)
"root"

julia> nestings(z)
2-element Vector{NestedValues{String}}:
NestedValues{String}: ("child1", "grandchild1", "grandchild2")
NestedValues{String}: child2
```

# Misc

For each `Nesting` a natural indexation of its data is provided:  

```julia; setup = :(z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2"))
julia> indexdata(z)
Dict{Vector{Int64}, String} with 5 entries:
  []     => "root"
  [1]    => "child1"
  [1, 1] => "grandchild1"
  [1, 2] => "grandchild2"
  [2]    => "child2"
```
An extension of map for `Nesting`: 

```julia
julia> map(length,z)
NestedValues{Int64}: (4, (6, 11, 11), 6)
```
"""
module Nestings



import Base: eltype, show, ==, map, convert

include("nested.jl")
export Nesting, NestingCondition, NestedData, NestedValues, 
    start, nestings, 
    nest, next, depth, elements, transform, 
    allnested, allnextunique, uniquenext,
    indexdata, indexends
    
include("Applications/ArchimedeanCopulas.jl")
#export ArchimedeanCopulas
end #module
