# Nestings.jl

`Nestings.jl` provides a lightweight framework for recursive data structures.
It is designed to facilitate the implementation of recursive algorithms
by providing a clear interface for nested data.

## Overview

`Nestings.jl` provides an abstract type `Nesting` with a clean interface and
concrete types for nesting data recursively. The main concrete type is `NestedData{T,C}`,
which allows nesting values of type `T` under a nesting condition `C`.

`NestedValues{T}` is a convenient alias for `NestedData{T,HasNoConstraint}` when no constraints are needed.

## Quick Start

```julia
using Nestings

z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
```

### Basic Construction

The simplest way to construct nested values is using the `nest` function:

```julia
julia> z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")
```

### Interface

Every concrete sub-type of `Nesting` needs an implementation of `start` and `nestings`:

```julia
julia> start(z)
"root"

julia> nestings(z)
2-element Vector{NestedValues{String}}:
    NestedValues{String}: ("child1", "grandchild1", "grandchild2")
    NestedValues{String}: child2
```

### Indexing Nested Data

Nestings provide natural indexing via dictionaries:

```julia
julia> indexdata(z)
Dict{Vector{Int64}, String} with 5 entries:
  []     => "root"
  [1]    => "child1"
  [1, 1] => "grandchild1"
  [1, 2] => "grandchild2"
  [2]    => "child2"
```

### Mapping Over Nestings

You can apply functions to all values in a nesting:

```julia
julia> map(length, z)
NestedValues{Int64}: (4, (6, 11, 11), 6)
```

## Nesting Conditions

You can enforce constraints on how data can be nested using different `NestingCondition` types:

- `HasNoConstraint` — no restrictions (used in `NestedValues`)
- `IsIncreasing` — enforces monotonic ordering
- `NextAreDifferent` — enforces uniqueness of siblings

Example with constraints:

```julia
julia> nest(Int, Nestings.IsIncreasing(), (1, (1, 3), (2, 2, 2)))
NestedData{Int64, Nestings.IsIncreasing}: (1, (1, 3), (2, 2, 2))
```

## Use Cases

This package is useful for:

- Structuring hierarchical or tree-like data
- Implementing recursive algorithms that operate on nested structures
- Managing multi-level constraints on data organization
- Representing nested decompositions in statistical or mathematical contexts

See the API Reference for complete documentation of all functions and types.
