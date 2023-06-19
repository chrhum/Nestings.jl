"""
Abstract type for nestings data of the same type recursively.

Any concrete subtype needs to implement methods 'start' and 'nestings'.
"""
abstract type Nesting{T} end

eltype(::Nesting{T}) where {T} = T

"""
NestedValues{T}(u,[v])    
NestedValues(u,[v])   
NestedValues(u...)    

Allows for nesting values of the same type recursively.    

This is an implementation of a tree structure (see https://en.wikipedia.org/wiki/Tree_structure) with a value assigned to each node.


 # Examples

    NestedValues{String}("root", [NestValues{String}("child1",[NestedValues{String}("grandchild1"),NestedValues{String}("grandchild2")]), NestedValues{String}("child2")])

Construction by nested parentheses

    NestedValues("root", ("child1", "grandchild1", "grandchild2"), "child2")

"""
struct NestedValues{T} <: Nesting{T}
    val::T
    nst::Vector{NestedValues{T}}
end

## implicit constructors 
NestedValues{T}(val) where {T} = NestedValues{T}(val, NestedValues{T}[])
NestedValues(val) = NestedValues{typeof(val)}(val, NestedValues{typeof(val)}[])

function NestedValues(x...) # Example: NestedValues("root", ("child1", "grandchild1", "grandchild2"), "child2")
    if length(x) == 1
        return NestedValues(x[1])
    end
    # If z below is not a tuple, it may not be iterable so that NestedValues(z...) would fail. Hence,
    nst = [isa(z, typeof(x[1])) ? NestedValues(z) : NestedValues(z...) for z in x[2:end]]
    return NestedValues{typeof(x[1])}(x[1], nst)
end

## Interface to be implemented for each concrete subtype of Nesting

"""
    start(z)
    
The start of the series of nested elements of `z`. 

# Example
```jldoctest
julia> z = NestedValues(3,(4,2),7)
NestedValues{Int64}: (3, (4, 2), 7)

julia> start(z)
3
```
"""
start(z::NestedValues) = z.val

"""
    nestings(z)
    
returns what is nested.

# Example
```jldoctest
julia> z = NestedValues(3,(4,2),7)
NestedValues{Int64}: (3, (4, 2), 7)

nestings(z)
2-element Vector{NestedValues{Int64}}:
 NestedValues{Int64}: (4, 2)
 NestedValues{Int64}: 7
```
"""
nestings(z::NestedValues) = z.nst

## Methods for Nesting

==(nv::Nesting, nw::Nesting) = start(nv) == start(nw) && nestings(nv) == nestings(nw)

"""
    next(z)
    
The elements at the next level of nesting. See also `start` and `next`.

# Example
```jldoctest
julia> z = NestedValues(3,(4,2),7)
NestedValues{Int64}: (3, (4, 2), 7)

julia> next(z)
2-element Vector{Int64}:
 4
 7
```
"""
next(z::Nesting) = map(start, nestings(z))

# show
represent(z::Nesting) = isempty(nestings(z)) ? start(z) : tuple(start(z), represent.(nestings(z))...)
show(io::IO, z::Nesting) = print(io, "$(typeof(z)): $(represent(z))")

"""
    depth(z)

The maximal number of recursive nestings. 
# Examples
```jldoctest
julia> depth(NestedValues("no nestings"))
0

julia> depth(NestedValues("a",("b",("c", "d", "e"), "f"), "g"))
3
```
"""
depth(z::Nesting) = isempty(nestings(z)) ? zero(Int) : 1 + maximum(depth.(nestings(z)))

"""
    indexdata(z)

An indexing of the nested elements 
# Examples
```jldoctest
julia> indexdata(NestedValues("no nestings"))
Dict{Vector{Int64}, String} with 1 entry:
  [] => "no nestings"

julia> indexdata(NestedValues("a",("b",("c", "d", "e"), "f"), "g"))
Dict{Vector{Int64}, String} with 7 entries:
  [1, 1, 2] => "e"
  []        => "a"
  [1]       => "b"
  [1, 1]    => "c"
  [1, 2]    => "f"
  [1, 1, 1] => "d"
  [2]       => "g"
```
See also `indexends`.
"""
function indexdata(z::Nesting{T}) where {T} # Is this a good name? Alternativ: index_data
    iv = Dict{Vector{Int},T}([] => start(z))
    _add_nxt_indi!(indexdata, iv, nestings(z))
    return iv
end

"""
    indexends(z)

An indexing of the ends of the nestings
# Examples
```jldoctest
julia> indexends(NestedValues("no nestings"))
Dict{Vector{Int64}, String} with 1 entry:
  [] => "no nestings"

julia> indexends(NestedValues("a",("b",("c", "d", "e"), "f"), "g"))
Dict{Vector{Int64}, String} with 4 entries:
  [1, 1, 2] => "e"
  [1, 2]    => "f"
  [1, 1, 1] => "d"
  [2]       => "g"
```
See also `indexends`.
"""
function indexends(z::Nesting{T}) where {T}
    iv = Dict{Vector{Int},T}()
    depth(z) == 0 ? iv[[]] = start(z) : _add_nxt_indi!(indexends, iv, nestings(z))
    return iv
end

# auxiliary function used in indexdata and indexends
function _add_nxt_indi!(fun, iv, nst)
    for (k, w) in enumerate(nst)
        iw = fun(w)
        for i in keys(iw)
            pushfirst!(i, k)
        end
        merge!(iv, iw)
    end
end

## methods for concrete types

"""
    map(f,z)

Applies type-stable function f to each nested value. 
# Examples
```jldoctest
julia> map(x->x^2, NestedValues(1,(2,3),4))
NestedValues{Int64}: (1, (4, 9), 16)
```
"""
map(f::Function, z::NestedValues) = isempty(nestings(z)) ? NestedValues(f(start(z))) :
                                    NestedValues(f(start(z)), [map(f, w) for w in nestings(z)])

# We may use this for NestedKeys or NestedSets
# """
#     isinitpartof(z, w)

# `true` if `z` is contained in `w` from the start of `w`.

# # Examples
# ```jldoctest
# julia> isinitpartof(NestedValues(1,3), NestedValues(1,2,(3,4)))
# true

# julia> isinitpartof(NestedValues(1,4), NestedValues(1,2,(3,4)))
# false
# ```
# """
# function isinitpartof(z::Nesting, w::Nesting) #types of z and w may be differnt by intention
#     start(z) == start(w) || return false
#     for x in nestings(z)
#         isempty(nestings(w)) && return false
#         k = findfirst(start(x) .== next(w))
#         isnothing(k) && return false
#         isinitpartof(x, nestings(w)[k]) || return false
#     end
#     return true
# end

# equiv(z::S, w::S) where S <: Nesting = isinitpartof(z, w) && isinitpartof(w, z)


## other methods


"""
    allnest(b) -> Bool

Returns `true` if all elements of a boolean nesting are true.  
# Examples
```jldoctest
julia> map(x->x^2, NestedValues(1,(2,3),4))
NestedValues{Int64}: (1, (4, 9), 16)
```
"""
function allnest(b::Nesting{Bool})
    start(b) || return false
    for w in nestings(b)
        allnest(w) || return false
    end
    return true
end

"""
    allnextunique(z) -> Bool

Returns `true` if, elements of `next(z)` is unique and likewise for all further nestings, recursively.
# Examples
 ```jldoctest
julia> allnextunique(NestedValues(1,(1,2),2,3))
true

julia> allnextunique(NestedValues(1,(1,2),3,3))
false
```
"""
function allnextunique(z::Nesting)
    allunique(next(z)) || return false
    for w in nestings(z)
        allnextunique(w) || return false
    end
    return true
end

function getdigraph(z::Nesting)
    g = SimpleDiGraph(1)
    gvals = [start(z)]
    for x in nestings(z)
        h, hvals = getdigraph(x) # recursively, get the digraph for the nesting x
        n = nv(g)              # we need n after we modify g below
        add_vertices!(g, nv(h)) # add vertices of h to g
        add_edge!(g, 1, n + 1)    # connect the roots
        append!(gvals, hvals)   # add the values of h
        for e in edges(h)      # add the edges of h          
            add_edge!(g, n + e.src, n + e.dst)
        end
    end
    return g, gvals
end

