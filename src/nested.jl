"""
Abstract type for nestings data of the same type recursively.

Any concrete subtype needs to implement methods 'start' and 'nestings'.
"""
abstract type Nesting{T} end

eltype(::Nesting{T}) where {T} = T

"""
NestedValues{T}(u,[v])    
NestedValues(u,[v])   
  

Allows for nesting values of the same type recursively.    

This is an implementation of a tree structure (see https://en.wikipedia.org/wiki/Tree_structure) with a value assigned to each node.

 # Examples

    NestedValues{String}("root", [NestValues{String}("child1",[NestedValues{String}("grandchild1"),NestedValues{String}("grandchild2")]), NestedValues{String}("child2")])

See also `nest` for construction by nested parentheses. 

"""
struct NestedValues{T} <: Nesting{T}
    val::T
    nst::Vector{NestedValues{T}}
end

## further constructors 
NestedValues{T}(val) where {T} = NestedValues{T}(val, NestedValues{T}[])
NestedValues(val) = NestedValues{typeof(val)}(val) #, NestedValues{typeof(val)}[])


"""
    nest(T,x)

    nest(x...)
Nest the elements of x using nested tuples. Provides an intuitive way to construct `NestedValues`.

 # Examples
    nest(Real,(1,(1//2,π), sqrt(2)))

    nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
"""
function nest(T::DataType, x::Tuple) 
    x[1] isa T || throw(DomainError(x, "First value of $x must be of type $T."))
    length(x) != 1 || return NestedValues{T}(x[1])
    # If z below is not a tuple, it may not be iterable so that nest(z...) would fail. Hence,
    nst = [z isa T ? NestedValues{T}(z) : nest(T,z) for z in x[2:end]]
    return NestedValues{T}(x[1], nst)
end

nest(x...) = nest(typeof(x[1]), x) # Example: nest("root", ("child1", "grandchild1", "grandchild2"), "child2")

## Interface to be implemented for each concrete subtype of Nesting

"""
    start(z)
    
The start of the series of nested elements of `z`. 

# Example
```jldoctest
julia> z = nest(3,(4,2),7)
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
julia> z = nest(3,(4,2),7)
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
    
The elements at the next level of nesting. See also `start` and `nestings`.

# Example
```jldoctest
julia> z = nest(3,(4,2),7)
NestedValues{Int64}: (3, (4, 2), 7)

julia> next(z)
2-element Vector{Int64}:
 4
 7
```
"""
next(z::Nesting) = map(start, nestings(z))

# show

"""
    represent(z) 

return the nested tuple which represents the `z`. It is the inverse function 
of `nest` for NestedValues.

# Examples
```jldoctest
julia> z = nest(1,2,(3,4))
NestedValues{Int64}: (1, 2, (3, 4))

julia> represent(z)
(1, 2, (3, 4))

julia> w = nest(represent(z)...)
NestedValues{Int64}: (1, 2, (3, 4))

julia> w == z
true
```
"""
represent(z::Nesting) = isempty(nestings(z)) ? start(z) : tuple(start(z), represent.(nestings(z))...)

show(io::IO, z::Nesting) = print(io, "$(typeof(z)): $(represent(z))")

"""
    depth(z)

The maximal number of recursive nestings. 
# Examples
```jldoctest
julia> depth(NestedValues("no nestings"))
0

julia> depth(nest("a",("b",("c", "d", "e"), "f"), "g"))
3
```
"""
depth(z::Nesting) = isempty(nestings(z)) ? zero(Int) : 1 + maximum(depth.(nestings(z)))

"""
    indexdata(z)

An indexing of the nested elements 
# Examples
```jldoctest
julia> indexdata(nest("no nestings"))
Dict{Vector{Int64}, String} with 1 entry:
  [] => "no nestings"

julia> indexdata(nest("a",("b",("c", "d", "e"), "f"), "g"))
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
    _add_next_indices!(indexdata, iv, nestings(z))
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

julia> indexends(nest("a",("b",("c", "d", "e"), "f"), "g"))
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
    depth(z) == 0 ? iv[[]] = start(z) : _add_next_indices!(indexends, iv, nestings(z))
    return iv
end

# auxiliary function used in indexdata and indexends
function _add_next_indices!(fun, iv, nst)
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
julia> map(x->x^2, nest(1,(2,3),4))
NestedValues{Int64}: (1, (4, 9), 16)
```
"""
map(f::Function, z::NestedValues) = isempty(nestings(z)) ? NestedValues(f(start(z))) :
                                    NestedValues(f(start(z)), [map(f, w) for w in nestings(z)])

# We may use this for NestedKeys
# """
#     isinitpartof(z, w)

# `true` if `z` is contained in `w` from the start of `w`.

# # Examples
# ```jldoctest
# julia> isinitpartof(nest(1,3), nest(1,2,(3,4)))
# true

# julia> isinitpartof(nest(1,4), nest(1,2,(3,4)))
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
julia> map(x->x^2, nest(1,(2,3),4))
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

Returns `true` if elements of `next(z)` are unique and likewise for all further nestings, recursively.
# Examples
 ```jldoctest
julia> allnextunique(nest(1,(1,2),2,3))
true

julia> allnextunique(nest(1,(1,2),3,3))
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

