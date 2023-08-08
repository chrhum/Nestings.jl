"""
Abstract type for nestings data of the same type recursively.

# Interface 
Any concrete subtype needs the methods `start` and `nestings`.
"""
abstract type Nesting{T} end

eltype(::Nesting{T}) where {T} = T

abstract type NestingCondition end

struct HasNoConstraint <: NestingCondition end
struct IsIncreasing <: NestingCondition end
struct NextAreDifferent <: NestingCondition end


NestingCondition(::Type{<:Nesting}) = HasNoConstraint()

"""
    NestedValues{T}(u,[v])    
    NestedValues(u,[v])   
  

Allows for nesting values of the same type recursively.    

Implements a tree structure (see https://en.wikipedia.org/wiki/Tree_structure) 
with a value assigned to each node.

 # Examples

    NestedValues{Int}(0, [NestedValues{Int}(1), NestedValues{Int}(2)])
    NestedValues(0, [NestedValues(1,[NestedValues(11), NestedValues(12)]), NestedValues(2)])

See also `nest` for an intuitive and simpler construction by nested parentheses.

"""
struct NestedValues{T,C<:NestingCondition} <: Nesting{T}
    val::T
    nst::Vector{NestedValues{T}}
    function NestedValues{T,C}(val, nst) where {T,C}
        z = new{T,C}(val, nst)
        is_condition_valid(z) || throw(ArgumentError("NestingCondition $C not fulfilled"))
        return z
    end
end

NestingCondition(::Type{NestedValues{T,C}}) where {T,C<: NestingCondition} = C()
#NestingCondition(::Type{NestedValues{<:Any,C}}) where {C<:NestingCondition} = C()

nesting_condition(::S) where S<:Nesting = NestingCondition(S)

is_condition_valid(z::S) where {S<:Nesting} = is_condition_valid(NestingCondition(S), z)
is_condition_valid(::HasNoConstraint, z) = true
is_condition_valid(::NextAreDifferent, z) = allnextunique(z)

function is_condition_valid(::IsIncreasing, z)
    for w in nestings(z)
        start(z) <= start(w) || return false
        is_condition_valid(IsIncreasing(), w) || return false
    end
    return true
end

## further constructors 

NestedValues{T,C}(val::T) where {T,C<:NestingCondition} = NestedValues{T,C}(val, NestedValues{T,C}[])
NestedValues{T}(val, nst) where {T} = NestedValues{T,HasNoConstraint}(val, nst)
NestedValues{T}(val) where {T} = NestedValues{T}(val, NestedValues{T}[])
NestedValues(val, nst) = NestedValues{typeof(val)}(val, nst)
NestedValues(val) = NestedValues{typeof(val)}(val) #, NestedValues{typeof(val)}[])

"""
    nest(T,t::Tuple)

    nest(t...) 

Nest the elements of `t`. Provides an intuitive way to construct `NestedValues`.

 # Examples
 ```jldoctest
julia> nest(Real,(1,(1//2,π), sqrt(2)))
NestedValues{Real}: (1, (1//2, π), 1.4142135623730951)

julia> nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")
```
The first element of `t` is the `start` value. The subsequent elements define the `nestings`
recursively. 

A more stringent but equivalent notation for a "nested tuple" may be along the lines of `s` below: 

```jldoctest
julia> s= ("root", ("child1", ("grandchild1",), ("grandchild2",)), ("child2",))
("root", ("child1", ("grandchild1",), ("grandchild2",)), ("child2",))

julia> nest(s...) == nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
true
```
Both notations can be used.

# Remark

The order of the elements of `t` is a Depth-First-Search order for the nested values, 
see https://en.wikipedia.org/wiki/Depth-first_search.
"""
function nest(T::DataType, nc::NestingCondition, t::Tuple)
    t[1] isa T || throw(DomainError(t, "First value of $t must be of type $T."))
    C = typeof(nc)
    length(t) != 1 || return NestedValues{T,C}(t[1])
    nst = [z isa T ? NestedValues{T,C}(z) :
           z isa NestedValues{T,C} ? z : nest(T, nc, z) for z in t[2:end]]
    return NestedValues{T,C}(t[1], nst)
end

nest(nc::NestingCondition, t::Tuple) = nest(typeof(t[1]), nc, t)
nest(T::DataType, t::Tuple) = nest(T, HasNoConstraint(), t)
nest(x...) = nest(typeof(x[1]), x)

# function nest(T::DataType, t::Tuple)
#     t[1] isa T || throw(DomainError(t, "First value of $t must be of type $T."))
#     length(t) != 1 || return NestedValues{T}(t[1])
#     nst = [z isa T ? NestedValues{T}(z) : nest(T, z) for z in t[2:end]]
#     return NestedValues{T}(t[1], nst)
# end

# nest(x...) = nest(typeof(x[1]), x)

"""
isnestedtuple(T, t)

Check if tuple `t` is a valid argument for `nest`.
"""
function isnestedtuple(T::DataType, t::Tuple)
    t[1] isa T || return false
    for s in t[2:end]
        if s isa T
            nothing
        elseif s isa Tuple
            isnestedtuple(T, s) || return false
        else
            return false
        end
    end
    return true
end

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

==(nv::Nesting, nw::Nesting) = typeof(nv) == typeof(nw) &&
                               start(nv) == start(nw) &&
                               nestings(nv) == nestings(nw)

function Base.hash(nv::Nesting, h::UInt)
    hh = hash(start(nv), h)
    for n in nestings(nv)
        hh = hash(n, hh)
    end
    return hash(typeof(nv), hh)
end

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

Return a nested tuple which represents `z`. It is an inverse 
of `nest` for NestedValues.

# Examples
```jldoctest
julia> z = nest(1,2,(3,4))
NestedValues{Int64}: (1, 2, (3, 4))

julia> represent(z)
(1, 2, (3, 4))

julia> w = nest(represent(z)...)
NestedValues{Int64}: (1, 2, (3, 4))
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

An indexing of the nested elements. 

The indexation for `nest(x...)` is from left to right. 

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

Apply type-stable function `f` to each nested value. Returns 'NestValues{eltype(f(z)),HasNoConstraint}'
# Examples
```jldoctest
julia> map(x->x^2, nest(1,(2,3),4))
NestedValues{Int64}: (1, (4, 9), 16)
```
"""
map(f::Function, z::NestedValues) = isempty(nestings(z)) ? NestedValues(f(start(z))) :
                                    NestedValues(f(start(z)), [map(f, w) for w in nestings(z)])

"""
    transform(f, z)

Apply `f` which preserves both `eltype` and `NestingCondition` of `z`.
"""
function transform(f::Function, z::NestedValues)
    T = eltype(z)
    C = typeof(nesting_condition(z))
    fz = isempty(nestings(z)) ? NestedValues{T,C}(f(start(z))) :
         NestedValues{T,C}(f(start(z)), [transform(f, w) for w in nestings(z)])
    return fz
end

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

"""
    isinitpartof(z, w)

`true` if `z` is contained in `w` from the start of `w`.

# Examples
```jldoctest
julia> isinitpartof(nest(1,3), nest(1,2,(3,4)))
true

julia> isinitpartof(nest(1,4), nest(1,2,(3,4)))
false
```
"""
function isinitpartof(z::Nesting, w::Nesting) #types of z and w may be differnt by intention
    start(z) == start(w) || return false
    for x in nestings(z)
        isempty(nestings(w)) && return false
        k = findfirst(start(x) .== next(w))
        isnothing(k) && return false
        isinitpartof(x, nestings(w)[k]) || return false
    end
    return true
end

equiv(z::S, w::S) where {S<:Nesting} = isinitpartof(z, w) && isinitpartof(w, z)


"""
    g, gvals = getdigraph(z)

Tree representation of a `Nesting` as a `SimpelDiGraph` in `Graphs.jl`. 
Nested data are associated with the nodes. 
"""
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

