"""
Abstract type for nestings data of the same type recursively.

# Interface 
Requires methods `start` and `nestings`.
"""
abstract type Nesting{T} end

eltype(::Nesting{T}) where {T} = T

"""
Abstract type for definition of nesting conditions.

Each concrete subtype `C` needs an implementation of `is_condition_valid(::C, z)`.

See also: `HasNoConstraint`, `IsIncreasing`, `NextAreDifferent`.
"""
abstract type NestingCondition end

"""
    Applies no further conditions of nesting. 

See also: `NestedValues`.
"""
struct HasNoConstraint <: NestingCondition end

"""
    Enforces recursively that all elements of a Nesting are `>=` than the predecessor. 
"""
struct IsIncreasing <: NestingCondition end

"""
    Enforces recursively that the next elements of a `Nesting` are pairwiese different. 
"""
struct NextAreDifferent <: NestingCondition end

#NestingCondition(::Type{<:Nesting}) = HasNoConstraint()

"""
    NestedData{T,C}(u,[v])   

Allows for nesting values of the same type `T` recursively under a nesting condition `C`.

Implements a tree structure (see https://en.wikipedia.org/wiki/Tree_structure) 
with a value assigned to each node.

 # Example

    NestedData{Int, Nestings.IsIncreasing}(0, [NestedData{Int, Nestings.IsIncreasing}(1), NestedData{Int, Nestings.IsIncreasing}(1)])

Use `nest` for an intuitive and simple construction by nested parentheses.

See also: `NestingCondition`, `NestedValues`.
"""
struct NestedData{T,C<:NestingCondition} <: Nesting{T}
    val::T
    nst::Vector{NestedData{T,C}}
    function NestedData{T,C}(val, nst) where {T,C}
        z = new{T,C}(val, nst)
        is_condition_valid(z) || throw(ArgumentError("NestingCondition $C not fulfilled"))
        return z
    end
end

NestedData{T,C}(val::T) where {T,C<:NestingCondition} = NestedData{T,C}(val, NestedData{T,C}[])

NestingCondition(::Type{NestedData{T,C}}) where {T,C<:NestingCondition} = C()

# define the nesting conditions

is_condition_valid(z::S) where {S<:Nesting} = is_condition_valid(NestingCondition(S), z)

is_condition_valid(c::NestingCondition, z) = error("Validation of $c needs to be defined.")
is_condition_valid(::HasNoConstraint, z) = true
is_condition_valid(::NextAreDifferent, z) = allnextunique(z)

function is_condition_valid(::IsIncreasing, z)
    for w in nestings(z)
        start(z) <= start(w) || return false
        is_condition_valid(IsIncreasing(), w) || return false
    end
    return true
end

# NestedValues

"""
    NestedValues{T}(u,[v])    
    NestedValues(u,[v])   
  
Alias for `NestedData{T,HasNoConstraint}`. 

 # Examples

    NestedValues{Int}(0, [NestedValues{Int}(1), NestedValues{Int}(2)])
    NestedValues(0, [NestedValues(1,[NestedValues(11), NestedValues(12)]), NestedValues(2)])

See also `nest` for an intuitive and simpler construction by nested parentheses.
"""
const NestedValues{T} = NestedData{T,HasNoConstraint}

NestedValues(val, nst) = NestedValues{typeof(val)}(val, nst)
NestedValues(val) = NestedValues{typeof(val)}(val) #, NestedValues{typeof(val)}[])

"""
    nest(t...) 
    nest(T, t::Tuple)
    nest(nc::NestingCondition, t::Tuple)
    nest(T, nc, t::Tuple)

Nest the elements of `t`. Provides an intuitive way to construct `NestedValues` and `NestedData`.

 # Examples

 ```jldoctest
 julia> nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
NestedValues{String}: ("root", ("child1", "grandchild1", "grandchild2"), "child2")

julia> nest(Real,(1, (1//2, π), sqrt(2)))
NestedValues{Real}: (1, (1//2, π), 1.4142135623730951)

julia> nest(Int,Nestings.IsIncreasing(),(1, (1, 3), (2, 2, 2)))
NestedData{Int64, Nestings.IsIncreasing}: (1, (1, 3), (2, 2, 2))
```
The first element of `t` is the `start` value. The subsequent elements define the `nestings`
recursively. 

Elements of `t` can also be of type `NestedData` as long as they have no next values. 

```jldoctest
z = nest(1, 2)
nest(3, z, 4)
NestedValues{Int64}: (3, (1, 2), 4)

julia> nest(3, (4, z), 5)
NestedValues{Int64}: (3, (4, (1, 2)), 5)
```

A more stringent but equivalent notation for a "nested tuple" along the lines of `s` below is
supported as well:

```jldoctest
julia> s= ("root", ("child1", ("grandchild1",), ("grandchild2",)), ("child2",))
("root", ("child1", ("grandchild1",), ("grandchild2",)), ("child2",))

julia> nest(s...) == nest("root", ("child1", "grandchild1", "grandchild2"), "child2")
true
```
# Remark

The order of the elements of `t` is a Depth-First-Search order of the nested values, 
see https://en.wikipedia.org/wiki/Depth-first_search.
"""
nest(x...) = nest(typeof(x[1]), x)
nest(T::DataType, t::Tuple) = nest(T, HasNoConstraint(), t)
nest(nc::NestingCondition, t::Tuple) = nest(typeof(t[1]), nc, t)

function nest(T::DataType, nc::NestingCondition, t::Tuple)
    t[1] isa T || throw(DomainError(t, "First value of $t must be of type $T."))
    C = typeof(nc)
    length(t) != 1 || return NestedData{T,C}(t[1])
    nst = [z isa T ? NestedData{T,C}(z) :
           z isa NestedData{T,C} ? z : nest(T, nc, z) for z in t[2:end]]
    return NestedData{T,C}(t[1], nst)
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
start(z::NestedData) = z.val

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
nestings(z::NestedData) = z.nst

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

"""
    elements(z::Nesting)


    elements(z::Nesting)
Retrieve the elements of `z`. 

The order of `elements(nest(t...))` is the order of the elements of `t` from left to right. 

# Example
```jldoctest
julia> z = nest(3, (4, 2), 7)
NestedValues{Int64}: (3, (4, 2), 7)

julia> elements(z)
4-element Vector{Int64}:
 3
 4
 2
 7
```
"""
function elements(z::Nesting)
    el = eltype(z)[start(z)] #vcat(start(z))
    for y in nestings(z)
        append!(el, elements(y))
    end
    return el
end

"""
    represent(z) 

Return a nested tuple which represents `z`. It is an inverse 
of `nest` for `NestedValues{T}` where `T` is concrete. 

# Examples
```jldoctest
julia> z = nest(1, 2, (3, 4))
NestedValues{Int64}: (1, 2, (3, 4))

julia> Nestings.represent(z)
(1, 2, (3, 4))

julia> w = nest(Nestings.represent(z)...)
NestedValues{Int64}: (1, 2, (3, 4))
```
"""
represent(z::Nesting) = isempty(nestings(z)) ? start(z) : tuple(start(z), represent.(nestings(z))...)

show(io::IO, ::MIME"text/plain", z::Nesting) = print(io, typeof(z), ": ", represent(z))

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
    map(f,z)

Apply type-stable function `f` to each nested value. Type of return is 'NestValues{eltype(f(x))}'.

# Examples
```jldoctest
julia> map(x->x^2, nest(1, (2, 3), 4))
NestedValues{Int64}: (1, (4, 9), 16)
```
"""
map(f::Function, z::Nesting{T}) where {T} = map(f, z, NestedValues{typeof(f(start(z)))})

# # The below map2 is faster than map above, but why? The difference is irrelevant, though
# map2(f::Function, z::Nesting) = isempty(nestings(z)) ? NestedValues(f(start(z))) :
#                                NestedValues(f(start(z)), [map(f, w) for w in nestings(z)])

function map(f::Function, z::Nesting, ::Type{NestedData{T,C}}) where {T,C}
    fz = isempty(nestings(z)) ? NestedData{T,C}(f(start(z))) :
         NestedData{T,C}(f(start(z)), [map(f, w, NestedData{T,C}) for w in nestings(z)])
    return fz
end

convert(::Type{NestedData{T,C}}, z::Nesting) where {T,C} = map(identity, z, NestedData{T,C})

"""
    transform(f, z::NestedData)

Apply `f` provided it preserves both `eltype` and `NestingCondition` of `z`. 
Return is of the same type as `z`.
"""
transform(f::Function, z::NestedData{T,C}) where {T,C} = map(f, z, NestedData{T,C})


## other methods

"""
    allnested(b) -> Bool

Returns `true` if all elements of a boolean nesting are true.  
# Examples
```jldoctest
julia> allnested(nest(true,(true, false),true))
false
```
"""
function allnested(b::Nesting{Bool})
    start(b) || return false
    for w in nestings(b)
        allnested(w) || return false
    end
    return true
end

"""
    allnextunique(z) -> Bool

Returns `true` if elements of `next(z)` are unique and likewise for all further nestings, recursively.
# Examples
 ```jldoctest
julia> allnextunique(nest(1, (1, 2), 2, 3))
true

julia> allnextunique(nest(1, (1, 2), 3, 3))
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
    uniquenext(z::Nesting) -> NestedValues{Bool}

Indicates whether next values are unique.
"""
uniquenext(z::Nesting) = isempty(nestings(z)) ? NestedValues{Bool}(allunique(next(z))) :
                         NestedValues{Bool}(allunique(next(z)), uniquenext.(nestings(z)))

"""
    isinitpartof(z, w)

Return `true` if `z` is contained in `w` from the start of `w`.

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

"""
isnestedtuple(T, t)

If `true` then `t` is a valid argument for `nest(t...)` and `nest(T,t))` with `T=typeof(t[1])`.
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