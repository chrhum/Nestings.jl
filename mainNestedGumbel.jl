using Revise
using Nestings
using Graphs
#using AlphaStableDistributions 

## Nestings

# This is about the syntax for recursively nesting data
# Use help, in particular Nestings, NestedValues, nest

# Examples

ambigfun(a,b) = "a,b"
ambigfun(x...) = "x..."

ambigfun(a::Int,b::String) =  "a::Int, b::String"

function nest2(T::DataType, t::Tuple)
    t[1] isa T || throw(DomainError(t, "First value of $t must be of type $T."))
    length(t) != 1 || return NestedValues{T}(t[1])
    nst = [z isa T ? NestedValues{T}(z) : 
        z isa NestedValues{T} ? z : nest2(T, z) for z in t[2:end]]
    return NestedValues{T}(t[1], nst)
end

nest2(x...) = nest2(typeof(x[1]), x)

abstract type Condition end
struct IsDecreasing <:Condition end
struct NoConstraint <:Condition end

# struct Foo{T,V<:Condition}
#     x::T
#     y::T
#     v::V
#     function Foo{T,V}(x::T,y::T) where {T,V}
#         z = new{T,V}(x,y, V())
#         vali(z) || error("not validated")
#         return z
#     end    
# end

struct Boo{T,V<:Condition}
    x::T
    y::T
    function Boo{T,V}(x::T,y::T) where {T,V}
        z = new{T,V}(x,y)
        vali(z) || error("not validated")
        return z
    end    
end

Boo{T}(x,y) where T = Boo{T,NoConstraint}(x,y)

Boo{Int}(2,5)

vali(z::Boo{<:Real,IsDecreasing}) = z.y < z.x
vali(z::Boo{<:Any,NoConstraint}) = true
f = Boo{Int,IsDecreasing}(3,2)
g = Boo{Int,IsDecreasing}(2,3)
fun(b::Boo) = "generic"
fun(b::Boo{T,IsDecreasing }) where T  = "specific"
fun(b::Boo{Int,IsDecreasing}) = "very specific"

fun(Boo{String,NoConstraint}("a","b"))
fun(Boo{Float64,IsDecreasing}(2.1,2.0))
fun(Boo{Int,IsDecreasing}(2,1))
fun2(b::Boo{<:Any,NoConstraint}) = b.x
##

gen0 = Gumbel(1.1)
gen1 = Gumbel(1.4)
gen2 = Gumbel(2.0)
gen11 = Gumbel(2.5)

gen2 / gen1

g0 = Copula{Gumbel}(gen0, 0)
g1 = Copula{Gumbel}(gen1, 1)
g2 = Copula{Gumbel}(gen2, 2)
g11 = Copula{Gumbel}(gen11, 3)
family(g11)



#g11(0.3, 0.7, 0.9)

g0.a
inverse(g0.a)
h = nestedcopula(g1, g11)
h([0.3, 0.5, 0.7, 0.9])
dimension(h)
c = nestedcopula(g0, (g1, g11), g2)
dimension(c)
@time c([0.4, 0.5, 0.7, 0.8, 0.9, 0.7])
map(upper_taildep, c)

g, vals = Nestings.getdigraph(c)

h = c
nitr = 10000
U = Nestings.sample(h, nitr)

u = 1.0 .- rand(dimension(h)) .^ 6
h(u)

TF = U .< u
sum(all!(ones(Bool, 1, nitr), TF)) / nitr




##
function qdep(smp, q, lower=true)
    if !lower
        return qdep(1.0 .- smp, 1 - q)
    end
    Q = similar(smp, size(smp, 1), size(smp, 1))
    tf = smp .<= q
    for i = 1:size(smp, 1)
        for j = i+1:size(smp, 1)
            Q[i, j] = sum(tf[i, :] .& tf[j, :]) / size(smp, 2) / q
        end
    end
    return Q
end


function cplot(C::CoverNetwork)
    g, v = getdigraph(C)
    edgec = [ecol(C.links[v[src(e)], v[dst(e)]]) for e in edges(g)]
    gplot(g, nodefillc=colorant"beige", nodelabel=v, edgestrokec=edgec)
end

function nestedsample(c::NestedCopula{Gumbel},smp)
    marginals = map(z -> dimension(z),c)
    smp = sample(c,nsmp)
    depth(c) > 0 || return NestedValues(smp)
    
end

nsmp = 100
marginals = map(z -> dimension(z),c)
smp = sample(c,nsmp)


function nestedsample(c::NestedCopula{Gumbel}, nsmp::Int)
    smp = rand(dimension(start(c)), nsmp)
    
    nst = NestedValues{Matrix{Float64}}[]

    for h in nestings(c) # here Gumbel is easier than the other NestedArchimedeans 
        h1 = h / generator(start(c))
        push!(nst, nestedsample(h1, nsmp))
    end
    z = NestedValues(smp,nst)
    generator(start(c)).theta > 1.0 + 1e-10 || return z # cannot be distinguished from the independent case
    v = rand(inverse_laplace_trafo(generator(start(c))), 1, nsmp)

    return map(s -> generator(start(c)).(-log.(s) ./ v),z)
end


struct foo
    x
end

#Base.show(io::IO, z::foo) = print(io, "$(typeof(z)): $(z.x)")
Base.show(io::IO, ::MIME"text/plain", z::foo) = print(io, "$(typeof(z)):", z.x)
