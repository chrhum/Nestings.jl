# Archimedean Copula with a focus on Gumbel 

"""
Abstract type for the families of archimedean copulas.

Concrete subtypes are callable and encode a family of generators. 
"""
abstract type Archimedean end

generator(a::Archimedean) = error("generator for $(typeof(a)) to be defined")
(a::Archimedean)(t) = generator(a)(t)

inverse(a::Archimedean) = error("inverse generator for $(typeof(a)) to be defined")
Base.isless(::A, ::A) where {A<:Archimedean} = error("isless for $A to be defined")

upper_taildep(a::Archimedean) = error("upper_taildep for $(typeof(a)) to be defined")
lower_taildep(a::Archimedean) = error("lower_taildep for $(typeof(a)) to be defined")

mvcdf(a::Archimedean, u::Float64...) = all(u .>= 0) & all(u .<= 1) ?
                                       a(sum(inverse(a)(v) for v in u)) :  # works for any number of marginals
                                       throw(DomainError(u, "all arguments must be between 0 and 1"))

struct Gumbel <: Archimedean
    theta::Float64
    Gumbel(theta) = theta < one(theta) ? throw(ArgumentError("Gumbel theta must be >= 1")) :
                    new(theta)
end


generator(g::Gumbel) = t -> exp(-t^(1 / g.theta))
inverse(g::Gumbel) = x -> (-log(x))^g.theta
Base.isless(g::Gumbel, h::Gumbel) = g.theta < h.theta # needs to be fulfilled for monotonicty in NestedCopula

upper_taildep(g::Gumbel) = 2 - 2^(1 / g.theta)
lower_taildep(g::Gumbel) = zero(g.theta)

Base. /(g::Gumbel, h::Gumbel) = Gumbel(g.theta / h.theta)

inverse_laplace_trafo(g::Gumbel) = g.theta > one(g.theta) ?
                                   AlphaStable(α=1.0 / g.theta, β=1.0, scale=(cos(π / (2.0 * g.theta)))^g.theta) :
                                   AlphaStable(α=1.0, β=1.0, scale=0.0, location = 1.0)

##
abstract type AbstractCopula{T} end

struct Copula{A<:Archimedean} <: AbstractCopula{A}
    a::A
    dim::UInt
end

generator(c::Copula{<:Archimedean}) = c.a
family(::Copula{A}) where {A<:Archimedean} = A

"""
    dimension(c) 

The number of marginals of the copula
"""
dimension(c::Copula{<:Archimedean}) = Int(c.dim)

@forward Copula.a upper_taildep, lower_taildep

Base. /(c::Copula{Gumbel},g::Gumbel) = Copula{Gumbel}(generator(c) / g, dimension(c))
##

#See  https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf


struct NestedCopula{A<:Archimedean} <: Nesting{Copula{A}}
    param::NestedValues{Copula{A}}
    function NestedCopula{A}(param) where A
        _check_monotonicity(param) || throw(ArgumentError("Monotonicity condition for generators not fulfilled."))
        return new{A}(param)
    end
end

function _check_monotonicity(g::NestedValues{Copula{A}}) where A <: Archimedean
    for h in nestings(g)
        generator(start(g)) <= generator(start(h)) || return false
        _check_monotonicity(h) || return false
    end
    return true
end


nestedcopula(c...) = NestedCopula{family(c[1])}(nest(c...))

@forward NestedCopula.param start

nestings(c::NestedCopula{A}) where {A<:Archimedean} = NestedCopula{A}.(nestings(c.param)) #forward does not work for this
map(f::Function, z::NestedCopula) = map(f, z.param)


function dimension(c::NestedCopula{<:Archimedean})
    d = dimension(start(c))
    for h in nestings(c)
        d += dimension(h)
    end
    return d
end

family(::NestedCopula{A}) where {A<:Archimedean} = A

Base. /(c::NestedCopula{Gumbel}, g::Gumbel) = NestedCopula{Gumbel}(map(z -> z / g, c))

function (c::NestedCopula)(u::Float64...)
    dimension(c) > 0 || 
    throw(ArgumentError("copula function in dimesion 0 is not defined"))
    length(u) == dimension(c) ||
        throw(DomainError(u, "$(dimension(c)) arguments needed, equal to number of copulas marginals."))
    all(0.0 .<= u) && all(u .<= 1.0) ||
        throw(DomainError(u, "all arguments must be between 0 and 1"))

    d = dimension(start(c))
    v = collect(u)
    q, r = v[1:d], v[d+1:end]

    for h in nestings(c)
        d = dimension(h)
        p, r = r[1:d], r[d+1:end]
        push!(q, h(p...))
    end
    mvcdf(generator(start(c)), q...)
end

#  Note that sampling from NestedGumbel is easier than for the other nested Archimedean. The algorithm
#  below the origial from McNeil where the special case that the Laplace-Stieltjes transform
#  used in the recursive step is of the same family as for sampling from the Gumbel copula 
#  (compare  Tables 3 and 1 
#  in Hofert https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf )
#  The algorithm below can be modified so the cases of Algorithm 3.2 in Hofert are covered. 
"""
    sample(g, nsmp)

Drawing `nsmp` independent samples from  `NestedGumbel` copula.
"""
function sample(c::NestedCopula{Gumbel}, nsmp)
    smp = rand(dimension(start(c)), nsmp)

    for h in nestings(c) # here Gumbel is easier than the other NestedArchimedeans 
        #h1 = NestedCopula{Gumbel}(map(z -> z / generator(start(c)), h))
        h1 = h / generator(start(c))
        smp = [smp; sample(h1, nsmp)]
    end
    generator(start(c)).theta > 1.0 + 1e-10 || return smp # cannot be distinguished from the independent case
    v = rand(inverse_laplace_trafo(generator(start(c))), 1, nsmp)

    return generator(start(c)).(-log.(smp) ./ v)
end

"""
    uppertaildep(g)

# Example

```jldoctest
julia> uppertaildep(nest(Gumbel(2, 1.1), (Gumbel(3, 1.4), Gumbel(2, 2.5))))
NestedValues{Float64}: (0.12213817867658738, (0.3593292879847241, 0.6804920892271058))
```
"""
#uppertaildep(gg::Nested{Gumbel}) = map(g -> 2 - 2^(1 / g.theta), gg)

