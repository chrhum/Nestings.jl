# Gumbel Copula 
using AlphaStableDistributions



abstract type Archimedean end

struct Gumbel <: Archimedean
    theta::Float64
    nnc::Int
    Gumbel(theta,nnc) = theta >= 1.0 && nnc >= 0 ? new(theta, nnc) : throw(ArgumentError("Gumbel theta must be >= 1.0 and number "))
end

generator(a::Gumbel) = t -> exp(-t^(1 / a.theta))
invgenerator(a::Gumbel) = x -> (-log(x))^a.theta



#See  https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf


struct Nested{A <: Archimedean,N} <: Nesting{A}
    a::NestedValues{A} # 
    function Nested(a)
        _validate_monotonicity(a) || throw(ArgumentError("Monotonicity condition for generators not fulfilled."))
        return new{eltype(a),num_marginals(a)}(a)
    end
end


function _validate_monotonicity(g::NestedValues{Gumbel})
    for h in nestings(g)
        start(g).theta .<= start(h).theta || return false
        _validate_monotonicity(h) || return false
    end
    return true
end

_validate_monotonicity(a::NestedValues{Archimedean}) = error("Method for Type $(type(a)) not implemented")

"""
    num_marginals(g) 

The number of marginals of a the copula
"""
function num_marginals(g::NestedValues{Gumbel})
    n = start(g).nnc
    for h in nestings(g) n += num_marginals(h) end
    return n
end

num_marginals(a::NestedValues{Archimedean}) = error("Method for Type $(type(a)) not implemented")


# methods for Nesting

start(c::Nested{Archimedean}) = start(c.a)
nestings(c::Nested{Archimedean}) = Nested.(nestings(c.a))
map(f,c::Nested{Archimedean}) = map(f,c.a)


"""
    g(u...)   

Calling a copula to evaluate its multivariate distribution function 
"""
(g::Gumbel)(u::Float64...) = generator(g)(sum(invgenerator(g)(v) for v in u)) # works for any number of marginals

function (gg::Nested{Gumbel})(u::Float64...) # later (gg::NestedGumbel{N})(u::Vararg{N,Float64})
    length(u) == num_marginals(gg) || throw(DomainError(u,
        "$(num_marginals(gg)) arguments needed, equal to number of copulas marginals."))
    all(0.0 .<= u) && all(u .<= 1.0) || throw(DomainError(u, "all arguments must be between 0 and 1"))
    ng = start(gg).nnc
    v = collect(u)
    q, r = v[1:ng], v[ng+1:end]
    for h in nestings(gg)
        nh = num_marginals(h)
        p, r = r[1:nh], r[nh+1:end]
        push!(q, h(p...))
    end
    start(gg)(q...)
end


#  Note that sampling from NestedGumbel is easier than for the other NestedGumbel. The algorithm
#  below the origial from McNeil where the special case that the Laplace-Stieltjes transform
#  used in the recursive step is of the same family as for sampling from the Gumbel copula 
#  (compare  Tables 3 and 1 
#  in Hofert https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf )
#  The algorithm below can be modified so the cases of Algorithm 3.2 in Hofert are covered. 
"""
    sample(g, nsmp)

Drawing `nsmp` independent samples from  `NestedGumbel` copula.
"""
function sample(gg::Nested{Gumbel}, nsmp)
    smp = rand(start(gg).nnc, nsmp)
    theta0 = start(gg).theta
    for h in nestings(gg) # here Gumbel is easier than the other NestedArchimedeans 
        h1 = map(x -> Gumbel(x.theta / theta0, x.nnc), h)
        smp = [smp; sample(h1, nsmp)]
    end
    theta0 > 1.0 + 1e-10 || return smp # theta0 = 1 is the independent case and thus no mixing needed. 
    # The case theta0 > 1.0 + 1e-10 cannot be distinguished from the independent case
    sd = AlphaStable(α=1.0 / theta0, β=1.0)
    v = rand(sd, 1, nsmp) * (cos(π / (2.0 * theta0)))^theta0 # numerically instable if theta0 approaches 1.0. 
    return generator(start(gg)).(-log.(smp) ./ v)
end

"""
    uppertaildep(g)

# Example

```jldoctest
julia> uppertaildep(nest(Gumbel(2, 1.1), (Gumbel(3, 1.4), Gumbel(2, 2.5))))
NestedValues{Float64}: (0.12213817867658738, (0.3593292879847241, 0.6804920892271058))
```
"""
uppertaildep(gg::Nested{Gumbel}) = map(g -> 2 - 2^(1 / g.theta), gg)

