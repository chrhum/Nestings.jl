
using Revise
using Nestings
using Graphs
using BenchmarkTools

## playground

getindices(z::Nesting) = _nstidx(z,Int[])

function _nstidx(z,v::Vector{Int})
    ni = [v]
    for (k,w) in enumerate(nestings(z))
        vk = copy(v)
        append!(ni,nstidx(w,push!(vk,k)))
    end
    return ni
end

@time getindices(z)

##

addindices(z::Nesting) = _addidx(z,Int[])

function _addidx(z,v)
    S = Pair{Vector{Int},eltype(z)}
    nst = NestedValues{S}[]
    for (k,w) in enumerate(nestings(z))
        vk = vcat(first(v),k)
        push!(nst, foop(w, vk => start(w)))
    end
    return NestedValues{S}(v,nst)
end


##
nestsum(z) = isempty(nestings(z)) ? start(z) : start(z) + sum(nestsum.(nestings(z)))

isend(z) = isempty(nestings(z)) ? NestedValues{Bool}(true,Bool[]) : NestedValues{Bool}(false,isend.(nestings(z)))


##
function nestedsample(c::NestedCopula{Gumbel}, nsmp::Int)
    smp = rand(nsmp, dimension(start(c)))
    
    nst = NestedValues{Matrix{Float64}}[]

    for h in nestings(c) # here Gumbel is easier than the other NestedArchimedeans 
        h1 = h / generator(start(c))
        push!(nst, nestedsample(h1, nsmp))
    end
    z = NestedValues(smp,nst)
    generator(start(c)).theta > 1.0 + 1e-10 || return z # cannot be distinguished from the independent case
    v = rand(inverse_laplace_trafo(generator(start(c))), nsmp)

    return map(s -> generator(start(c)).(-log.(s) ./ v),z)
end

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
