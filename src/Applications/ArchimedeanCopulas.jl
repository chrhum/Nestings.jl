module ArchimedeanCopulas

using ..Nestings

using AlphaStableDistributions
using Lazy: @forward

export Archimedean, ArchimedeanMonotonicity, Gumbel, Copula, NestedCopula, nestedcopula, 
    generator, inverse, inverse_laplace_trafo, dimension,  upper_taildep, lower_taildep, family,
    sample

include("gumbel.jl")

end # module
