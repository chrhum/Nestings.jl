"""
Archimedean copulas reprsent a special class of probability measures on a cube [0,1]^n
with uniform marginal distributions.

They can be nested under certain monotonicity conditions, see 
https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf

`Nestings.jl`enables the nesting of Archimedan copulas. 
"""
module ArchimedeanCopulas

using ..Nestings

using AlphaStableDistributions
using Lazy: @forward

export Archimedean, ArchimedeanMonotonicity, Gumbel, Copula, NestedCopula, nestedcopula, 
    generator, inverse, inverse_laplace_trafo, dimension,  upper_taildep, lower_taildep, family,
    sample

include("gumbel.jl")

end # module
