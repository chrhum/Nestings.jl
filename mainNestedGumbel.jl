using Revise
using Nestings
using AbstractTrees
using Plots
using GraphRecipes

##  General commment on this nested gumbel implementation:

# framework is set up to work for nesting archimedean copulas as described in 
# https://cran.r-project.org/web/packages/copula/vignettes/nacopula-pkg.pdf
#
# The sampling algorithm is only implemented for Gumbel which is easier than others. It is
# the original one from A. McNeil. 
# The general case from M Hofert requires different distributions and poses 
# some numerical problems which are addressed in the R-package. 
#
# This implementation is independent of Copulas.jl
# Archimedean copulas are not supposed to be part of this package. 
# I added it to share my thoughts only.


## Suggetsion how to get familar with the implementation.

#  - Go through this script and use "help". Most types and functions are documented.
#  - Start with "help Nestings" and "help Archimedean" which contains an intro 
#    how nestings are handled generally and archimedean families are described. 


## Archimedean 

# generators
gen0 = Gumbel(1.1)
gen1 = Gumbel(1.4)
gen2 = Gumbel(2.0)
gen3 = Gumbel(2.5)

gen0(3.0)
inverse(gen0)(0.066)

# An archimeden copula is defined by a generator and a dimension 
g0 = Copula(gen0, 0)
g1 = Copula(gen1, 1)
g2 = Copula(gen2, 2)  
g3 = Copula(gen3, 3)

generator(g3)
family(g3)
dimension(g3)
inverse(generator(g3))

## nesting copulas

# nest g3 into g1
h = nestedcopula(g1, g3)

dimension(h)

# evaluate the copula
h([0.3, 0.5, 0.7, 0.9])

# we can nest h further, say into g0 together with g2
c = nestedcopula(g0, h, g2)

#This is the same as 
c == nestedcopula(g0, (g1,g3), g2)

dimension(c)
map(upper_taildep, c)
nonnestedcomponents =  map(z -> dimension(z),c)

# visualise
AbstractTrees.children(z::Nesting) = nestings(z)
AbstractTrees.printnode(io::IO, z::NestedCopula{Gumbel}) = 
print(io,(generator(start(z)).theta, dimension(start(z))))
default(size=(1000, 600))
plot(TreePlot(c), method=:tree, fontsize=7, nodeshape = :ellipse, title = "θ,dim")


# sampling
nsmp = 10000
U = sample(c, nsmp)

# testing
u = 1.0 .- rand(dimension(c)) .^ 6 # .^6 so that c(u) is not typically not too small

c(u)

# estimate c(u) empirically from the samples U
TF = U .< u'
estimate_cu = sum(all!(ones(Bool, nsmp, 1), TF)) / nsmp 

# c(u) and estimated_cu should be close. 
