using Test


# generators
gen0 = Gumbel(1.1)
gen1 = Gumbel(1.4)
gen2 = Gumbel(2.0)
gen3 = Gumbel(2.5)

@test gen0(3.0) ≈ 0.06621457152659894 atol =1e-15
@test inverse(gen0)(0.066) ≈ 3.003945630774493 atol =1e-15

# An archimeden copula is defined by a generator and a dimension 
g0 = Copula(gen0, 0)
g1 = Copula(gen1, 1)
g2 = Copula(gen2, 2)  
g3 = Copula(gen3, 3)

@test generator(g3) == gen3
@test family(g3) == Gumbel
@test dimension(g3) == 3

@test inverse(generator(g3))(0.0) == Inf
@test inverse(generator(g3))(1.0) == 0.0
## nesting copulas

# nest g3 into g1
h = nestedcopula(g1, g3)

@test dimension(h) == dimension(g1) + dimension(g3)

# evaluate the copula
@test h([0.3, 0.5, 0.7, 0.9]) ≈ 0.19852829072034361 atol = 1e-15

# we can nest h further, say into g0 together with g2
c = nestedcopula(g0, h, g2)

#This is the same as 
@test c == nestedcopula(g0, (g1,g3), g2)

@test dimension(c) == 6
u = map(upper_taildep, c)
@test typeof(u) == NestedValues{Float64}
ud = indexdata(u)
@test ud[[]] ≈ upper_taildep(g0) atol = 1e-15
@test ud[[1,1]] ≈ upper_taildep(g3) atol = 1e-15

@test map(z -> dimension(z),c) == nest(0,(1,3),2)
