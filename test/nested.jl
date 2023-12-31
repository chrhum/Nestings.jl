using Test

# NestedValues
nst = NestedValues{Int64}[]
z = NestedValues{Int}(1, nst)
w = NestedValues(1, nst)
r = nest(Real,(1,(1//2,π), ℯ))
i = nest(0,1,1,(2,3,(4,5,5)), (7,7,7))
u = NestedValues{Int}(0,[i,z])



@test start(z) == 1
@test isempty(nestings(z))
@test z == w
@test next(r) == Real[1//2, ℯ]
@test next(u) == [0,1]
@test nest(eltype(i), Nestings.represent(i)) == i
@test nest(eltype(r), Nestings.represent(r)) == r
@test nestings(u) == [i,z]

@test nest(0,nest(1,(2,3),4)) == nest(0,(1,(2,3),4))

z = nest("root", ("child1", "grandchild1", "grandchild2"), "child2")

@test eltype(z) == String
@test start(z) == "root"
@test next(z) == ["child1", "child2"]

@test Nestings.represent(z) == ("root", ("child1", "grandchild1", "grandchild2"), "child2")

@test indexdata(z) == Dict([] => "root", [1] => "child1", [1, 1] => "grandchild1",
    [1, 2] => "grandchild2", [2] => "child2")
@test indexends(z) == Dict( [1, 1] => "grandchild1", [1, 2] => "grandchild2", [2] => "child2")
@test depth(z) == 2

w = nest(1,(2,3),4)
@test map(x->x^2,w) == nest(1^2,(2^2,3^2),4^2)


@test depth(NestedValues(1)) == 0
@test depth(nest(1,(2,(3)))) == 2

@test !allnested(nest(true,(true,true, false),true))
@test allnested(nest(true,(true,true, true),true))
@test allnested(map(x -> x isa Int, nest(1,(1,2),3,3)))

@test allnextunique(nest(1,(1,2),2,3))
@test !allnextunique(nest(1,(1,2),3,3))

#NestedData

i = nest(Int,Nestings.IsIncreasing(), (0,1,(2,3,(4,4))))
@test transform(x -> x^2,i) == nest(Int,Nestings.IsIncreasing(), (0,1,(4,9,(16,16))))
@test convert(NestedValues{Int}, i) == nest(0,1,(2,3,(4,4)))
@test convert(NestedData{Int,Nestings.NextAreDifferent}, i) == nest(Int,Nestings.NextAreDifferent(), (0,1,(2,3,(4,4))))