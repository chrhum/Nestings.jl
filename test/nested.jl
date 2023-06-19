using Test

# constructors
nst = NestedValues{Int64}[]
z = NestedValues{Int}(1, nst)
w = NestedValues(1, nst)

@test start(z) == 1
@test isempty(nestings(z))
@test z == w


z = NestedValues("root", ("child1", "grandchild1", "grandchild2"), "child2")

@test eltype(z) == String
@test start(z) == "root"
@test next(z) == ["child1", "child2"]

@test Nestings.represent(z) == ("root", ("child1", "grandchild1", "grandchild2"), "child2")

@test indexdata(z) == Dict([] => "root", [1] => "child1", [1, 1] => "grandchild1",
    [1, 2] => "grandchild2", [2] => "child2")
@test indexends(z) == Dict( [1, 1] => "grandchild1", [1, 2] => "grandchild2", [2] => "child2")
@test depth(z) == 2

w = NestedValues(1,(2,3),4)
@test map(x->x^2,w) == NestedValues(1^2,(2^2,3^2),4^2)

@test depth(NestedValues(1)) == 0
@test depth(NestedValues(1,(2,(3)))) == 2

@test !allnest(NestedValues(true,(true,true, false),true))
@test allnest(NestedValues(true,(true,true, true),true))

@test allnextunique(NestedValues(1,(1,2),2,3))
@test !allnextunique(NestedValues(1,(1,2),3,3))