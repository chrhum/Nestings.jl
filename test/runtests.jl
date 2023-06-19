using Nestings
using Test


const tests = [
    "nested"
]

for t in tests
    @testset "Test $t" begin
        include("$t.jl")
    end
end

