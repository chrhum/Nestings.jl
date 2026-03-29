using Nestings
using Test

# 1. List of core test files (without .jl extension)
const core_tests = [
    "nested",
    # "other_tests" # Add more files here as the project grows
]

# 2. Run core tests
@testset "Nestings Core" begin
    for t in core_tests
        @testset "Test: $t" begin
            # Include files from the same directory as runtests.jl
            include("$t.jl")
        end
    end
end

# 3. Submodule tests (Opt-in application)
@testset "ArchimedeanCopulas" begin
    # Import the submodule for testing
    using Nestings.ArchimedeanCopulas
    
    # You can either add tests here directly or include a separate file
    include("archimedean.jl")
    
end
