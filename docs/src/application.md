# Applications

## Archimedean Copulas

`Nestings.jl` supports a neat application for nesting Archimedean copulas under monotonicity conditions.

The `ArchimedeanCopulas` submodule defines:

- `Archimedean` — abstract type for Archimedean generator families
- `Gumbel` — concrete Archimedean generator
- `Copula` — copula parameterised by an Archimedean generator and dimension
- `ArchimedeanMonotonicity` — nesting condition that ensures valid nested copulas
- `NestedCopula{A}` — alias for nested copulas built with a generator family
- `nestedcopula(c...)` — intuitive constructor for nested copula structures

### Why this fits Nestings

Nested copulas are a natural example of recursive structures with constraints:
- each copula node carries a generator and a dimension
- nested children are constrained by Archimedean monotonicity
- the package uses `NestedData{T,C}` to encode this cleanly

### Example

```julia
using Nestings
using Nestings.Applications.ArchimedeanCopulas

# Build a Gumbel copula nested under monotonicity
c = nestedcopula(Gumbel(1.5), Gumbel(2.0), Gumbel(3.0))

# Evaluate the nested copula at a point
u = [0.2, 0.5, 0.9]
c(u)
```

### What is enforced

The `ArchimedeanMonotonicity` condition extends `Nestings.IsIncreasing` by checking the generator ordering required for valid nested Archimedean copulas.

### Benefits

- Demonstrates `Nestings.jl` working beyond pure data structures
- Shows how nesting conditions can enforce domain-specific monotonicity
- Provides a clear model for nested statistical constructs

For the full API and all nesting utilities, see the API Reference.
