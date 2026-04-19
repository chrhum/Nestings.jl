# Applications: Archimedean Copulas
The ArchimedeanCopulas submodule demonstrates how Nestings.jl enforces mathematical constraints on recursive structures.

## 1. Abstract Framework
A d-dimensional Archimedean copula is defined as:
$$C(u_1, \dots, u_d) = \phi(\phi^{-1}(u_1) + \dots + \phi^{-1}(u_d))$$
where $\phi$ is the generator of the copula.
For nested Archimedean copulas to be valid, the generators must satisfy specific monotonicity requirements. In Nestings.jl, this is handled by a specialized NestingCondition.
### Monotonicity and Validation
The condition ensures that for any two nested generators $\phi_1$ and $\phi_2$ (where $\phi_1$ is the parent), the composition $\phi_1^{-1} \circ \phi_2$ is completely monotonic.
```@docs
Nestings.ArchimedeanCopulas.ArchimedeanMonotonicity
```
### Core Methods
The framework defines abstract types and common functions for all Archimedean families.
```@docs
Nestings.ArchimedeanCopulas.Archimedean
Nestings.ArchimedeanCopulas.Copula
Nestings.ArchimedeanCopulas.NestedCopula
Nestings.ArchimedeanCopulas.generator
Nestings.ArchimedeanCopulas.inverse
Nestings.ArchimedeanCopulas.inverse_laplace_trafo
Nestings.ArchimedeanCopulas.isless
Nestings.ArchimedeanCopulas.family
Nestings.ArchimedeanCopulas.dimension
Nestings.ArchimedeanCopulas.mvcdf
```
## 2. Concrete Implementation: Gumbel
The Gumbel copula is defined by the generator:
$$\phi(t) = \exp(-t^{1/\theta}), \quad \theta \ge 1$$
It is a popular choice for modeling upper tail dependence.
```@docs
Nestings.ArchimedeanCopulas.Gumbel
```
### Construction
Use the nestedcopula function for an intuitive, tuple-based construction.
```@docs
Nestings.ArchimedeanCopulas.nestedcopula
```
### Hierarchical Gumbel structure
Condition: $\theta_{root} \le \theta_{child}$ (e.g., $1.5 \le 2.0$)
### Construct the nested copula

```julia

c = nestedcopula(Gumbel(1.5), (Gumbel(2.0), Gumbel(3.0)), Gumbel(2.5))
```
### Calculate the copula value at a specific point

```julia
u = [0.2, 0.5, 0.9, 0.8]
val = mvcdf(c, u)
```
## 3. Statistical Analysis
The submodule provides tools for sampling and analyzing the dependence structure.

```@docs
Nestings.ArchimedeanCopulas.sample
Nestings.ArchimedeanCopulas.lower_taildep
Nestings.ArchimedeanCopulas.upper_taildep
```
