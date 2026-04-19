# API Reference

This page provides a detailed overview of the types and functions available in `Nestings.jl`.

## Core Types and Interface

The package is built around the `Nesting` abstract type. Every concrete subtype must implement `start` and `nestings`.

```@docs
Nestings.Nesting
Nestings.NestingCondition
Nestings.NestedData
Nestings.NestedValues
Nestings.start
Nestings.nestings
```

## Construction and Validation

The nest function is the primary way to construct nested structures intuitively.

```@docs
Nestings.nest
Nestings.is_condition_valid
Nestings.isnestedtuple
Nestings.HasNoConstraint
Nestings.IsIncreasing
Nestings.NextAreDifferent
```

## Traversal and Information

Functions to query properties of a nested structure or retrieve its elements.

```@docs
Nestings.next
Nestings.elements
Nestings.depth
Nestings.represent
```

## Transformations and Mapping

Higher-order functions to transform the data within a nesting while preserving or changing its structure.

```@docs
Nestings.map
Nestings.transform
```

## Indexing and Analysis

Tools to create flat index maps of recursive structures and perform recursive checks.

```@docs
Nestings.indexdata
Nestings.indexends
Nestings.allnested
Nestings.allnextunique
Nestings.uniquenext
Nestings.isinitpartof
```