using Documenter
using Nestings

makedocs(
    sitename = "Nestings.jl",
    modules = [Nestings],
    authors = "chrhum",
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        canonical = "https://github.com/chrhum/Nestings.jl",
        edit_link = "master",
    ),
    pages = [
        "Home" => "index.md",
        "API Reference" => "api.md",
        "Applications" => "application.md",
    ],
    doctest = false,
    warnonly = [:missing_docs, :cross_references, :docs_block],
)


