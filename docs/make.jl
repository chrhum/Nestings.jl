using Documenter
using Nestings

DocMeta.setdocmeta!(
    Nestings, 
    :DocTestSetup, 
    :(using Nestings, Nestings.ArchimedeanCopulas); 
    recursive=true
)

makedocs(
    sitename = "Nestings.jl",
    modules = [Nestings, Nestings.ArchimedeanCopulas],
    authors = "chrhum",
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        canonical = "https://chrhum.github.io/Nestings.jl"
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

deploydocs(
    repo = "://github.com/chrhum",
    devbranch = "master",
    push_preview = true
)
