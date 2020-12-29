using TimeSpans
using Documenter

makedocs(modules=[TimeSpans],
         sitename="TimeSpans",
         authors="Beacon Biosignals, Inc.",
         pages=["API Documentation" => "index.md"])

deploydocs(repo="github.com/beacon-biosignals/TimeSpans.jl.git")
