# TimeSpans.jl

[![CI](https://github.com/beacon-biosignals/TimeSpans.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/beacon-biosignals/TimeSpans.jl/actions/workflows/CI.yml)
[![codecov](https://codecov.io/gh/beacon-biosignals/TimeSpans.jl/branch/main/graph/badge.svg?token=CSZJKZC6HE)](https://codecov.io/gh/beacon-biosignals/TimeSpans.jl)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://beacon-biosignals.github.io/TimeSpans.jl/stable)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://beacon-biosignals.github.io/TimeSpans.jl/dev)

TimeSpans.jl provides a simple `TimeSpan` type for representing a continuous span between two points in time, along with generic utility functions for common operations on `TimeSpan`-like types. Importantly, the package exposes a minimal interface (`TimeSpans.start` and `TimeSpans.stop`) that any type can implement to enable support for the TimeSpans API. In addition to `TimeSpan`, this package implements the TimeSpans API for `Period`s and `NamedTuple`s that have `:start` and `:stop` keys.
