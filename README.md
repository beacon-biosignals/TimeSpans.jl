# TimeSpans.jl

[![CI](https://github.com/beacon-biosignals/TimeSpans.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/beacon-biosignals/TimeSpans.jl/actions/workflows/CI.yml)
[![codecov](https://codecov.io/gh/beacon-biosignals/TimeSpans.jl/branch/main/graph/badge.svg?token=CSZJKZC6HE)](https://codecov.io/gh/beacon-biosignals/TimeSpans.jl)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://beacon-biosignals.github.io/TimeSpans.jl/stable)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://beacon-biosignals.github.io/TimeSpans.jl/dev)

TimeSpans.jl provides a simple `TimeSpan` type for representing a continuous span between two points in time, along with generic utility functions for common operations on `TimeSpan`-like types. Importantly, the package exposes a minimal interface (`TimeSpans.start` and `TimeSpans.stop`) that any type can implement to enable support for the TimeSpans API.

## Example usage

```julia
julia> span = TimeSpan(Nanosecond(100), Nanosecond(1000))
TimeSpan(00:00:00.000000100, 00:00:00.000001000)

julia> start(span)
100 nanoseconds

julia> stop(span)
1000 nanoseconds

julia> duration(span)
900 nanoseconds
```

TimeSpans.jl supports common functions for comparing timespans, such as `contains` and `overlaps`:

```julia
julia> overlaps(TimeSpan(Minute(1), Minute(5)), TimeSpan(Minute(2), Minute(10)))
true
julia> TimeSpans.contains(TimeSpan(Minute(1), Minute(5)), TimeSpan(Minute(2), Minute(10)))
false
```

Operations on collections of timespans include `merge_spans` and `invert_spans`:

```julia
julia> spans = [TimeSpan(Minute(1), Minute(5)), TimeSpan(Minute(2), Minute(6)), TimeSpan(Minute(10), Minute(15))]
3-element Vector{TimeSpan}:
 TimeSpan(00:01:00.000000000, 00:05:00.000000000)
 TimeSpan(00:02:00.000000000, 00:06:00.000000000)
 TimeSpan(00:10:00.000000000, 00:15:00.000000000)

# 2 out of 3 spans overlap, returning 2 merged timespans
julia> merge_spans(overlaps, spans) 
2-element Vector{TimeSpan}:
 TimeSpan(00:01:00.000000000, 00:06:00.000000000)
 TimeSpan(00:10:00.000000000, 00:15:00.000000000)

# no timespans contain one another
julia> merge_spans(TimeSpans.contains, spans)
3-element Vector{TimeSpan}:
 TimeSpan(00:01:00.000000000, 00:05:00.000000000)
 TimeSpan(00:02:00.000000000, 00:06:00.000000000)
 TimeSpan(00:10:00.000000000, 00:15:00.000000000)

julia> parent_span = TimeSpan(Minute(0), Minute(15))
TimeSpan(00:00:00.000000000, 00:15:00.000000000)

# return spans within `parent_span` when provided `spans` are removed
julia> invert_spans(spans, parent_span)
2-element Vector{TimeSpan}:
 TimeSpan(00:00:00.000000000, 00:01:00.000000000)
 TimeSpan(00:06:00.000000000, 00:10:00.000000000)
```

Timespans can be indexed corresponding to a signal of a given sample rate, and vice versa.

```julia
julia> index_from_time(100, TimeSpan(Second(0), Second(1)))
1:100

julia> index_from_time(100, TimeSpan(Second(1)))
101:101

julia> index_from_time(100, TimeSpan(Second(3), Second(6)))
301:600

julia> time_from_index(1, 1)
0 nanoseconds

julia> time_from_index(1, 2)
1000000000 nanoseconds

julia> time_from_index(100, 100)
990000000 nanoseconds

julia> time_from_index(100, 101)
1000000000 nanoseconds
```
