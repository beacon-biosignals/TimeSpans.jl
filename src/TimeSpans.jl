module TimeSpans

using Dates

export TimeSpan, start, stop

#####
##### `TimeSpan`
#####

"""
    TimeSpan(start, stop)

Return `TimeSpan(Nanosecond(start), Nanosecond(stop))` representing the interval `[start, stop)`.

If `start == stop`, a single `Nanosecond` is added to `stop` since `stop` is an exclusive
upper bound and TimeSpan operations only generally support up to nanosecond precision anyway.

The benefit of this type over e.g. `Nanosecond(start):Nanosecond(1):Nanosecond(stop)` is
that instances of this type are guaranteed to obey `TimeSpans.start(x) < TimeSpans.stop(x)`
by construction.
"""
struct TimeSpan
    start::Nanosecond
    stop::Nanosecond
    function TimeSpan(start::Nanosecond, stop::Nanosecond)
        stop += Nanosecond(start == stop)
        start < stop || throw(ArgumentError("start(span) < stop(span) must be true, got $start and $stop"))
        return new(start, stop)
    end
    TimeSpan(start, stop) = TimeSpan(Nanosecond(start), Nanosecond(stop))
end

"""
    TimeSpan(x)

Return `TimeSpan(start(x), stop(x))`.
"""
TimeSpan(x) = TimeSpan(start(x), stop(x))

#####
##### generic TimeSpans.jl interface
#####

"""
    istimespan(x)

Return `true` if `x` has been declared to support `TimeSpans.start(x)` and `TimeSpans.stop(x)`,
return `false` otherwise.

Types that overload `TimeSpans.start`/`TimeSpans.stop` should also overload `istimespan`.
"""
istimespan(::Any) = false
istimespan(::TimeSpan) = true
istimespan(::Period) = true

"""
    start(span)

Return the inclusive lower bound of `span` as a `Nanosecond` value.
"""
start(span::TimeSpan) = span.start
start(t::Period) = convert(Nanosecond, t)

"""
    stop(span)

Return the exclusive upper bound of `span` as a `Nanosecond` value.
"""
stop(span::TimeSpan) = span.stop
stop(t::Period) = convert(Nanosecond, t) + Nanosecond(1)

#####
##### generic utilities
#####

"""
    translate(span, by::Period)

Return `TimeSpan(start(span) + by, stop(span) + by)`.
"""
function translate(span, by::Period)
    by = convert(Nanosecond, by)
    return TimeSpan(start(span) + by, stop(span) + by)
end

"""
    TimeSpans.contains(a, b)

Return `true` if the timespan `b` lies entirely within the timespan `a`, return `false` otherwise.
"""
contains(a, b) = start(a) <= start(b) && stop(a) >= stop(b)

"""
    TimeSpans.overlaps(a, b)

Return `true` if the timespan `a` and the timespan `b` overlap, return `false` otherwise.
"""
function overlaps(a, b)
    starts_earlier, starts_later = ifelse(start(b) > start(a), (a, b), (b, a))
    return stop(starts_earlier) >= start(starts_later)
end

"""
    TimeSpans.shortest_timespan_containing(spans)

Return the shortest possible `TimeSpan` containing all timespans in `spans`.

`spans` is assumed to be an iterable of timespans.
"""
function shortest_timespan_containing(spans)
    isempty(spans) && throw(ArgumentError("input iterator must be nonempty"))
    lo, hi = Nanosecond(typemax(Int64)), Nanosecond(0)
    for span in spans
        lo = min(start(span), lo)
        hi = max(stop(span), hi)
    end
    return TimeSpan(lo, hi)
end

"""
    TimeSpans.duration(span)

Return `stop(span) - start(span)`.
"""
duration(span) = stop(span) - start(span)

nanoseconds_per_sample(sample_rate) = inv(sample_rate) * 1_000_000_000

"""
    TimeSpans.index_from_time(sample_rate, sample_time::Period)

Given `sample_rate` in Hz, return the integer index of the most recent sample
taken at `sample_time`. Note that `sample_time` must be non-negative and support
`convert(Nanosecond, sample_time)`.

Examples:

```
julia> TimeSpans.index_from_time(1, Second(0))
1

julia> TimeSpans.index_from_time(1, Second(1))
2

julia> TimeSpans.index_from_time(100, Millisecond(999))
100

julia> TimeSpans.index_from_time(100, Millisecond(1000))
101
```
"""
function index_from_time(sample_rate, sample_time::Period)
    time_in_nanoseconds = convert(Nanosecond, sample_time).value
    time_in_nanoseconds >= 0 || throw(ArgumentError("`sample_time` must be >= 0 nanoseconds"))
    ns_per_sample = nanoseconds_per_sample(sample_rate)
    return floor(Int, time_in_nanoseconds / ns_per_sample) + 1
end

"""
    TimeSpans.index_from_time(sample_rate, span)

Return the `UnitRange` of indices corresponding to `span` given `sample_rate` in Hz:

```
julia> TimeSpans.index_from_time(100, TimeSpan(Second(0), Second(1)))
1:100

julia> TimeSpans.index_from_time(100, TimeSpan(Second(1)))
101:101

julia> TimeSpans.index_from_time(100, TimeSpan(Second(3), Second(6)))
301:600
```
"""
function index_from_time(sample_rate, span)
    i = index_from_time(sample_rate, start(span))
    j = index_from_time(sample_rate, stop(span))
    j = i == j ? j : (j - 1)
    return i:j
end

"""
    TimeSpans.time_from_index(sample_rate, sample_index)

Given `sample_rate` in Hz and assuming `sample_index > 0`, return the earliest
`Nanosecond` containing `sample_index`.

Examples:

```
julia> TimeSpans.time_from_index(1, 1)
0 nanoseconds

julia> TimeSpans.time_from_index(1, 2)
1000000000 nanoseconds

julia> TimeSpans.time_from_index(100, 100)
990000000 nanoseconds

julia> TimeSpans.time_from_index(100, 101)
1000000000 nanoseconds
```
"""
function time_from_index(sample_rate, sample_index)
    sample_index > 0 || throw(ArgumentError("`sample_index` must be > 0"))
    return Nanosecond(ceil(Int, (sample_index - 1) * nanoseconds_per_sample(sample_rate)))
end

"""
    TimeSpans.time_from_index(sample_rate, sample_range::AbstractUnitRange)

Return the `TimeSpan` corresponding to `sample_range` given `sample_rate` in Hz:

```
julia> TimeSpans.time_from_index(100, 1:100)
TimeSpan(0 nanoseconds, 1000000000 nanoseconds)

julia> TimeSpans.time_from_index(100, 101:101)
TimeSpan(1000000000 nanoseconds, 1000000001 nanoseconds)

julia> TimeSpans.time_from_index(100, 301:600)
TimeSpan(3000000000 nanoseconds, 6000000000 nanoseconds)
```
"""
function time_from_index(sample_rate, sample_range::AbstractUnitRange)
    i, j = first(sample_range), last(sample_range)
    j = j == i ? j : j + 1
    return TimeSpan(time_from_index(sample_rate, i),
                    time_from_index(sample_rate, j))
end

end # module
