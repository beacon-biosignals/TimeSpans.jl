module TimeSpans

using Dates, StatsBase, Random

export TimeSpan, start, stop, istimespan, translate, overlaps,
       shortest_timespan_containing, duration, index_from_time, time_from_index,
       extend

#####
##### `TimeSpan`
#####

"""
    TimeSpan(start, stop)

Return `TimeSpan(Nanosecond(start), Nanosecond(stop))` representing the interval
`[start, stop)`.

If `start == stop`, a single `Nanosecond` is added to `stop` since `stop` is an
exclusive upper bound and TimeSpan operations only generally support up to
nanosecond precision anyway.

The benefit of this type over e.g.
`Nanosecond(start):Nanosecond(1):Nanosecond(stop)` is that instances of this
type are guaranteed to obey `TimeSpans.start(x) < TimeSpans.stop(x)` by
construction.

## Set operations

Time spans implement set operations from `Base` (e.g. `union`, `intersect`,
`setdiff`), which each return a read-only array of timespans, (possibly of
length 1), and can accept both single time span values and arrays of time spans.
Call `collect` on the returned values if you need a writable copy of the
returned result. An efficient implementation also exists for
`reduce(union, timespans)`.

## Sampling

Time spans (and arrays of these objects) implement `rand`, which generate random
time values within the given time span(s). When an array is passed, any overlap
between time spans is first removed by effectively calling `reduce(union,
timespans)`.

"""
struct TimeSpan
    start::Nanosecond
    stop::Nanosecond
    function TimeSpan(start::Nanosecond, stop::Nanosecond)
        stop += Nanosecond(start == stop)
        start < stop ||
            throw(ArgumentError("start(span) < stop(span) must be true, got $start and $stop"))
        return new(start, stop)
    end
    TimeSpan(start, stop) = TimeSpan(Nanosecond(start), Nanosecond(stop))
end

"""
    TimeSpan(x)

Return `TimeSpan(start(x), stop(x))`.
"""
TimeSpan(x) = TimeSpan(start(x), stop(x))

Base.in(x::TimePeriod, y::TimeSpan) = start(y) <= x < stop(y)

# work around <https://github.com/JuliaLang/julia/issues/40311>:
function Base.findall(pred::Base.Fix2{typeof(in),TimeSpan},
                      obj::Union{Tuple,AbstractArray})
    return invoke(findall, Tuple{Function,typeof(obj)}, pred, obj)
end

#####
##### pretty printing
#####

function nanosecond_to_periods(ns::Integer)
    μs, ns = divrem(ns, 1000)
    ms, μs = divrem(μs, 1000)
    s, ms = divrem(ms, 1000)
    m, s = divrem(s, 60)
    hr, m = divrem(m, 60)
    return (hr, m, s, ms, μs, ns)
end

format_duration(t::Period) = format_duration(convert(Nanosecond, t).value)

function format_duration(ns::Integer)
    hr, m, s, ms, μs, ns = nanosecond_to_periods(ns)
    hr = lpad(hr, 2, '0')
    m = lpad(m, 2, '0')
    s = lpad(s, 2, '0')
    ms = lpad(ms, 3, '0')
    μs = lpad(μs, 3, '0')
    ns = lpad(ns, 3, '0')
    return string(hr, ':', m, ':', s, '.', ms, μs, ns)
end

function Base.show(io::IO, w::TimeSpan)
    start_string = format_duration(start(w))
    stop_string = format_duration(stop(w))
    return print(io, "TimeSpan(", start_string, ", ", stop_string, ')')
end

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
    extend(span, by::Period)

Returns `TimeSpan(start(span), max(start(span), stop(span) + by)`
"""
function extend(x, by::Period)
    newstop = convert(Nanosecond, stop(x)) + convert(Nanosecond, by)
    return TimeSpan(start(x), max(start(x), newstop))
end

"""
    TimeSpans.contains(a, b)

Return `true` if the timespan `b` lies entirely within the timespan `a`, return `false` otherwise.
"""
contains(a, b) = start(a) <= start(b) && stop(a) >= stop(b)

"""
    overlaps(a, b)

Return `true` if the timespan `a` and the timespan `b` overlap, return `false` otherwise.
"""
function overlaps(a, b)
    starts_earlier, starts_later = ifelse(start(b) > start(a), (a, b), (b, a))
    return stop(starts_earlier) > start(starts_later)
end

"""
    shortest_timespan_containing(spans)

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
    duration(span)

Return `stop(span) - start(span)`.
"""
duration(span) = stop(span) - start(span)

"""
    TimeSpans.nanoseconds_per_sample(sample_rate)

Given `sample_rate` in Hz, return the number of nanoseconds corresponding to one sample.
"""
nanoseconds_per_sample(sample_rate) = inv(sample_rate) * 1_000_000_000

"""
    index_from_time(sample_rate, sample_time::Period)

Given `sample_rate` in Hz, return the integer index of the most recent sample
taken at `sample_time`. Note that `sample_time` must be non-negative and support
`convert(Nanosecond, sample_time)`.

Examples:

```jldoctest
julia> index_from_time(1, Second(0))
1

julia> index_from_time(1, Second(1))
2

julia> index_from_time(100, Millisecond(999))
100

julia> index_from_time(100, Millisecond(1000))
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
    index_from_time(sample_rate, span)

Return the `UnitRange` of indices corresponding to `span` given `sample_rate` in Hz:

```jldoctest
julia> index_from_time(100, TimeSpan(Second(0), Second(1)))
1:100

julia> index_from_time(100, TimeSpan(Second(1)))
101:101

julia> index_from_time(100, TimeSpan(Second(3), Second(6)))
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
    time_from_index(sample_rate, sample_index)

Given `sample_rate` in Hz and assuming `sample_index > 0`, return the earliest
`Nanosecond` containing `sample_index`.

Examples:

```jldoctest
julia> time_from_index(1, 1)
0 nanoseconds

julia> time_from_index(1, 2)
1000000000 nanoseconds

julia> time_from_index(100, 100)
990000000 nanoseconds

julia> time_from_index(100, 101)
1000000000 nanoseconds
```
"""
function time_from_index(sample_rate, sample_index)
    sample_index > 0 || throw(ArgumentError("`sample_index` must be > 0"))
    return Nanosecond(ceil(Int, (sample_index - 1) * nanoseconds_per_sample(sample_rate)))
end

"""
    time_from_index(sample_rate, sample_range::AbstractUnitRange)

Return the `TimeSpan` corresponding to `sample_range` given `sample_rate` in Hz:

```jldoctest
julia> time_from_index(100, 1:100)
TimeSpan(0 nanoseconds, 1000000000 nanoseconds)

julia> time_from_index(100, 101:101)
TimeSpan(1000000000 nanoseconds, 1000000001 nanoseconds)

julia> time_from_index(100, 301:600)
TimeSpan(3000000000 nanoseconds, 6000000000 nanoseconds)
```
"""
function time_from_index(sample_rate, sample_range::AbstractUnitRange)
    i, j = first(sample_range), last(sample_range)
    j = j == i ? j : j + 1
    return TimeSpan(time_from_index(sample_rate, i),
                    time_from_index(sample_rate, j))
end

#####
##### set operations
#####

# Set operations return some kind of `AbstractTimeSpanUnion` an
# AbstractTimeSpanUnion contains possibly multiple time spans and guarantees two
# invariants:
# 1. no overlap between timespans
# 2. time spans sorted from earlier to later start times
#
# These invariants are maintained to ensure that sequences of multiple set
# operations do not have to repeatedly check and preserve these invariants.
abstract type AbstractTimeSpanUnion <: AbstractVector{TimeSpan} end

function readOnlyError()
    # AbstractTimeSpanUnion types are for read only objects. The reasoning here
    # is that if you can modify the contents of any AbstractTimeSpanUnion, the
    # invariants may no longer hold if the containing struct were to be passed
    # to some new set operation.
    return error("This is a read only value. Call `collect` on the result to get " *
                 "an editable copy.")
end

# a `union` of a single time span
struct TimeSpanSingleton <: AbstractTimeSpanUnion
    data::TimeSpan
end
Base.@propagate_inbounds Base.getindex(x::TimeSpanSingleton, i...) = x.data
Base.size(::TimeSpanSingleton) = (1,)
Base.setindex(::TimeSpanSingleton, _) = readOnlyError()

# a `union` of multiple time spans
struct TimeSpanUnion{A} <: AbstractTimeSpanUnion
    data::A
    function TimeSpanUnion(x::AbstractVector{TimeSpan}, issorted=false,
                           nooverlap=false)
        sorted = issorted ? x : sort(x; by=start)
        merged = nooverlap ? sorted : sorted_timespan_union(sorted)
        return new{typeof(merged)}(merged)
    end
end
Base.parent(x::TimeSpanUnion) = x.data
Base.@propagate_inbounds Base.getindex(x::TimeSpanUnion, i...) = x.data[i...]
Base.setindex(x::TimeSpanUnion, i...) = readOnlyError()
Base.size(x::TimeSpanUnion) = size(x.data)

# `extend` preserves TimeSpanUnion invariants when `by` is negative
function Base.Broadcast.broadcasted(::typeof(extend), x::TimeSpanUnion, by)
    if by ≤ Nanosecond(0)
        TimeSpanUnion(extend.(x.data, by), true, true)
    else
        Broadcasted(extend, x, y)
    end
end

# `timeunion`: internal method to convert objects to `TimeSpanUnion` or
# `TimeSpanSignletone` as appropriate
timeunion(x) = TimeSpanUnion(x)
timeunion(data::TimeSpan) = TimeSpanSingleton(data)
timeunion(data::TimeSpanUnion) = data

# `sorted_timespan_union` merges a series of sorted timespans so there is no
# overlap between timespans
function sorted_timespan_union(spans::AbstractVector)
    result = TimeSpan[]
    isempty(spans) && return result

    sizehint!(result, length(spans))
    push!(result, spans[1])
    for span in @view(spans[2:end])
        if overlaps(result[end], span)
            result[end] = TimeSpan(start(result[end]), stop(span))
        else
            push!(result, span)
        end
    end
    return result
end

# efficeint implementation of `reduce(union, timespans)`
function Base.reduce(::typeof(union), spans::AbstractVector{TimeSpan};
                     init=TimeSpan[])
    spans = timeunion(spans)
    init = timeunion(init)
    if isempty(init)
        return spans
    else
        return union(init, spans)
    end
end

# set operations use a generic, higher-order function (`mergesets`) that merges
# two AbstractTimeSpanUnion objects: the passed operator should indicate, given
# the membership of a given point `t` in x and in y, whether the set operation
# `OP` should include the given point `t` in the result of `x` `OP` `y`
const MergableSpans = Union{TimeSpan,AbstractVector{TimeSpan}}
function Base.intersect(x::MergableSpans, y::MergableSpans)
    return mergesets((inx, iny) -> inx && iny, timeunion(x), timeunion(y))
end
function Base.union(x::MergableSpans, y::MergableSpans)
    return mergesets((inx, iny) -> inx || iny, timeunion(x), timeunion(y))
end
function Base.setdiff(x::MergableSpans, y::MergableSpans)
    return mergesets((inx, iny) -> inx && !iny, timeunion(x), timeunion(y))
end
function Base.symdiff(x::MergableSpans, y::MergableSpans)
    return mergesets((inx, iny) -> inx ⊻ iny, timeunion(x), timeunion(y))
end
function Base.issubset(x::MergableSpans, y::MergableSpans)
    return isempty(setdiff(x, y))
end
function Base.issetequal(x::MergableSpans, y::MergableSpans)
    x, y = timeunion.((x, y))
    length(x) != length(y) && return false
    # @infiltrate
    # why is this equal when collect(zip(x, y))[1][1] == collect(zip(x, y))[1][2]
    # is false?
    # return all(@show(xᵢ) == @show(yᵢ) for (xᵢ,yᵢ) in zip(x, y))
    return all(xᵢ == yᵢ for (xᵢ, yᵢ) in zip(x, y))
end
@static if VERSION ≥ v"1.5"
    function Base.isdisjoint(x::MergableSpans, y::MergableSpans)
        return isempty(intersect(x, y))
    end
end
Base.in(x::TimePeriod, y::AbstractVector{TimeSpan}) = any(yᵢ -> x ∈ yᵢ, y)

# `sortedsides` is an internal method that returns an iterator over the start
# and stop time points of two TimeSpanUnion's in order from earliest to latest
# time point. The returned value iterates over the time points and two flags
# which indicate whether the returned time point is located in x and in y. (Start
# points are in their set, end points are not)

function sortsides(x::AbstractTimeSpanUnion, y::AbstractTimeSpanUnion)
    return SortedSides(x, y)
end
struct SortedSides{A,B}
    x::A
    y::B
end
Base.length(sides::SortedSides) = 2length(sides.x) + 2length(sides.y)
struct End end
start(::End) = typemax(Nanosecond)
stop(::End) = typemax(Nanosecond)
side(x, isstart) = isstart ? start(x) : stop(x)

function Base.iterate(sides::SortedSides,
                      ((xi, xstart), (yi, ystart))=((1, true), (1, true)))
    x, y = get(sides.x, xi, End()), get(sides.y, yi, End())
    if side(x, xstart) < side(y, ystart)
        # advance x
        xstate = xstart ? (xi, false) : (xi + 1, true)
        return (side(x, xstart), xstart, !ystart), (xstate, (yi, ystart))
    elseif side(y, ystart) < typemax(Nanosecond)
        # advance both x and y
        if side(x, xstart) == side(y, ystart)
            xstate = xstart ? (xi, false) : (xi + 1, true)
            ystate = ystart ? (yi, false) : (yi + 1, true)
            return (side(x, xstart), xstart, ystart), (xstate, ystate)
            # advance y
        else
            ystate = ystart ? (yi, false) : (yi + 1, true)
            return (side(y, ystart), !xstart, ystart), ((xi, xstart), ystate)
        end
    else
        # finish iteration
        return nothing
    end
end

# `mergesets` is the primary internal method implementing set operations (see
# above for description of `op`). It iterates through the start and stop points
# in x and y, in order from lowest to highest. The implementation is based on
# the insight that we can make a decision to include or exclude a given start or
# stop time of the timespan (based on `op`) and all future points will yield the
# same decision, until we hit another start or stop point.
#
# For each start/stop point, we determine two things: 
#   1. whether the point should be included in the merge operation or not
#        (based on its member ship in `x` and `y`) by using `op`
#   2. whether the next step will 
#        a. define a region that will include this and future points (a start point)
#        b. define a region that will exclude this and future points (a stop point)
function mergesets(op, x::AbstractTimeSpanUnion, y::AbstractTimeSpanUnion)
    result = TimeSpan[]
    sizehint!(result, length(x) + length(y))

    spanstart = Nanosecond(0)

    # is the given time point included or excluded from x and/or y?
    t_in_x = false
    t_in_y = false

    # does the next time point we add to `result` define a region that
    # inclues (start) or excludes (stop) future points.
    point_will_include = true

    for (t, t_in_x, t_in_y) in sortsides(x, y)
        # should the current time point be included or excluded from the result?
        include_t = op(t_in_x, t_in_y)

        # do we add a new time point?
        if include_t == point_will_include
            if point_will_include
                spanstart = t
                point_will_include = false
            else
                push!(result, TimeSpan(spanstart, t))
                point_will_include = true
            end
        end
    end

    return TimeSpanUnion(result, true, true)
end

#####
##### Sampling from time spans 
#####

# sample from time points defined by a single time span
struct TimeSpanSampler{S} <: Random.Sampler{Nanosecond}
    sampler::S
end
Random.gentype(::TimeSpan) = Nanosecond

function Random.Sampler(RNG::Type{<:Random.AbstractRNG}, x::TimeSpan,
                        ::Random.Repetition)

    sampler = Random.Sampler(RNG, (start(x).value):(stop(x).value - 1))
    return TimeSpanSampler(sample)
end
function Base.rand(rng::Random.AbstractRNG, x::TimeSpanSampler)
    return Nanosecond(rand(rng, x.sampler))
end

# sample from the time points defined by multiple time spans
struct TimeSpanUnionSampler{T,W} <: Random.Sampler{Nanosecond}
    x::T
    weights::W
end
Random.gentype(::AbstractVector{TimeSpan}) = Nanosecond
function Random.Sampler(::Type{<:Random.AbstractRNG},
                        x::AbstractVector{TimeSpan}, ::Random.Repetition)
    unioned = timeunion(x)
    wts = weights(getproperty.(duration.(unioned), :value))
    return TimeSpanUnionSampler(x, wts)
end

function Random.rand(rng::Random.AbstractRNG, sampler::TimeSpanUnionSampler)
    span = sample(rng, sampler.x, sampler.weights)
    return rand(rng, span)
end

end # module
