using Test, TimeSpans, Dates

using TimeSpans: contains, nanoseconds_per_sample
using Statistics

function naive_index_from_time(sample_rate, sample_time)
    # This stepping computation is prone to roundoff error, so we'll work in high precision
    sample_time_in_seconds = big(Dates.value(Nanosecond(sample_time))) // big(TimeSpans.NS_IN_SEC)
    # At time 0, we are at index 1
    t = Rational{BigInt}(0//1)
    index = 1
    while true
        # Now step forward in time; one index, and time 1/sample_rate
        t += 1 // sample_rate
        index += 1
        if t > sample_time_in_seconds
            # we just passed it, so previous index is the last one before the time of interest
            return index - 1
        end
    end
end

@testset "basic TimeSpan code paths" begin
    t = TimeSpan(Nanosecond(rand(UInt32)))
    @test t == TimeSpan(t)
    @test t == TimeSpan(start(t), stop(t))
    @test t == TimeSpan(start(t), start(t))
    @test t == TimeSpan(start(t), start(t) + Nanosecond(1))
    @test contains(t, t)
    @test overlaps(t, t)
    @test start(t) ∈ t
    @test !(stop(t) ∈ t)
    @test stop(t) + Nanosecond(1) ∉ t
    @test shortest_timespan_containing([t]) == t
    @test shortest_timespan_containing((t,t,t)) == t
    @test shortest_timespan_containing(t, t) == t
    @test duration(TimeSpan(start(t), stop(t) + Nanosecond(100))) == Nanosecond(101)
    @test duration(start(t)) == Nanosecond(1)
    @test_throws ArgumentError TimeSpan(4, 2)
    @test istimespan(t)
    @test istimespan(start(t))
    @test !istimespan(1)
    @test !istimespan(1:10)
    by = Second(rand(1:10))
    @test translate(t, by) === TimeSpan(start(t) + Nanosecond(by), stop(t) + Nanosecond(by))
    @test translate(t, -by) === TimeSpan(start(t) - Nanosecond(by), stop(t) - Nanosecond(by))
    @test repr(TimeSpan(6149872364198, 123412345678910)) == "TimeSpan(01:42:29.872364198, 34:16:52.345678910)"

    # Periods and compound periods are supported
    for start in [Nanosecond(3), Minute(1), Minute(3) + Nanosecond(1)]
        stop = start + Nanosecond(8)
        start_ns = convert(Nanosecond, start)
        stop_ns = convert(Nanosecond, stop)
        @test TimeSpan(start, stop) == TimeSpan(start_ns, stop_ns) == TimeSpan(Dates.value(start_ns), Dates.value(stop_ns))
    end
    @test_throws MethodError TimeSpan(now(), now() + Nanosecond(1))
end

@testset "format_duration" begin
    @test TimeSpans.format_duration(3723004005006) == "01:02:03.004005006"
    @test TimeSpans.format_duration(-3723004005006) == "-01:02:03.004005006"
end

@testset "contains(::TimeSpan...)" begin
    @test contains(TimeSpan(10, 20), TimeSpan(10, 20))
    @test contains(TimeSpan(10, 20), TimeSpan(11, 19))
    @test contains(TimeSpan(11, 20), TimeSpan(11, 19))
    @test contains(TimeSpan(10, 19), TimeSpan(11, 19))
    @test !contains(TimeSpan(10, 20), TimeSpan(11, 21))
    @test !contains(TimeSpan(11, 20), TimeSpan(10, 19))
    @test !contains(TimeSpan(10, 19), TimeSpan(10, 21))
    @test !contains(TimeSpan(11, 19), TimeSpan(10, 20))
    @test contains(TimeSpan(1, 10), Nanosecond(4))
end

@testset "overlaps(::TimeSpan...)" begin
    @test overlaps(TimeSpan(10, 20), TimeSpan(10, 20))
    @test overlaps(TimeSpan(10, 20), TimeSpan(11, 19))
    @test overlaps(TimeSpan(11, 20), TimeSpan(11, 19))
    @test overlaps(TimeSpan(10, 19), TimeSpan(11, 19))
    @test overlaps(TimeSpan(10, 20), TimeSpan(11, 21))
    @test overlaps(TimeSpan(11, 20), TimeSpan(10, 19))
    @test overlaps(TimeSpan(10, 19), TimeSpan(10, 21))
    @test overlaps(TimeSpan(11, 19), TimeSpan(10, 20))
    @test !overlaps(TimeSpan(20, 30), TimeSpan(10, 20))
    @test !overlaps(TimeSpan(10, 20), TimeSpan(20, 30))
    @test !overlaps(TimeSpan(10, 20), TimeSpan(21, 30))
    @test !overlaps(TimeSpan(21, 30), TimeSpan(10, 20))
end

@testset "shortest_timespan_containing(spans)" begin
    @test shortest_timespan_containing([TimeSpan(1, 2),
                                        TimeSpan(5, 10),
                                        TimeSpan(2, 3)]) == TimeSpan(1, 10)
    @test shortest_timespan_containing([TimeSpan(3, 7),
                                        TimeSpan(1, 10),
                                        TimeSpan(2, 5)]) == TimeSpan(1, 10)
    @test shortest_timespan_containing(TimeSpan(1, 10),
                                       TimeSpan(4, 20)) == TimeSpan(1, 20)
end

@testset "time <--> index conversion" begin
    @test_throws ArgumentError time_from_index(200, 0)
    @test time_from_index(100, 1) == Nanosecond(0)
    @test time_from_index(100, 301:600) == TimeSpan(Second(3), Second(6))
    @test time_from_index(100, 101:101) == TimeSpan(Second(1), Nanosecond(1010000000))
    @test_throws ArgumentError index_from_time(200, Nanosecond(-1))
    @test index_from_time(100, Nanosecond(0)) == 1
    @test index_from_time(100, TimeSpan(Second(3), Second(6))) == 301:600
    @test index_from_time(100, TimeSpan(Second(1))) == 101:101

    # https://github.com/beacon-biosignals/TimeSpans.jl/issues/28
    @test index_from_time(1, Millisecond(1500)) == 2
    @test index_from_time(1, Millisecond(2500)) == 3
    @test index_from_time(1, TimeSpan(Millisecond(1500), Millisecond(2500))) == 2:3

    # test non-integer sample rates
    rate = 100.66
    ns_per_sample = nanoseconds_per_sample(rate)
    for i in 1:1000
        t = Nanosecond(ceil(Int, (i - 1) * ns_per_sample))
        @test index_from_time(rate, t) == i
        @test time_from_index(rate, i) == t
    end

    for rate in (101//2, 1001//10, 200, 256, 1, 10)
        for sample_time in (Nanosecond(12345), Minute(5), Nanosecond(Minute(5)) + Nanosecond(1),
                            Nanosecond(1), Nanosecond(10^6), Nanosecond(6970297031))
            # compute with a very simple algorithm
            index = naive_index_from_time(rate, sample_time)
            # Check against our `TimeSpans.index_from_time`:
            @test index == index_from_time(rate, sample_time)
            # Works even if `rate` is in Float64 precision:
            @test index == index_from_time(Float64(rate), sample_time)
        end
    end

    @testset "docstring" begin
        @test index_from_time(1, Second(0)) == 1
        @test index_from_time(1, Second(1)) == 2
        @test index_from_time(100, Millisecond(999)) == 100
        @test index_from_time(100, Millisecond(1000)) == 101
    end

    @testset "floating-point precision" begin
        ns = Nanosecond((2 * 60 + 30) * 1e9)
        @test index_from_time(200, ns) == 30001
        @test index_from_time(200e0, ns) == 30001
        @test index_from_time(200f0, ns) == 30001
        @test time_from_index(143.5, 8611) == Nanosecond(60000000000)
        @test time_from_index(Float32(143.5), 8611) == Nanosecond(60000000000)
    end

    for i in 1:10
        @test index_from_time(1.5, time_from_index(1.5, 1:i)) == 1:i
    end
end

@testset "`in` and `findall`" begin
    @test findall(in(TimeSpan(1, 10)), Nanosecond.(5:15)) == 1:5
    @test findall(in(TimeSpan(1, 10)), map(Nanosecond, (9,10,11))) == 1:1
    @test in(TimeSpan(1,2))(Nanosecond(1))
    @test !in(TimeSpan(1,2))(Nanosecond(2))
end

@testset "merge_spans!" begin
    spans = [TimeSpan(0, 10), TimeSpan(6, 12), TimeSpan(15, 20),
             TimeSpan(21, 30), TimeSpan(29, 31)]
    merge_spans!(overlaps, spans)
    @test spans == [TimeSpan(0, 12), TimeSpan(15, 20), TimeSpan(21, 31)]
    # No-op when the predicate is never `true`
    merge_spans!(overlaps, spans)
    @test spans == [TimeSpan(0, 12), TimeSpan(15, 20), TimeSpan(21, 31)]
    merge_spans!((a, b) -> true, spans)
    @test spans == [TimeSpan(0, 31)]
    @test merge_spans!((a, b) -> rand(Bool), TimeSpan[]) == TimeSpan[]
    @test merge_spans!((a, b) -> rand(Bool), [TimeSpan(0, 1)]) == [TimeSpan(0, 1)]
end

@testset "merge_spans" begin
    @test merge_spans((a, b) -> start(b) - stop(a) < Nanosecond(5),
                      (TimeSpan(0, 1), TimeSpan(4, 10))) == [TimeSpan(0, 10)]
    x = [TimeSpan(0, 10), TimeSpan(100, 200), TimeSpan(400, 1000)]
    @test merge_spans((a, b) -> true, x) == [shortest_timespan_containing(x)]
end

@testset "Statistics.middle" begin
    @test middle(TimeSpan(Nanosecond(0), Nanosecond(2))) == Nanosecond(1)
    @test middle(TimeSpan(Nanosecond(-1), Nanosecond(1))) == Nanosecond(0)
    # rounding
    @test middle(TimeSpan(Nanosecond(0), Nanosecond(1))) == Nanosecond(0)
    @test middle(TimeSpan(Nanosecond(0), Nanosecond(1)), RoundUp) == Nanosecond(1)
    @test middle(TimeSpan(Nanosecond(-1), Nanosecond(0))) == Nanosecond(0)
    @test middle(TimeSpan(Nanosecond(-1), Nanosecond(0)), RoundDown) == Nanosecond(-1)
end

@testset "invert_spans" begin
    parent_span = TimeSpan(Second(0), Second(60))
    # non-overlapping spans that extend to limits of parent_span
    spans = [TimeSpan(Second(x), Second(x + 1)) for x in 0:10:59]
    i_spans = invert_spans(spans, parent_span)
    @test length(i_spans) == 6
    @test all(duration.(i_spans) .== Second(9))
    spans = [TimeSpan(Second(x + 8), Second(x + 10)) for x in 0:10:50]
    i_spans = invert_spans(spans, parent_span)
    @test length(i_spans) == 6
    @test all(duration.(i_spans) .== Second(8))

    # non-overlapping spans that do not extend to limits of parent_span
    spans = [TimeSpan(Second(x + 1), Second(x + 2)) for x in 0:10:59]
    i_spans = invert_spans(spans, parent_span)
    @test length(i_spans) == 7
    @test i_spans[1] == TimeSpan(Second(0), Second(1))
    @test all(duration.(i_spans[2:6]) .== Second(9))
    @test i_spans[end] == TimeSpan(Second(52), stop(parent_span))

    # some spans lie outside of parent_span
    i_spans = invert_spans(spans, TimeSpan(Second(0), Second(30)))
    @test length(i_spans) == 4
    @test maximum(stop, i_spans) <= Second(30)
    # all spans lie outside of parent_span
    i_spans = invert_spans(spans, TimeSpan(Minute(10), Minute(30)))
    @test only(i_spans) == TimeSpan(Minute(10), Minute(30))

    # adjacent but not overlapping spans, unsorted
    spans = vcat([TimeSpan(Second(x), Second(x + 1)) for x in 0:10:59],
                 [TimeSpan(Second(x + 1), Second(x + 3)) for x in 0:10:59])
    i_spans = invert_spans(spans, parent_span)
    @test length(i_spans) == 6
    @test all(duration.(i_spans) .== Second(7))

    # overlapping, unsorted
    spans = vcat([TimeSpan(Second(x), Second(x + 1)) for x in 0:10:59],
                 [TimeSpan(Millisecond(x * 1000) + Millisecond(500), Second(x + 2))
                  for x in 0:10:59])
    i_spans = invert_spans(spans, parent_span)
    @test length(i_spans) == 6
    @test all(duration.(i_spans) .== Second(8))

    # empty
    @test invert_spans(TimeSpan[], parent_span) == [parent_span]

    # some spans cross the parent span's boundary
    i_spans = invert_spans([TimeSpan(-5, 3), TimeSpan(6, 8)], TimeSpan(0, 10))
    @test i_spans == [TimeSpan(3, 6), TimeSpan(8, 10)]
end

@testset "broadcast_spans" begin
    test_vec = [TimeSpan(0, 100), TimeSpan(0, 200)]
    test_vec .= TimeSpan(0, 300)
    @test test_vec == [TimeSpan(0, 300), TimeSpan(0, 300)]

    test_vec = []
    test_vec .= TimeSpan(0, 300)
    @test test_vec == []
end
