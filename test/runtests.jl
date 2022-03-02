using Test, TimeSpans, Dates

using TimeSpans: contains, nanoseconds_per_sample

function naive_index_from_time(sample_rate, sample_time)
    # This stepping computation is prone to roundoff error, so we'll work in high precision
    sample_time_in_seconds = big(Dates.value(convert(Nanosecond, sample_time))) // big(TimeSpans.NS_IN_SEC)
    # At time 0, we are at index 1
    t = Rational{BigInt}(0//1)
    index = 1
    while true
        # Now step forward in time; one index, and time 1/sample_rate
        t += 1 // sample_rate
        index += 1
        if t > sample_time_in_seconds
            # we just passed it, so prevoius index is the last one before the time of interest
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
    @test time_from_index(100, 101:101) == TimeSpan(Second(1))
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
        for sample_time in (Nanosecond(12345), Minute(5), Nanosecond(Minute(5)) + Nanosecond(1), Nanosecond(1), Nanosecond(10^6))
            @test naive_index_from_time(rate, sample_time) == TimeSpans.index_from_time(rate, sample_time)
        end
    end

end

@testset "`in` and `findall`" begin
    @test findall(in(TimeSpan(1, 10)), Nanosecond.(5:15)) == 1:5
    @test findall(in(TimeSpan(1, 10)), map(Nanosecond, (9,10,11))) == 1:1
    @test in(TimeSpan(1,2))(Nanosecond(1))
    @test !in(TimeSpan(1,2))(Nanosecond(2))
end

@testset "index_from_time" begin
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
    end
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
