using Test, TimeSpans, Dates

using TimeSpans: contains, nanoseconds_per_sample

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
    @test extend(t, by) === TimeSpan(start(t), max(start(t), stop(t) + Nanosecond(by)))
    @test extend(t, -by) === TimeSpan(start(t), max(start(t), stop(t) - Nanosecond(by)))
    @test repr(TimeSpan(6149872364198, 123412345678910)) == "TimeSpan(01:42:29.872364198, 34:16:52.345678910)"
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
    # test non-integer sample rates
    rate = 100.66
    ns_per_sample = nanoseconds_per_sample(rate)
    for i in 1:1000
        t = Nanosecond(ceil(Int, (i - 1) * ns_per_sample))
        @test index_from_time(rate, t) == i
        @test time_from_index(rate, i) == t
    end
end

@testset "`in` and `findall`" begin
    @test findall(in(TimeSpan(1, 10)), Nanosecond.(5:15)) == 1:5
    @test findall(in(TimeSpan(1, 10)), map(Nanosecond, (9,10,11))) == 1:1
    @test in(TimeSpan(1,2))(Nanosecond(1))
    @test !in(TimeSpan(1,2))(Nanosecond(2))
end

@testset "Set operations: (e.g. `intersect`, `union`, `setdiff`)" begin
    starts = Nanosecond.(rand(1:100_000, 50))
    spans = TimeSpan.(starts, starts .+ Nanosecond.(rand(1:10_000)))
    a, b = spans[1:25], spans[26:end]
    function testsets(a, b)
        @test sum(duration, (a ∪ b)) ≤ sum(duration, a) + sum(duration, b)
        @test sum(duration, setdiff(a, b)) ≤ sum(duration, a)
        @test sum(duration, (a ∩ b)) + sum(duration, symdiff(a, b)) ==
            sum(duration, union(a,b))
        @test a ⊆ (a ∪ b)
        @test !issetequal(a, b)
        @test issetequal(a, a)
        @static if VERSION ≥ v"1.5"
            @test isdisjoint(setdiff(a, b), b)
            @test !isdisjoint(a, a)
        end
    end
    testsets(spans[1:25], spans[26:end])
    testsets(spans[1], spans[26:end])
    testsets(spans[1:25], spans[26])
    start(starts[1]) ∈ spans

    # whitebox testing of the internal, `timeunion` function
    x = reduce(union, spans[2:end], spans[1]) 
    @test x == TimeSpans.timeunion(spans)
    @test_throws ErrorException x[1] = TimeSpan(Nanosecond(0), Nanosecond(1))
    span = TimeSpans.timeunion(spans[1])
    @test_throws ErrorException span[] = TimeSpan(Nanosecond(0), Nanosecond(1))
end

@testset "`rand` methods over `TimeSpan` and vectors of it." begin
    starts = rand(1:100_000, 50)
    spans = TimeSpan.(starts, starts .+ Nanosecond(rand(1:10_000)))
    @test all(t ∈ spans for t in rand(spans, 20))
end
