# TimeSpans.jl

TimeSpans.jl provides a simple `TimeSpan` type for representing a continuous span between two points in time, along with generic utility functions for common operations on `TimeSpan`-like types. Importantly, the package exposes a minimal interface (`TimeSpans.start` and `TimeSpans.stop`) that any type can implement to enable support for the TimeSpans API.