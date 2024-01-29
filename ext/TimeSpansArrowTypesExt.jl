module TimeSpansArrowTypesExt

using ArrowTypes
using TimeSpans

const TIME_SPAN_ARROW_NAME = Symbol("JuliaLang.TimeSpan")

ArrowTypes.arrowname(::Type{TimeSpan}) = TIME_SPAN_ARROW_NAME
ArrowTypes.JuliaType(::Val{TIME_SPAN_ARROW_NAME}) = TimeSpan

end
