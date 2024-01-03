# Benchmark to know cost of unwind

|implementation | count    | performance |
|---------------|----------|-------------|
|return         | 2452021  |     100.00% |
|global         | 2202721  |      89.83% |
|tuple          | 2178868  |      88.86% |
|jump           |  718723  |      29.31% |
|c++throw       |  266273  |      10.86% |
