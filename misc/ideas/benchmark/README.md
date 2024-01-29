# Benchmark to know cost of unwind

|implementation | count       | performance |
|---------------|-------------|-------------|
|return         | 1952060424  |     100.00% |
|global         | 1960441329  |     100.43% |
|tuple          | 1960245758  |     100.42% |
|jump           |    1071334  |       0.05% |
|c++throw       |     302770  |       0.02% |
