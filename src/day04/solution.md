# [Day 4](https://adventofcode.com/2025/day/4) solution

## Expected Result:

```console
❯ aoc2025/src/day04 ghc day04.hs -O -o day04 && time ./day04                                                                                              on  master [!] via  9.12.2
Loaded package environment from /home/armino112/.ghc/x86_64-linux-9.12.2/environments/default
[1 of 2] Compiling Main             ( day04.hs, day04.o ) [Source file changed]
[2 of 2] Linking day04 [Objects changed]
Part 1 1543
Part 2 9038
real    0m3.360s
user    0m3.360s
sys     0m0.000s

```

### Note:
Horrible perf... need to use Data.Array and maybe some sort of ST but skill issue still

