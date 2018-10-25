# viterbi

```
Input: observations of length N, state set of size L
Output: best-path
create a path probability matrix viterbi[N, L + 2]
create a path backpointer matrix backpointer[N, L + 2]
for each state s from 1 to L do
viterbi[1,s] ← trans(hSi,s) × emit(o1,s)
backpointer[1,s] ← 0
end
for each time step i from 2 to N do
for each state s from 1 to L do
viterbi[i,s] ← maxL
s
0=1
viterbi[i − 1,s
0
] × trans(s
0
,s) × emit(oi
,s)
backpointer[i,s] ← arg maxL
s
0=1
viterbi[i − 1,s
0
] × trans(s
0
,s)
end
end
viterbi[N, L + 1] ← maxL
s=1
viterbi[s, N] × trans(s,h/Si)
backpointer[N, L + 1] ← arg maxL
s=1
viterbi[N,s] × trans(s,h/Si)
return the path by following backpointers from backpointer[N, L + 1]
```
