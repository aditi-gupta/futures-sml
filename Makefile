MLTON=/opt/mlton-parmem/build/bin/mlton-parmem
MLTRACE=/opt/mlton-parmem/build/bin/mltrace
DBG=-keep g -debug true -debug-runtime true
FLAGS=

.PHONY: qsort primes-fut merge sumlist-fut tab tab.dbg fib fib.dbg fib-fut clean

all: qsort fib fib-fut tab sumlist-fut merge primes-fut

fib-fut: fib-fut.sml fib-fut.mlb
	$(MLTON) $(FLAGS) -output fib-fut fib-fut.mlb

fib: fib.sml fib.mlb
	$(MLTON) $(FLAGS) -output fib fib.mlb

fib.dbg: fib.sml fib.mlb
	$(MLTON) $(FLAGS) $(DBG) -output fib.dbg fib.mlb

tab: tab.sml tab.mlb
	$(MLTON) $(FLAGS) -output tab tab.mlb

tab.dbg: tab.sml tab.mlb
	$(MLTON) $(FLAGS) $(DBG) -output tab.dbg tab.mlb

sumlist-fut: sumlist-fut.sml sumlist-fut.mlb
	$(MLTON) $(FLAGS) -output sumlist-fut sumlist-fut.mlb

sumlist-fut.dbg: sumlist-fut.sml sumlist-fut.mlb
	$(MLTON) $(FLAGS) $(DBG) -output sumlist-fut.dbg sumlist-fut.mlb

merge: merge.sml merge.mlb
	$(MLTON) $(FLAGS) -output merge merge.mlb

primes-fut: primes-fut.sml primes-fut.mlb
	$(MLTON) $(FLAGS) -output primes-fut primes-fut.mlb

qsort: qsort.sml qsort.mlb
	$(MLTON) $(FLAGS) -output qsort qsort.mlb

clean:
	rm -f tab tab.dbg fib fib.dbg tab.*.c fib.*.c sumlist-fut.*.c
