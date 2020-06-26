MLTON=/opt/mlton-parmem/build/bin/mlton-parmem
MLTRACE=/opt/mlton-parmem/build/bin/mltrace
DBG=-keep g -debug true -debug-runtime true
FLAGS=

.PHONY: primes-fut primes3 primes2 primes merge mapincr sumlist-fut tab tab.dbg fib fib.dbg fib-fut clean

all: fib fib-fut tab sumlist-fut mapincr merge primes primes2 primes3 primes-fut

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

mapincr: mapincr.sml mapincr.mlb
	$(MLTON) $(FLAGS) -output mapincr mapincr.mlb

mapincr.dbg: mapincr.sml mapincr.mlb
	$(MLTON) $(FLAGS) $(DBG) -output mapincr.dbg mapincr.mlb

merge: merge.sml merge.mlb
	$(MLTON) $(FLAGS) -output merge merge.mlb

primes: primes.sml primes.mlb
	$(MLTON) $(FLAGS) -output primes primes.mlb

primes2: primes2.sml primes2.mlb
	$(MLTON) $(FLAGS) -output primes2 primes2.mlb

primes3: primes3.sml primes3.mlb
	$(MLTON) $(FLAGS) -output primes3 primes3.mlb

primes-fut: primes-fut.sml primes-fut.mlb
	$(MLTON) $(FLAGS) -output primes-fut primes-fut.mlb

clean:
	rm -f tab tab.dbg fib fib.dbg tab.*.c fib.*.c sumlist-fut.*.c primes.*.c
