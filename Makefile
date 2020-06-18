MLTON=/opt/mlton-parmem/build/bin/mlton-parmem
MLTRACE=/opt/mlton-parmem/build/bin/mltrace
DBG=-keep g -debug true -debug-runtime true
FLAGS=

.PHONY: primes merge mapincr sumlist-fut tab tab.dbg fib fib.dbg fib-fut clean

all: fib fib-fut tab sumlist-fut mapincr merge primes

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

clean:
	rm -f tab tab.dbg fib fib.dbg tab.*.c fib.*.c sumlist-fut.*.c primes.*.c
