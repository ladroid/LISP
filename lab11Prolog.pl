program :- write('Hello, world!').
:- program.
%task1
sum(D,N,R):- N =< D, R = N,!.
sum(D,N,R):- N1 is N-D, sum(D,N1,R1),R is N+R1.
start:- write('enter D '),read(D),write('enter N '),read(N),sum(D,N,R),write(R).

%task2
degree(_,0,1):-!.
degree(A,1,A):-!.
degree(A,N,R):-N =< 0, N1 is (-N), degree(A,N1,R1), R is 1/R1,!.
degree(A,N,R):-N1 is N-1, degree(A,N1,R1), R is A * R1.

degree2(_,0,1):-!.
degree2(A,1,A):-!.
degree2(A,N,R):-(N mod 2) =:= 0, N1 is (N div 2), degree2(A,N1,R1), R is (R1 * R1),!.

%task3 will work sperately
sum(D,N,R):- N =< D, R = N,!.
sum(D,N,R):- N1 is N-D, sum(D,N1,R1),R is N+R1.
start:- write('enter N '),read(N),sum(1,N,R),write(R).

%task4
mysum(N,R):- N > 100, R = 0,!.
mysum(N,R):- N1 is N+1, mysum(N1,R1),R is (1/N/N)+R1.
