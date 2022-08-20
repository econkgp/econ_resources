% SLIDE - 6
child(peter).
child(dora).
child(john).
male(peter).
male(john).
male(felix).
female(dana).

boy(X) :- child(X),male(X).

% SLIDE - 8
advisor(minsky, moses).
advisor(papert, moses).
advisor(moses, genesereth).
advisor(genesereth, russell).
advisor(russell, bhaskara).
advisor(russell, milch).
advisor(russell, shaunak).
advisor(russell, friedman).
advisor(friedman, dana).
advisor(dana, felix).
advisor(dana, chen).
advisor(dana, amir).
advisor(dana, azizi).

grand_advisor(X,Z) :- advisor(X,Y), advisor(Y,Z).
ancestor(X,Z) :- advisor(X,Z).
ancestor(X,Z) :- advisor(X,Y),ancestor(Y,Z).

%SLIDE 16
ancestor1(X,Z) :- advisor(X, Z).
ancestor1(X,Z) :- advisor(X, Y), ancestor1(Y, Z).

%SLIDE 17
ancestor2(X,Z) :- advisor(X, Y), ancestor2(Y, Z).
ancestor2(X,Z) :- advisor(X, Z).

%SLIDE 18
ancestor3(X,Z) :- advisor(X, Z).
ancestor3(X,Z) :- ancestor3(Y, Z), advisor(X, Y).

%SLIDE 18
ancestor4(X,Z) :- ancestor4(Y, Z), advisor(X, Y).		
ancestor4(X,Z) :- advisor(X, Z).

%SLIDE 20
between_number(N1,N2) :- 	N1<N2-1, N is N1+1, 	
    						print(N),nl, NN1 is N1+1, 	
    						between_number(NN1,N2).
%Slide 20: Series Sum
series_sum(N,N,N).
series_sum(N1, N2, Sum) :- N1<N2, N is N1+1, 
     					   series_sum(N,N2,SumInter),
        				   Sum is SumInter + N1.

%SLIDE 21
%Color1 = [maroon, green],
%Color2 = [red, yellow],
%Clubs = [mohanB, Color1, eastB, Color2].

%L = [X1,X2|[X3,X4,X5]].

%SLIDE 21

%Concatenating two lists
conc([], L, L).
conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

%List Membership
member(X,[X|_]).
member(X, [_|Tail]) :- member(X, Tail).

%Partition with respect to pivot
partition(X,List,Before,After) :- conc(Before, [X|After], List).

%Before After elements
before_after(X,L,B,A) :- conc(_, [B,X,A|_],L).

%SLIDE 22
%Delete element from list
del(X,[X|L1],L1).
del(X,[Y|L1],[Y|L2]):-  del(X,L1,L2).

%List is ordered or not
ordered_list([]).
ordered_list([_]).
ordered_list([H|Tail]):- conc([H1],Tail1,Tail),   	 
    					 H=<H1,
    					 ordered_list([H1|Tail1]).

% Max in a list
max_list([X],X).

max_list([H|Tail],N):- max_list(Tail,N1), H>N1, N is H.
max_list([H|Tail],N):- max_list(Tail,N1), H=<N1, N is N1.

%SLIDE 24
see(a, 1, 5).
see(d, 4, 5).
see(e, 3, 1).

on(a, b).
on(b, c).
on(c, table).
on(d, table).
on(e, table).

%What are blocks in this world?

%Pairs of blocks having the same y-coordinate

%Boxes that are not visible

%Leftmost visible block

%z-coordinate of blocks


%Blocks between two blocks

%SLIDE 27
fib(0,0).
fib(1,1).
fib(X,N):-N>1, N1 is N-1, N2 is N-2, fib(X1, N1), fib(X2, N2), 
          X is X1+X2.


fib_seq(S, N) :-
   N > 1,
   fib_seq_(N, SR,1,[1,0]),      
   reverse(SR,S).             

fib_seq_(N,Seq,N,Seq).
fib_seq_(N,Seq,N0,[B,A|Fs]) :-
   N > N0,
   N1 is N0+1,
   C is A+B,
   fib_seq_(N,Seq,N1,[C,B,A|Fs]). 

%SLIDE 28
min(A, A, B) :- A =< B.
min(B, A, B) :- B =< A.

smallest(A, [A|[]]).
smallest(Min, [A|B]) :- smallest(SB, B), min(Min, A, SB).


sorted([], []).

sorted([Min|RestSorted], List) :-
  smallest(Min, List),
  append(BeforeMin, [Min|AfterMin], List), %To instantiate BeforeMin and AfterMin
  append(BeforeMin, AfterMin, RestUnsorted), %Pepare list that is List - Min
  sorted(RestSorted, RestUnsorted).


%Merge sort
merge(List, List, []).
merge(List, [], List).

merge([MinList1|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :-
  MinList1 =< MinList2,
  merge(RestMerged,RestList1,[MinList2|RestList2]).
merge([MinList2|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :-
  MinList2 =< MinList1,
  merge(RestMerged,[MinList1|RestList1],RestList2).

mergeSort([], []).
mergeSort([A], [A|[]]).

mergeSort(Sorted, List) :-
  length(List, N),
  FirstLength is //(N, 2),
  SecondLength is N - FirstLength,
  length(FirstUnsorted, FirstLength),
  length(SecondUnsorted, SecondLength),
  append(FirstUnsorted, SecondUnsorted, List),
  mergeSort(FirstSorted, FirstUnsorted),
  mergeSort(SecondSorted, SecondUnsorted),
  merge(Sorted, FirstSorted, SecondSorted).

%SLIDE 29
edge(1,2).
edge(1,6).
edge(2,3).
edge(2,5).
edge(3,4).
edge(4,5).
%edge(4,1).
edge(5,6).
%edge(6,3).

path2(N,N).
path2(S,D) :- edge(S,N), path2(N,D).

path1(N,N, [N]).
path1(S,D, L):-edge(S,N), path1(N,D,T), append([S],T,L).

%Knapsack 
subseq([],[]).
subseq([Item | RestX], [Item | RestY]) :-
subseq(RestX,RestY).
subseq(X, [_ | RestY]) :-
subseq(X,RestY).

weight([],0).
weight([food(_,W,_) | Rest], X) :-
weight(Rest,RestW),
X is W + RestW.

calories([],0).
calories([food(_,_,C) | Rest], X) :-
calories(Rest,RestC),
X is C + RestC.

%Knapsack Decision Problem
knapsackDecision(Pantry,Capacity,Goal,Knapsack) :-
subseq(Knapsack,Pantry),
weight(Knapsack,Weight),
Weight =< Capacity,
calories(Knapsack,Calories),
Calories >= Goal.

%Knapsack Optimization Problem
maxC([],Sofar,_,Sofar).
maxC([First | Rest],_,MC,Result) :-
calories(First,FirstC),
MC =< FirstC,
maxC(Rest,First,FirstC,Result).
maxC([First | Rest],Sofar,MC,Result) :-
calories(First,FirstC),
MC > FirstC,
maxC(Rest,Sofar,MC,Result).
maxCalories([First | Rest],Result) :-
calories(First,FirstC),
maxC(Rest,First,FirstC,Result).

legalKnapsack(Pantry,Capacity,Knapsack):-
subseq(Knapsack,Pantry),
weight(Knapsack,W),
W =< Capacity.

knapsackOptimization(Pantry,Capacity,Knapsack) :-
findall(K,legalKnapsack(Pantry,Capacity,K),L),
maxCalories(L,Knapsack).

nocheck(_, []).
nocheck(X/Y, [X1/Y1 | Rest]) :-
%X =\= X1,
Y =\= Y1,
abs(Y1-Y) =\= abs(X1-X),
nocheck(X/Y, Rest).

legal([]).
legal([X/Y | Rest]) :-
legal(Rest),
%member(X,[1,2,3,4,5,6,7,8]),
member(Y,[1,2,3,4,5,6,7,8]),
nocheck(X/Y, Rest).

eightqueens(X) :-
X = [1/_,2/_,3/_,4/_,5/_,6/_,7/_,8/_],
legal(X).






   























		






