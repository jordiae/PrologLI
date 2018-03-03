:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

% predicats afegits

insideBigSquare(_,_,[],[]).
insideBigSquare(N,Big,[S|Sides],[V|Vars]):- Big #>= S+V-1, insideBigSquare(N,Big,Sides,Vars).
% nota: de que serveix la N del predicat? La mantinc perque surt a l original, pero no l utilitzo


nonoverlapping(_,[],[],[]).
nonoverlapping(N,[S|Sides],[R|RowVars],[C|ColVars]):- nonoverlapping2(S,Sides,R,RowVars,C,ColVars),nonoverlapping(N,Sides,RowVars,ColVars).

% Hem de comprovar que totes, totes, les parelles de quadrats no tinguin overlapping. 
% Ho hem mirant els extrems dels quadrats per les dues coordenades.

nonoverlapping2(_,[],_,[],_,[]).
nonoverlapping2(S1,[S2|Sides],R1,[R2|RowVars],C1,[C2|ColVars]):- 
	R2 #> R1+S1-1 #\/ R2+S2-1 #< R1 #\/ C2 #> C1+S1-1 #\/ C2+S2-1 #< C1,
	nonoverlapping2(S1,Sides,R1,RowVars,C1,ColVars).

main:- 
    ejemplo(3,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides,N), 
    length(RowVars,N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars,N),
    RowVars ins 1..Big,
    ColVars ins 1..Big,
    
    insideBigSquare(N,Big,Sides,RowVars),
    insideBigSquare(N,Big,Sides,ColVars),
    nonoverlapping(N,Sides,RowVars,ColVars),
    
    % hem de fer tot el labeling "de cop"
    append(RowVars,ColVars,Vars),
    %labeling([],Vars), % (default opts: leftmost, up, step)
    
    % Utilitzant els default opts triga massa. Optimitzem:
    
    labeling([ff],Vars), % Selection: ff. Segons la web oficial, ff: First fail. Label the leftmost variable with smallest domain next, in order to detect infeasibility early. This is often a good strategy.


    displaySol(N,Sides,RowVars,ColVars), halt.



displaySol(N,Sides,RowVars,ColVars):- 
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.

