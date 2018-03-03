% Practica 3 definitiva

% notes:
% prohibit utilitzar ; com a OR

% concat --> append
% pert --> member


% 1

% flatten: aplana llistes:
% ?- flatten( [a,[b,c,[b,b],e], f], L).
% L = [a,b,c,b,b,e,f]



flatten([],[]):-!.
flatten([L1|L2],L):- !, flatten(L1,L1_flat), flatten(L2,L2_flat), append(L1_flat,L2_flat,L).
flatten(X,[X]).

% flatten sense repeticions
% ?- flattenNoRepetitions( [a,[b,c,[b,b],e], f], L).
% L = [a,b,c,e,f]

flattenNoRepetitions(L1,L2):-flatten(L1,L3),noDup(L3,L2).

noDup([],[]).
noDup(X,Y) :- setof(Z,member(Z,X),Y).
 













% tenim fila de 5 cases, 5 veins, 5 professions, 5 animals, 5 begudes, 5 nacionalitats
 % 1 - El que vive en la casa roja es de Peru
   % 2 - Al frances le gusta el perro
    % 3 - El pintor es japones
    % 4 - Al chino le gusta el ron
    % 5 - El hungaro vive en la primera casa
    % 6 - Al de la casa verde le gusta el coñac
    % 7 - La casa verde esta a la izquierda de la blanca
    % 8 - El escultor cría caracoles
    % 9 - El de la casa amarilla es actor
   % 10 - El de la tercera casa bebe cava
   % 11 - El que vive al lado del actor tiene un caballo
   % 12 - El hungaro vive al lado de la casa azul
   % 13 - Al notario la gusta el whisky
   % 14 - El que vive al lado del medico tiene un ardilla,
% casa,color, professio,animal,beguda,nacionalitat
% casas:-	Sol = [	[1,A1,B1,C1,D1,E1],
%		    [2,A2,B2,C2,D2,E2],
%		    [3,A3,B3,C3,D3,E3],
%		    [4,A4,B4,C4,D4,E4],
%		    [5,A5,B5,C5,D5,E5] ],
 %       member(  ... , Sol),
  %      ...
% 	write(Sol), nl.

casas:-	Sol = [	[1,A1,B1,C1,D1,E1],
		    [2,A2,B2,C2,D2,E2],
		    [3,A3,B3,C3,D3,E3],
		    [4,A4,B4,C4,D4,E4],
		    [5,A5,B5,C5,D5,E5] ],
		       %[num_casa,color,profesion,animal,bebida,pais]
		member([_,roja,_,_,_,peruano],Sol),
		member([_,_,_,perro,_,frances],Sol),
		member([_,_,pintor,_,_,japones],Sol),
		member([_,_,_,_,ron,chino],Sol),
		member([1,_,_,_,_,hungaro],Sol),
		member([_,verde,_,_,conac,_],Sol),
		member([X1,verde,_,_,_,_],Sol), member([Y1,blanca,_,_,_,_],Sol),X1 is Y1-1, % 7 
		member([_,_,escultor,caracol,_,_],Sol),
		member([_,amarilla,actor,_,_,_],Sol),
		member([3,_,_,_,cava,_],Sol),
		%member([X2,_,_,caballo,_,_],Sol), member([Y2,_,actor,_,_,_],Sol),(X2 is Y2-1; X2 is Y2+1), % 11
		member([X2,_,_,caballo,_,_],Sol), member([Y2,_,actor,_,_,_],Sol),lado(X2,Y2),   
		%member([X3,_,_,_,_,hungaro],Sol),member([Y3,azul,_,_,_,_],Sol), (X3 is Y3-1; X3 is Y3+1),
		member([X3,_,_,_,_,hungaro],Sol),member([Y3,azul,_,_,_,_],Sol), lado(X3,Y3),  
		member([_,_,notario,_,whisky,_],Sol),
		member([3,_,_,_,cava,_],Sol),
		member([X4,_,_,ardilla,_,_],Sol),member([Y4,_,medico,_,_,_],Sol), (X4 is Y4-1; X4 is Y4+1), 
		write(Sol), nl.


% prohibit utilitzar ;

lado(X,Y):- X is Y+1.
lado(X,Y):- X is Y-1.







% 3: reines



noDiagonal(_,[]):-!.
noDiagonal(A,[B|L]):-first(A,Ai),first(B,Bi),last(A,Aj),last(B, Bj),N1 is abs(Ai-Bi), N2 is abs(Aj-Bj), dif(N1,N2), noDiagonal(A,L).

noDiagonals([]):-!.
noDiagonals([A|L]):-noDiagonal(A,L),noDiagonals(L).

first([A|_],A).

nombres(0,[]).
nombres(1,[1]).
nombres(N,L):- append(L1,[N],L),N1 is N-1,nombres(N1,L1).

parelles([],[],[]).
parelles([I1|I],[J1|J],[[I1,J1]|P]):- parelles(I,J,P).


reines(N,Sol):-
	nombres(N,L),
    parelles(I,J,Sol),
	permutation(L,I),
	permutation(L,J),
	noDiagonals(Sol).

reinesUna(N,Sol):-
	nombres(N,L),
    parelles(I,J,Sol),
	permutation(L,I),
	permutation(L,J),
	noDiagonals(Sol),!.


tauler(N,M,_):- M > N, !.
tauler(N,I,Sol):- nombres(N,C),mVegadesN(N,I,F),parelles(F,C,P),formateja(P,Sol,P1),nl, I1 is I + 1, tauler(N,I1,P1).

mVegadesN(0,_,[]).
mVegadesN(M,N,[N|L]):- M1 is M-1, mVegadesN(M1,N,L).

formateja([],_,_):-!.
formateja([X|A],Sol,B):- member(X,Sol), write('x '), formateja(A,Sol,B),!.
formateja([X|A],Sol,B):- write('. '), subtract(Sol,X,B), formateja(A,B,B),!.

% aquest es el que s ha de cridar
escriureReines(N):- reinesUna(N,Sol),tauler(N,1,Sol).