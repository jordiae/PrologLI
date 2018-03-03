% pert(X,L). Dona L una llista, X es un element de la llista. Sota BT em dona tots els X possibles.

pert(X,[X|_]).
pert(X,[_|L]):-pert(X,L).

% concat(L1,L2,C). C es la concatenacio de les llistes L1 i L2
concat([],L,L).
concat([X|L1],L2,[X|L]):-concat(L1,L2,L).

pert_con_resto(X,L,R):-concat(L1,[X|L2],L),concat(L1,L2,R).

% suma(L,S). Donada L una llista d enters, S es la seva suma.
suma([],0).
suma([X|L],S):-suma(L,S1),S is S1+X.

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

% Exercicis
% 2
% prod(L,P). P es el producte dels elements de la llista d enters donada L.

prod([],1).
prod([X|L],P):-prod(L,P1),P is P1*X.

%3 pescalar: donats els vectors en forma de llista L1 i L2, P n es el producte escalar
pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):-pescalar(L1,L2,P1),P is P1+X*Y.
% 4
%  unio Representant conjunts amb llistes sense repeticions, escriu predicats per les operacions de interseccio i unio de conjunts donats
unio([],L,L).
unio([X|L1],L2, U):-pert(X,L2), !, unio(L1,L2,U).
unio([X|L1],L2,[X|U]):-unio(L1,L2,U).

% interseccio
interseccio([],_,[]).
interseccio([X|L1],L2,[X|L3]):-pert(X,L2), !, interseccio(L1,L2,L3).
interseccio([_|L1],L2,L3):-interseccio(L1,L2,L3).

% 5

% ultim element i llista inversa, utilitzant concat
% ultim(L,X) vol dir que X es l ultim element de L
ultim(L,X):- concat(_,[X],L).

invers([],[]).
invers(L,[X|L1]):- concat(L2,[X],L), invers(L2,L1).

% 6

% fib(N,F) F es el N-esim numero de fibonaccio per a la N donada
% fib(1) = 1, fib(2) = 1, si N > 2 com fib(N) = fib(N-1)+fib(N-2)
fib(1,1).
fib(2,1).
fib(N,F):- N > 2, N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.

% 7

% dados(P,N,L) la lista L expresa una manera de sumar P puntos lanzando N dados

dados(0,0,[]).
dados(P,N,[X|L]):- N > 0, pert(X,[1,2,3,4,5,6]), Q is P-X, M is N-1, dados(Q,M,L).

% 8

% suma_demas(L) se satisface si existe algun elemento en L que es igual a la suma de los demas elementos de L
%suma_demas([X|L]):- suma(L,S), X is S.
suma_demas(L):- pert_con_resto(X,L,R), suma(R,X), !.

% 9

% suma_ants(L) dada una lista de enteros L, se satisface si existe algun elemento en L que es igual a la suma de los elementos anteriores a el en L, y falla en caso contrario
suma_ants(L):- concat(L1,[X|_],L), suma(L1,X), !.

% 10

% card(L) dada una lista de enteros L, escribe la lista que, para cada elemento de L, dice cuantas veces aparece este elemento en L

card([],[]).
card([X|L],[[X,C1]|Kr]):- card(L,K), pert_con_resto([X,C],K,Kr), !, C1 is C+1.
card([X|L],[[X,1]|K]):-card(L,K).

card(L):- card(L,K), write(K).



% 11 
% esta_ordenada(L), la lista L de numeros enteros esta ordenada de menor a mayor
esta_ordenada([]).
esta_ordenada([_]):- !.
% segons solucio el tall no cal
esta_ordenada([X,Y|L]):- X =< Y, esta_ordenada([Y|L]).
% alternativa anterior que FALLAVA: esta_ordenada([X|L]):- pert_con_resto(Y,L,L1), X =< Y, esta_ordenada(L1).



% 12

% ordenacion(L1,L2), que signifique L2 es la lista de enteros L1 ordenada  de menor a mayor
% utilitzant permutacion i esta_ordenada, en una sola linia

ordenacion(L1,L2) :- permutacion(L1,L2), esta_ordenada(L2).

% 13

% (n!)n < (n+1)!
% factorial, molt elevat. ha de generar totes les permutacions (n!)

% 14

% ordenacion(L1,L2), insercion(X,L1,L2), L2 es la lista obtenida al insertar X en su sitio en la lista de enteros L1 que esta ordenada de menor a mayor

insercion(X,L1,L2):- pert_con_resto(X,L2,L1), esta_ordenada(L2).

% la solucio es molt diferent, pero crec que funciona. La solucio era:
% insercion(X,[],[X]). 
% insercion(X,[Y|L],[X,Y|L]) :- X=<Y. 
% insercion(X,[Y|L],[Y|L1]) :- X>Y, insercion(X,L,L1). 

ordenacion_por_insercion([],[]).
ordenacion_por_insercion([X|L],L1):- ordenacion_por_insercion(L,L2), insercion(X,L2,L1).

% 15

% La insercio te complexitat lineal (en el pitjor cas, recorres tota la linia)
% Ordenacio per insercio te complexitat quadratica, perque insereix l ultim element a la llista buida,
% el penultim en una llista d un element, i aixi successivament:
% 1 + 2 + ... + n-1 = n(n-1)/2

% 16 ordenacion_por_fusion: si la lista tiene longitud mayor que 1, con contact divide la lista en dos mitades
% ordena cada una de ellas (llamada recursiva) y despues fusiona las dos partes ordenadas en una sola. nlogn

% split: divide en dos mitades
split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]):- split(R,Ra,Rb).

merge(L,[],L) :- !.
merge([],L,L).
merge([X|L1], [Y|L2],[X|L3]) :- X=<Y, !, merge(L1,[Y|L2],L3). % regla adequada, segons solucio
merge([X|L1], [Y|L2],[Y|L3]) :- merge([X|L1],L2,L3).

merge_sort([],[]) :- !.
merge_sort([X],[X]) :- !.
merge_sort(L,L3) :- split(L,L1,L2), merge_sort(L1,L11), merge_sort(L2,L22), merge(L11,L22,L3).

% 17

% diccionario(A,N) tal que, donat un alfabet A de simbols i un natural R, escrigui totes les paraules de
% N simbols, per ordre alfabetic (l ordre alfabetic es segons l alfabet A donat)
% Per exemple : diccionario([ga,chu,le],2) escriura
% gaga gachu gale chuga chuchu chule lega lechu lele

% nmembers(A,N,L) que utilitza pert per obtenir llista L de N simbols, escriu els simbols de L tots enganxats
% i provoca backtracking

diccionario(A,N):- nperts(A,N,S), escribir(S), fail.

nperts(_,0,[]):- !.
nperts(L,N,[X|S]):- pert(X,L), N1 is N-1, nperts(L,N1,S).

escribir([]):-write(''),nl,!.
escribir([X|L]):-write(X),escribir(L).

% 18

% palindromos(L) % una lista de letras L, escribe todas las permutaciones de sus elementos que
% sean palindromos. capicues, vaja

palindromos(L):-permutacion(L,P),es_palindromo(P), write(P), nl, fail.
palindromos(_).

es_palindromo([]).
es_palindromo([_]):- !. % regla adequada, segons solucions
es_palindromo([X|L]):- concat(L1,[X],L), es_palindromo(L1).

% per evitar repetits: setof

palindroms(L):- setof(P,(permutation(L,P),es_palindromo(P)),S),write(S).
