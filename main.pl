% Declaracoes iniciaias

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

% Definicoes iniciais
:- op( 900,xfy,'::' ).
:- use_module(library(lists)).

:- include('cidades.pl').
:- include('arcos.pl').
% -------------------------------------------------------------

% Link uteis : https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_44.html
%              https://sicstus.sics.se/sicstus/docs/3.9.1/html/sicstus/Arithmetic.html

% ------------- Solucoes --------------------------------------
% Termo, Predicado, Lista -> {V,F}
solucoes(T,Q,S) :- findall(T,Q,S).

% --------------- Predicados --------------------------------

% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :- Questao, !, fail.
nao( Questao ).

% Calcula o comprimento de uma lista
comprimento(S,N) :- length(S,N).

% Predicado que da um print no terminal
imprime([]).
imprime([X|T]) :- write(X), nl, imprime(T).

% Predicado com a finalidade de adicionar elementos a uma lista
ad_lista(X, [], [X]).
ad_lista(X, [Y|T], [X,Y|T]) :- X @< Y, !. % um termo X precede um termo Y
ad_lista(X, [Y|T1], [Y|T2]) :- ad_lista(X, T1, T2).

% Elimina o ultimo elemento de uma lista
remove_last([], []) :- !, fail.
remove_last([_], []) :- !.
remove_last([X | T], [X | T2]) :- remove_last(T, T2).


% Extensão do predicado 'removeRepetidos' que remove os elementos repetidos duma lista
removeRepetidos([], []).
removeRepetidos([H|T], R) :- contains(H, T) , removeRepetidos(T, R).
removeRepetidos([H|T], [H|R]) :- nao(contains(H, T)) , removeRepetidos(T, R).

% Imprime os N primeiros valores de uma lista 
imprime_N(0,_).
imprime_N(_,[]).
imprime_N(X,[H|T]) :- X>0, write(H), nl, imprime_N(X-1,T).

%verifica se um elemento pertence a uma  lista
temElem([],_).
temElem(L,[H|T]):- member(H,L).
temElem(L,[H|T]):- temElem(L,T);memberchk(H,L).  

getArco(Origem,Destino,Dist) :- arco(Origem,Destino,Dist).

estaVazia(L,V) :- comprimento(L,V),nao(V>0).

% ---------------- Não Informada -----------------------------%

% ----------------- Querie 1 ---------------------------------%

%arco(IDCidade1,IDCidade2,Distancia)
%cidade(ID,Cidade,Lat,Long,Distrito,Ligações,Monumentos,Capital)

% getCaminho(1, 209).


getCaminho(Origem,Destino) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),L),
	imprime(L).

caminho(Origem,Destino,Dist,[Origem|Percurso]) :-
	caminhoAux(Origem,Destino,Percurso,Dist,[]).

caminhoAux(Destino,Destino,[],0,_).
caminhoAux(Origem,Destino,[Proximo|Percurso],Dist,Visitados) :-
	Origem \= Destino,
	getArco(Origem,Proximo,Dist1),
	\+member(Proximo,Visitados),
	caminhoAux(Proximo,Destino,Percurso,Dist2,[Origem|Visitados]),
	Dist is Dist1 + Dist2.


% ----------------- Querie 2 ---------------------------------%

% getCaracteristica(3,71,'Sim').

getCaracteristica(Origem,Destino,Car) :-
	findall((P,Dist),caracteristica(Origem,Destino,P,Car,Dist),L),
	imprime(L).

caracteristica(Origem,Destino,[Origem|Percurso],Car,Dist) :-
	caracteristicaAux(Origem,Destino,Percurso,Car,Dist,[]).

caracteristicaAux(Destino,Destino,[],Car,0,_).
caracteristicaAux(Origem,Destino,[Proximo|Percurso],Car,Dist,Visitados) :-
	Origem \= Destino,
	getCar(Origem,Proximo,Car1,Dist1),
	\+member(Proximo,Visitados),
	Car1 == Car,
	caracteristicaAux(Proximo,Destino,Percurso,Car,Dist2,[Origem|Visitados]),
	Dist is Dist1 + Dist2.

getCar(Origem,Proximo,Car,Dist) :-
	getArco(Origem,Proximo,Dist),
	cidade(Proximo,_,_,_,_,_,_,Car,_).


% ----------------- Querie 3 ---------------------------------%

% excluiCaracteristica(1,3,['Palacio da Bolsa','Templo de Diana','Portugal dos Pequeninos'])

excluiCaracteristica(Origem,Destino,Monumentos) :-
	findall((P,Dist),excluiCaminho(Origem,Destino,P,Monumentos,Dist),L),
	imprime(L).

excluiCaminho(Origem,Destino,[Origem|Percurso],Monumentos,Dist) :-
	excluiCaminhoAux(Origem,Destino,Percurso,Monumentos,Dist,[]).

excluiCaminhoAux(Destino,Destino,[],_,0,_).
excluiCaminhoAux(Origem,Destino,[Proximo|Percurso],Monumentos,Dist,Visitados) :-
	Origem \= Destino,
	getNaoVai(Origem,Proximo,Monumento,Dist1),
	\+member(Proximo,Visitados),
	\+temElem(Monumentos,Monumento),
	excluiCaminhoAux(Proximo,Destino,Percurso,Monumentos,Dist2,[Origem|Visitados]),
	Dist is Dist1 + Dist2.

getNaoVai(Origem,Proximo,Monumento,Dist) :-
	getArco(Origem,Proximo,Dist),
	cidade(Proximo,_,_,_,_,_,Monumento,_,_).

% ----------------- Querie 4 ---------------------------------%

% menosLigacoes(182,94).

menosLigacoes(Origem,Destino) :-
	findall((Menor,P),menor(Origem,Destino,P,_,Menor),L),
	imprime(L).

menor(Origem,Destino,[Origem|Percurso],Dist,M) :-
	nrLig(Origem,C),
	menorAux(Origem,Destino,Percurso,Dist,Menor,[]),
	append([C],Menor,M).

menorAux(Destino,Destino,[],0,[],_).
menorAux(Origem,Destino, [Proximo|Percurso],Dist,Menor,Visitados) :-
	Origem \= Destino,
	cidadeVizinha(Origem,Proximo,Dist1,Menor1),
	\+member(Proximo,Visitados),
	menorAux(Proximo,Destino,Percurso,Dist2,Menor2,[Origem|Visitados]),
	Dist is Dist1 + Dist2,
	append([Menor1],Menor2,Menor).

nrLig(Id,(Id,L)) :-
	findall(X,getArco(Id,_,_),Z),
	comprimento(Z,L).

cidadeVizinha(Origem,Proximo,Dist,Menor) :-
	getArco(Origem,Proximo,Dist),
	nrLig(Proximo,Menor).

% ----------------- Querie 5 ---------------------------------%

% caminhoCurto(94,102).

caminhoCurto(Origem,Destino) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),C),
	tamanhoListaLig(C,D),
	minimoListas(D,E),
	filtroListas(D,E,U),
	imprime(U).

minimoListas([((A,_),_)],A).
minimoListas([((A,_),_)|L],B):- minimoListas(L,B), B =< A.
minimoListas([((A,_),_)|L],A):- minimoListas(L,B), A < B.

filtroListas([],_,[]).
filtroListas([((X,_),_)|Y],Min,R) :-
    Min \== X ,
    filtroListas(Y,Min,R).
filtroListas([((X,A),B)|Y],Min,K) :-
    Min == X,
    filtroListas(Y,Min,R),
    append([((X,A),B)],R,K).

tamanhoListaLig([],[]).
tamanhoListaLig([(H,T)],[((X,H),T)]) :- comprimento(H,X).
tamanhoListaLig([(H,T)|Z],[((X,H),T)|Y]) :- comprimento(H,X),tamanhoListaLig(Z,Y).



% ----------------- Querie 6 ---------------------------------%

% maisRapido(103,119).

maisRapido(Origem,Destino) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),C),
	mininoDist(C,Dist),
	filtroDist(C,Dist,L),
	imprime(L).

mininoDist([(_,H)],H) :- !,true.
mininoDist([(_,H)|T], M) :- 
	mininoDist(T, M), M =< H.
mininoDist([(_,H)|T], H):-
    mininoDist(T, M), H <  M.

filtroDist([],_,[]).
filtroDist([(_,D)|T],Min,R) :-
    Min \== D,
    filtroDist(T,Min,R).
filtroDist([(X,D)|T],Min,K) :-
    Min == D,
    filtroDist(T,Min,R),
    append([(X,D)],R,K).

% ----------------- Querie 7 ---------------------------------%

% getAdmin(22,32).

getAdmin(Origem,Destino) :-
	findall((P,Dist),municipio(Origem,Destino,P,Dist,'admin'),L),
	imprime(L). 

municipio(Origem,Destino,[Origem|Percurso],Dist,'admin') :-
	municipioAux(Origem,Destino,Percurso,Dist,'admin',[]).

municipioAux(Destino,Destino,[],0,_,_).
municipioAux(Origem,Destino,[Proximo|Percurso],Dist,'admin',Visitados) :-
	Origem \= Destino,
	getArco(Origem,Proximo,Dist1),
	cidade(Proximo,_,_,_,_,_,_,_,'admin'),
	\+member(Proximo,Visitados),
	municipioAux(Proximo,Destino,Percurso,Dist2,'admin',[Origem|Visitados]),
	Dist is (Dist1 + Dist2).

% ----------------- Querie 8 ---------------------------------%

% caminhosEntre(26,40,[22,3,32])

caminhosEntre(Origem,Destino,Intermedios) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),L),
	check(L,Intermedios,R).

check([],_,[]).
check([(X,_)|T],L,R) :- check(T,L,R), \+temTodos(L,X).                              
check([(X,D)|T],L,R) :- check(T,L,K), temTodos(L,X), append([(X,D)],K,R),imprime(R).  

%verifica se uma lista tem elementos de outra
temTodos([],_).
temTodos([H|T],L) :- member(H,L),temTodos(T,L).

% ---------------- Informada -----------------------------%

% ----------------- Querie 1/6 ---------------------------------%

getCoord(Id,Lat,Long) :- cidade(Id,_,Lat,Long,_,_,_,_,_).

distance(Origem,Destino,C) :-
	getCoord(Origem,LatO,LongO),
	getCoord(Destino,LatD,LongD),
	C is sqrt((LatD-LatO)^2 + (LongD-LongO)^2).


melhor([Percurso],Percurso) :- !.
melhor([Percurso1/Custo1/Estimativa1,_/Custo2/Estimativa2|Percursos],MelhorPercurso) :-
	Custo1 + Estimativa1 =< Custo2 + Estimativa2,
	!,
	melhor([Percurso1/Custo1/Estimativa1|Percursos], MelhorPercurso).
melhor([_|Percursos],MelhorPercurso) :-
	melhor(Percursos,MelhorPercurso).


usaAestrela(Origem,Destino,Percurso/Custo) :-
	distance(Origem,Destino,C),
	aEstrela([[(Origem,Destino)]/0/C],PercursoInv/Custo/_),
	reverse(PercursoInv,Percurso).

aEstrela(Percursos,Percurso) :-
	melhor(Percursos,Percurso),
	Percurso = [(Proximo,Destino)|_]/_/_,
	Proximo == Destino.
aEstrela(Percursos,SolPercurso) :-
	melhor(Percursos,MelhorPercurso),
	select(MelhorPercurso,Percursos,MaisPercursos),
	aEstrelaExtendida(MelhorPercurso,PercursosExt),
	append(MaisPercursos,PercursosExt,PercursosNovos),
	aEstrela(PercursosNovos,Percurso).

aEstrelaExtendida(Percursos,PercursosExt) :-
	findall(PercursosNovos, arcoAestrela(Percurso,PercursosNovos),PercursosExt).

arcoAestrela([(Proximo,Destino)|Percurso]/Custo/_,[(Proximo2,Destino),(Proximo,Destino)|Percurso]/Custo2/Estimativa) :-
	getArco(Proximo,Proximo2,Dist),
	\+member(Proximo2,Percurso),
	Custo2 is Custo + Dist,
	distance(Proximo2,Destino,Estimativa).












