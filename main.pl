%Declaracoes iniciaias

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

%Definicoes iniciais
:- op( 900,xfy,'::' ).
:- use_module(library(lists)).

:- include('cidades.pl').
:- include('arcos.pl').

%--------------- Predicados --------------------------------

%Extensao do meta-predicado nao
nao( Questao ) :- Questao, !, fail.
nao( Questao ).

%Calcula o comprimento de uma lista
comprimento(S,N) :- length(S,N).

%Predicado que da um print no terminal
imprime([]).
imprime([X|T]) :- write(X), nl, imprime(T).

%verifica se um elemento pertence a uma  lista
temElem([],_).
temElem(L,[H|T]):- member(H,L).
temElem(L,[H|T]):- temElem(L,T);memberchk(H,L).  

%Obtem um arco apartir de um id
getArco(Origem,Destino,Dist) :- arco(Origem,Destino,Dist).

%verifica se uma lista esta vazia
estaVazia(L,V) :- comprimento(L,V),nao(V>0).

%---------------- Não Informada -----------------------------%

%----------------- Query 1 ---------------------------------%

%arco(IDCidade1,IDCidade2,Distancia)
%cidade(ID,Cidade,Lat,Long,Distrito,Ligações,Monumentos,Capital)

%getCaminho(1, 209).


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


%----------------- Query 2 ---------------------------------%

%getCaracteristica(3,71,'Sim').

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

%Obtencao das caracteristicas de uma cidade apartir do id
getCar(Origem,Proximo,Car,Dist) :-
	getArco(Origem,Proximo,Dist),
	cidade(Proximo,_,_,_,_,_,_,Car,_).


%----------------- Query 3 ---------------------------------%

%excluiCaracteristica(1,3,['Palacio da Bolsa','Templo de Diana','Portugal dos Pequeninos'])

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

%Obtencao dos Monumentos de uma cidade apartir do id
getNaoVai(Origem,Proximo,Monumento,Dist) :-
	getArco(Origem,Proximo,Dist),
	cidade(Proximo,_,_,_,_,_,Monumento,_,_).

%----------------- Query 4 ---------------------------------%

%menosLigacoes(182,94).

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

%Calculo do numero ligacoes
nrLig(Id,(Id,L)) :-
	findall(X,getArco(Id,_,_),Z),
	comprimento(Z,L).

%Obtencao de um destino e calculo de ligacoes desse destino
cidadeVizinha(Origem,Proximo,Dist,Menor) :-
	getArco(Origem,Proximo,Dist),
	nrLig(Proximo,Menor).

%----------------- Query 5 ---------------------------------%

%caminhoCurto(94,102).

caminhoCurto(Origem,Destino) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),C),
	tamanhoListaLig(C,D),
	minimoListas(D,E),
	filtroListas(D,E,U),
	imprime(U).

%Calculo do menores de uma lista de listas
minimoListas([((A,_),_)],A).
minimoListas([((A,_),_)|L],B):- minimoListas(L,B), B =< A.
minimoListas([((A,_),_)|L],A):- minimoListas(L,B), A < B.

%Elemina as listas que nao tenham valor igual ao calculado
filtroListas([],_,[]).
filtroListas([((X,_),_)|Y],Min,R) :-
    Min \== X ,
    filtroListas(Y,Min,R).
filtroListas([((X,A),B)|Y],Min,K) :-
    Min == X,
    filtroListas(Y,Min,R),
    append([((X,A),B)],R,K).

%Calculo do numero de cidades de um percurso
tamanhoListaLig([],[]).
tamanhoListaLig([(H,T)],[((X,H),T)]) :- comprimento(H,X).
tamanhoListaLig([(H,T)|Z],[((X,H),T)|Y]) :- comprimento(H,X),tamanhoListaLig(Z,Y).



%----------------- Query 6 ---------------------------------%

%maisRapido(103,119).

maisRapido(Origem,Destino) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),C),
	mininoDist(C,Dist),
	filtroDist(C,Dist,L),
	imprime(L).

%Procura qual os percursos com distancia minima
mininoDist([(_,H)],H) :- !,true.
mininoDist([(_,H)|T], M) :- 
	mininoDist(T, M), M =< H.
mininoDist([(_,H)|T], H):-
    mininoDist(T, M), H <  M.

%Elemina os percursos que nao tenham a distancia minima
filtroDist([],_,[]).
filtroDist([(_,D)|T],Min,R) :-
    Min \== D,
    filtroDist(T,Min,R).
filtroDist([(X,D)|T],Min,K) :-
    Min == D,
    filtroDist(T,Min,R),
    append([(X,D)],R,K).

%----------------- Query 7 ---------------------------------%

%getAdmin(22,32).

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

%----------------- Query 8 ---------------------------------%

%caminhosEntre(26,40,[22,3,32])

caminhosEntre(Origem,Destino,Intermedios) :-
	findall((P,Dist),caminho(Origem,Destino,Dist,P),L),
	check(L,Intermedios,R).

%Filtra os Percursos que cumprem com o itenerario
check([],_,[]).
check([(X,_)|T],L,R) :- check(T,L,R), \+temTodos(L,X).                              
check([(X,D)|T],L,R) :- check(T,L,K), temTodos(L,X), append([(X,D)],K,R),imprime(R).  

%verifica se uma lista tem elementos de outra
temTodos([],_).
temTodos([H|T],L) :- member(H,L),temTodos(T,L).


%----------------- Query Extra ---------------------------------%

%maisMonumentos(3,32,3).

maisMonumentos(Origem,Destino,NumeroMon) :-
	findall(((P,N),Dist),maisMon(Origem,Destino,P,Dist,NumeroMon,N),L),
	imprime(L).

maisMon(Origem,Destino,[Origem|Percurso],Dist,NumeroMon,N):-
	maisMonAux(Origem,Destino,Percurso,Dist,NumeroMon,N,[]).

maisMonAux(Destino,Destino,[],0,_,_,_).
maisMonAux(Origem,Destino,[Proximo|Percurso],Dist,NumeroMon,N,Visitados) :-
	Origem \= Destino,
	getNumMon(Origem,Proximo,N,Dist1),
	NumeroMon =< N,
	\+member(Proximo,Visitados),
	maisMonAux(Proximo,Destino,Percurso,Dist2,NumeroMon,N1,[Origem|Visitados]),
	Dist is Dist1 + Dist2.

%Calcula o numero de Monumentos de uma cidade
getNumMon(Origem,Proximo,N,Dist) :- 
	getArco(Origem,Proximo,Dist),
	cidade(Proximo,_,_,_,_,_,Monumento,_,'admin'),
	comprimento(Monumento,N).
	

%---------------- Informada -----------------------------%

%----------------- Query 1/6 ---------------------------------%

usaAestrela(Origem,Destino,Percurso/Custo) :-
	distance(Origem,Destino,X),
	aEstrela([[(Origem,Destino)]/0/X], PercursoInv/Custo/_),
	reverseL(PercursoInv,Percurso).

aEstrela(Percursos,Percurso) :-
	temMelhor(Percursos,Percurso),
	Percurso = [(Cidade,Destino)|_]/_/_,
	Cidade == Destino.
aEstrela(Percursos,Solucao) :-
	temMelhor(Percursos,MelhorPercurso),
	escolhe(MelhorPercurso,Percursos,OutrosC),
	aEstrelaExtendida(MelhorPercurso,PercursosExt),
	append(OutrosC,PercursosExt,NovosC),
	aEstrela(NovosC,Solucao).

temMelhor([Percurso],Percurso) :- !.
temMelhor([Percurso1/Custo1/Estimativa1,_/Custo2/Estimativa2|Percursos],MelhorPercurso) :-
	(Custo1 + Estimativa1) =< (Custo2 + Estimativa2), !,
	temMelhor([Percurso1/Custo1/Estimativa1|Percursos],MelhorPercurso).
temMelhor([_|Percursos],MelhorPercurso) :-
	temMelhor(Percursos,MelhorPercurso).

aEstrelaExtendida(Percurso,PercursosExt) :-
	findall(NovosC, arcoAdj(Percurso,NovosC), PercursosExt).

arcoAdj([(Cidade,Destino)|Percurso]/Custo/_, [(Proximo,Destino),(Cidade,Destino)|Percurso]/NCusto/Estimativa) :-
	getArco(Cidade,Proximo,ProximoCusto),
	\+member(Proximo,Percurso),
	
	distance(Proximo,Destino,Estimativa),
	NCusto is Custo + ProximoCusto.

% Tira o elemen D de uma lista
escolhe(D, [D|Ds], Ds).
escolhe(D, [D|Ds], [D|Es]) :- escolhe(D,Ds,Es).

% reverte uma lista
reverseL(Ds,Es) :- reverseL(Ds, [], Es).
reverseL([],Ds,Ds).
reverseL([D|Ds],Es,Fs) :- reverseL(Ds, [D|Es], Fs).

getCoord(Id,Lat,Long) :- cidade(Id,_,Lat,Long,_,_,_,_,_).

distance(Origem,Destino,C) :-
	getCoord(Origem,LatO,LongO),
	getCoord(Destino,LatD,LongD),
	C is sqrt((LatD-LatO)^2 + (LongD-LongO)^2).









