   /*Joao Lopes | 90732*/
 /*IST               */
/*LP                */

:-consult(codigo_comum).
%-------------------------------aplica_R1_triplo------------------------------%
/*Se forem iguais, false()*/
aplica_R1_triplo([A, B, C], _):-
  A == B,
  B == C,
  !, false().

/*Se [A,B,C] contem dois elementos iguais, o outro tem de ser diferente*/
aplica_R1_triplo([A,B,_], N_Triplo):-
  A == 0,
  B == 0,
  N_Triplo = [0,0,1], !.
aplica_R1_triplo([A,B,_], N_Triplo):-
  A == 1,
  B == 1,
  N_Triplo = [1,1,0], !.

aplica_R1_triplo([A,_,B], N_Triplo):-
  A == 0,
  B == 0,
  N_Triplo = [0,1,0], !.
aplica_R1_triplo([A,_,B], N_Triplo):-
  A == 1,
  B == 1,
  N_Triplo = [1,0,1], !.

aplica_R1_triplo([_,A,B], N_Triplo):-
  A == 0,
  B == 0,
  N_Triplo = [1,0,0], !.
aplica_R1_triplo([_,A,B], N_Triplo):-
  A == 1,
  B == 1,
  N_Triplo = [0,1,1], !.

/*Em ultimo caso o triplo e sempre valido*/
aplica_R1_triplo(Triplo, Triplo).
%-------------------------------aplica_R1_triplo------------------------------%
/*****************************************************************************/
%------------------------------aplica_R1_fila_aux-----------------------------%
%replace/2 Copia as posicoes que verificam var() de uma lista para outra
replace([],[]).
replace([H1|T1], [H2|T2]):-
  \+(H1 == 1),
  \+(H1 == 0),
  \+(H2 == 0),
  \+(H2 == 1),
  H1 = H2, !,
  replace(T1,T2).
replace([_|T1], [_|T2]):-
  replace(T1,T2).

%Quando sobram dois elementos basta copiar visto que ja que
%nao se pode aplicar aplica_R1_triplo a dois elementos
recursivo_aplica_R1_fila_aux([A,B|H], [RT1, RT2]):-
  length(H, 0), !,
  RT1 = A, RT2 = B.
%aplica_R1_triplo tres a tres elementos
recursivo_aplica_R1_fila_aux([A,B,C|T], [I,J,K|RT]):-
  aplica_R1_triplo([A,B,C], [X,Y,Z]),
  I = X, J = Y, K = Z,
  recursivo_aplica_R1_fila_aux([B,C|T], [J,K|RT]).

aplica_R1_fila_aux(Fila, N_fila):-
  duplicate_term(Fila, Fake_fila),
  recursivo_aplica_R1_fila_aux(Fake_fila, N_fila),
  replace(N_fila, Fake_fila).

%------------------------------aplica_R1_fila_aux-----------------------------%
/*****************************************************************************/
%--------------------------------aplica_R1_fila-------------------------------%
%compara/2 e verdade caso as duas listas sejam iguais. Variaveis sao
%consideradas iguais e nao-variaveis devem ser iguais (==)
compara([],[]).
compara([L1|T1], [L2|T2]):-
  \+(L1 == 0),
  \+(L1 == 1),
  \+(L2 == 0),
  \+(L2 == 1), !,
  compara(T1,T2).
compara([L1|T1], [L2|T2]):-
  L1 == L2,
  compara(T1,T2).

%quando nao ha alteracoes de uma fila para a outra, acabou
recursivo_aplica_R1_fila(Fila, N_fila):- compara(Fila, N_fila),replace(N_fila ,Fila), !.
recursivo_aplica_R1_fila(Fila, N_fila):-
  aplica_R1_fila_aux(Fila, N_fila),
  T = Fila,
  recursivo_aplica_R1_fila(N_fila, T).

aplica_R1_fila(Fila, N_fila):-
  duplicate_term(Fila, Fake_fila),
  recursivo_aplica_R1_fila(Fake_fila, N_fila),
  replace(N_fila, Fila).
%--------------------------------aplica_R1_fila-------------------------------%
/*****************************************************************************/
%--------------------------------aplica_R2_fila-------------------------------%
%count_one/2 e count_zero/2 contam o numero de uns e zeros, respetivamente,
%numa lista
count_one([],0).
count_one([H|T],N) :- number(H), H == 1, count_one(T,N1), N is N1 + 1, !.
count_one([X|T],N) :- number(X), X \= 1, count_one(T,N).
count_one([X|T],N) :- \+(number(X)), count_one(T,N).
count_zero([],0).
count_zero([H|T],N) :- number(H), H == 0, count_zero(T,N1), N is N1 + 1, !.
count_zero([X|T],N) :- number(X), X \= 0, count_zero(T,N), !.
count_zero([X|T],N) :- \+(number(X)), count_zero(T,N).

%preenche todas as variaveis numa lista com N (0 ou 1)
preenche(L, N, Res):-
  fill(L, N),
  Res = L.
fill([], _).
fill([H|T], N):-
  \+(H == 0),
  \+(H == 1),
  H is N, !,
  fill(T, N).
fill([_|T], N):-
  fill(T,N).

aplica_R2_fila(Fila, N_fila):-
  duplicate_term(Fila, Fake_fila),
  length(Fila, X),
  S is X/2,
  count_zero(Fila, NZ),
  NZ == S,
  preenche(Fake_fila, 1, N_fila), !,
  replace(N_fila, Fila).

aplica_R2_fila(Fila, N_fila):-
  duplicate_term(Fila, Fake_fila),
  length(Fila, X),
  S is X/2,
  count_one(Fila, NO),
  NO == S,
  preenche(Fake_fila, 0, N_fila),
  replace(N_fila, Fila), !.

aplica_R2_fila(Fila, N_fila):-
  length(Fila, X),
  S is X/2,
  count_zero(Fila, NZ),
  count_one(Fila, NO),
  NZ < S,
  NO < S,
  Fila = N_fila, !.
%--------------------------------aplica_R2_fila-------------------------------%
/*****************************************************************************/
%------------------------------aplica_R1_R2_fila------------------------------%
aplica_R1_R2_fila(Fila, N_fila):-
  aplica_R1_fila(Fila, AUX),
  aplica_R2_fila(AUX, N_fila).
%------------------------------aplica_R1_R2_fila------------------------------%
/*****************************************************************************/
%------------------------------aplica_R1_R2_puzzle----------------------------%
%aplica_R1_R2_fila a todas as listas
linhas([], []).
linhas([H|T], [RH|RT]):-
  aplica_R1_R2_fila(H, RH),
  linhas(T, RT).

%aplica_R1_R2_fila a todas as linhas e a todas as linhas da transposta, ou seja,
%a todas as colunas
aplica_R1_R2_puzzle(Puz, N_Puz):-
  linhas(Puz, R),
  mat_transposta(R, T),
  linhas(T, C),
  mat_transposta(C, N_Puz).
%------------------------------aplica_R1_R2_puzzle-----------------------------%
/*****************************************************************************/
%-----------------------------------inicializa---------------------------------%
%compara_puz/2 e verdade caso dado dois puzzles eles sejam iguais
compara_puz([],[]).
compara_puz([H1|T1], [H2|T2]):-
  compara(H1, H2),
  compara_puz(T1, T2).

%quando nao ha alteracoes de uma recursao para a outra, acabou
inicializa(Puz, N_Puz):-compara_puz(Puz, N_Puz), !.
inicializa(Puz, N_Puz):-
  duplicate_term(Puz, Fake_Puz),
  aplica_R1_R2_puzzle(Fake_Puz, N_Puz),
  inicializa(N_Puz, Puz).
%-----------------------------------inicializa---------------------------------%
/*****************************************************************************/
%-----------------------------------verifica_R3--------------------------------%
%n_tem_vars e verdade caso numa lista todos os elementos verifiquem nonvar()
n_tem_vars([]).
n_tem_vars([H|T]):-
  nonvar(H),
  n_tem_vars(T).

%compara_com_todas/2 e verdade se dada uma lista e uma lista de listas a
%primeira for diferente das outras todas
compara_com_todas(_, []):-!.
compara_com_todas(E, [H|T]):-
  \+(compara(E, H)),
  compara_com_todas(E, T).

%faz o anterior para todas as linhas de um Puz
verificar([]).
verificar([H|T]):-
    \+(n_tem_vars(H)), !,
    verificar(T).
verificar([H|T]):-
  compara_com_todas(H, T),
  verificar(T).

%verifica se todas as linhas sao diferentes umas das outras e, depois, se as
%linhas da transposta (colunas) tambem cumprem a regra
verifica_R3(Puz):-
  duplicate_term(Puz, Fake_Puz),
  verificar(Puz),
  mat_transposta(Fake_Puz, T),
  verificar(T).
%-----------------------------------verifica_R3--------------------------------%
/*****************************************************************************/
%---------------------------------propaga_posicoes-----------------------------%
%linha/3 para obter uma linha de um Puz
linha(Lin, Mat, Fila):-
  mat_transposta(Mat, MatT),
  mat_elementos_coluna(MatT, Lin, Fila).
%coluna/3 para o analogo
coluna(Col, Mat, Fila):-
  mat_elementos_coluna(Mat, Col, Fila).

%elementos_diferentes/2 e verdade se E,F sao diferentes
elementos_diferentes(E, F):-
  nonvar(E),
  nonvar(F),
  E \== F, !.
elementos_diferentes(E,F):-
  nonvar(E),
  var(F), !.
elementos_diferentes(E,F):-
  nonvar(F),
  var(E), !.

%Dado duas listas este predicado da as coordenadas das posicoes que sao diferentes
posicoes_diferentes([], [], _, _).
posicoes_diferentes([H1|T1], [H2|T2], [H|T], P):-
  elementos_diferentes(H1, H2), !,
  H = P,
  NP is P + 1,
  posicoes_diferentes(T1,T2,T, NP).
posicoes_diferentes([_|T1], [_|T2], Pos, P):-
  NP is P + 1,
  posicoes_diferentes(T1,T2,Pos, NP), !.

create_coord_pair_X([], _, _).
create_coord_pair_X([H|_], _, _):- var(H),!.
create_coord_pair_X([H|T], Coord, [OH|OT]):-
  nonvar(H),
  OH = (Coord, H),
  create_coord_pair_X(T, Coord, OT),!.

create_coord_pair_Y([], _, _).
create_coord_pair_Y([H|_], _, _):- var(H),!.
create_coord_pair_Y([H|T], Coord, [OH|OT]):-
  nonvar(H),
  OH = (H, Coord),
  create_coord_pair_Y(T, Coord, OT),!.

%aplica_R1_R2_fila na linha e depois na coluna ---> Verificar as posicoes que
%mudaram e, antes de propagar outra posicao da lista de posicoes original,
%propagar essas posicoes alteradas
propaga_posicoes([], Puz, Res):- Res = Puz, !.
propaga_posicoes([A|_], Puz, Res):- var(A), Res = Puz, !.
propaga_posicoes([(X,Y)|T], Puz, N_Puz):-
  linha(X, Puz, L),
  aplica_R1_R2_fila(L, Lout),
  mat_muda_linha(Puz, X, Lout, N_Puz1),
  posicoes_diferentes(L, Lout, PosX, 1),
  create_coord_pair_X(PosX, X, PosXMudadas),
  coluna(Y, Puz, C),
  aplica_R1_R2_fila(C, Cout),
  mat_muda_coluna(N_Puz1, Y, Cout, N_Puz2),
  posicoes_diferentes(C, Cout, PosY, 1),
  create_coord_pair_Y(PosY, Y, PosYMudadas),
  append(PosXMudadas, PosYMudadas, PosMudadas),
  propaga_posicoes(PosMudadas, N_Puz2, N_Puz3),
  propaga_posicoes(T, N_Puz3, N_Puz), !.
%---------------------------------propaga_posicoes-----------------------------%
/******************************************************************************/
%-------------------------------------resolve----------------------------------%
%vazio_fila da a coordenada da primeira variavel (_) numa lista
vazio_fila([], (Y, Pos), Pos, Y):- !, false().
vazio_fila([H|T], V, Pos, Y):-
  nonvar(H), !,
  P is Pos + 1,
  vazio_fila(T, V, P, Y).
vazio_fila(_, (Y, Pos), Pos, Y):- !.
pos_vazia([], _, _):- !, false().
pos_vazia([H|_], Pos, Y):-
  vazio_fila(H, Pos, 1, Y), !.
pos_vazia([_|T], Pos, Y):-
  Y1 is Y+1,
  pos_vazia(T, Pos, Y1).

%aplica vazio_fila num puz tantas vezes ate descobrir as coordenadas de Variaveis
%livres (_)
pos_vazias(Puz, [P|_]):- \+(pos_vazia(Puz, P, 1)), !.
pos_vazias(Puz, [H|T]):-
  pos_vazia(Puz, H, 1),
  mat_muda_posicao(Puz, H, 0, N_Puz),
  pos_vazias(N_Puz, T).

%chama propaga para todas as posicoes com variaveis livres
propaga_vazios(Puz, N_Puz):- compara_puz(Puz, N_Puz), !.
propaga_vazios(Puz, N_Puz):-
  pos_vazias(Puz, P),
  propaga_posicoes(P, Puz, N_Puz),
  propaga_vazios(N_Puz, Puz), !.

resolve(Puz, Sol):-
  inicializa(Puz, N_Puz),
  verifica_R3(N_Puz),
  propaga_vazios(N_Puz, Sol).
%-------------------------------------resolve----------------------------------%
