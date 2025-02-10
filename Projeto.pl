% Projeto de Logica para Programacao em Prolog
% Madalena Cardoso Mota

:- use_module(library(clpfd)).
:- set_prolog_flag(answer_write_options,[max_depth(0)]). 
:- ["puzzlesAcampar.pl"]. 

/*-----------------------------------------------------------*/
/*----------------------Consultas----------------------------*/
/*-----------------------------------------------------------*/

% -----------------------------------------------------------
% vizinhanca(+(L, C), -Vizinhanca)
% Vizinhanca eh uma lista ordenada das posicoes
% imediatamente acima, imediatamente abaixo, 
% imediatamente a esquerda ou imediatamente a 
% direita da coordenada (L, C).
% -----------------------------------------------------------
vizinhanca((L, C), Vizinhanca):-
    Cmais1 is C+1,
    Cmenos1 is C-1,
    Lmais1 is L+1,
    Lmenos1 is L-1,
    Vizinhanca = [(Lmenos1, C), (L, Cmenos1), (L, Cmais1), (Lmais1, C)].

% -----------------------------------------------------------
% vizinhanca(+(L, C), -VizinhancaAlargada)
% VizinhancaAlargada e uma lista ordenada das posicoes
% imediatamente acima, imediatamente abaixo, 
% imediatamente a esquerda, imediatamente a 
% direita e nas diagonais da coordenada (L, C).
% -----------------------------------------------------------
vizinhancaAlargada((L, C), VizinhancaAlargada):-
    Cmais1 is C+1,
    Cmenos1 is C-1,
    Lmais1 is L+1,
    Lmenos1 is L-1,
    VizinhancaAlargada = [(Lmenos1, Cmenos1), (Lmenos1, C), (Lmenos1, Cmais1),
    (L, Cmenos1), (L, Cmais1), (Lmais1, Cmenos1), (Lmais1, C), (Lmais1, Cmais1)].

% -----------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas)
% TodasCelulas eh uma lista ordenada de todas
% as coordenadas de um Tabuleiro de dimensao N.
% -----------------------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas):-
    length(Tabuleiro, N),
    findall((X, Y), (between(1, N, X), between(1, N, Y)), TodasCelulas).

% -----------------------------------------------------------
% todasCelulas(+Tabuleiro, -TodasCelulas, +Objecto)
% TodasCelulas eh uma lista ordenada de todas as coordenadas
% do Tabuleiro que contem um objecto do tipo Objecto.
% -----------------------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
    var(Objecto),
    todasCelulas(Tabuleiro, Todas),
    findall((X, Y), 
    (
        member((X, Y), Todas),
        nth1(X, Tabuleiro, Linha), 
        nth1(Y, Linha, Elem),
        var(Elem)
    ), TodasCelulas).
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
    todasCelulas(Tabuleiro, Todas),
    findall((X, Y), 
    (
        member((X, Y), Todas),
        nth1(X, Tabuleiro, Linha), 
        nth1(Y, Linha, Elem),
        Elem == Objecto
    ), TodasCelulas).

% -----------------------------------------------------------
% calculaObjectosLista(+Lista, -Contagem, +Objecto)
% Auxiliar: usada no calculaObjectosTabuleiro
% Contagem eh o numero de ocorrencias de Objecto
% na lista Lista.
% -----------------------------------------------------------
calculaObjectosLista(Lista, Contagem, Objecto):-
    var(Objecto),
    findall(X, (member(X, Lista), var(X)), NumObjetos),
    length(NumObjetos, Contagem).
calculaObjectosLista(Lista, Contagem, Objecto):-
    findall(X, (member(X, Lista), X == Objecto), NumObjetos),
    length(NumObjetos, Contagem), !.

% -----------------------------------------------------------
% calculaObjectosTabuleiro(+T, -CLinhas, -CColunas, +Objecto)
% ContagemLinhas e ContagemColunas sao, respectivamente,
% listas com o numero de objectos do tipo Objecto por linha e 
% por coluna.
% -----------------------------------------------------------
% Base de recursao
calculaObjectosTabuleiro(T, CLinhas, CColunas, Objecto):-
    transpose(T, TTrans),
    calculaObjectosTabuleiro(T, TTrans, CLinhas, [], CColunas, [], Objecto).
% Caso terminal
calculaObjectosTabuleiro([], [], CLinhas, CLinhas, CColunas, CColunas, _):- !.
% Caso recursivo
calculaObjectosTabuleiro([CT|RT], [CTrans|RTrans], CLinhas, CLinhasAux, 
                        CColunas, CColunasAux, Objecto):-
    calculaObjectosLista(CT, Contador, Objecto),
    append(CLinhasAux, [Contador], ListaLinhas),
    calculaObjectosLista(CTrans, ContadorTrans, Objecto),
    append(CColunasAux, [ContadorTrans], ListaColunas),
    calculaObjectosTabuleiro(RT, RTrans, CLinhas, ListaLinhas, CColunas, 
    ListaColunas, Objecto).

% -----------------------------------------------------------
% CelulaVazia(+Tabuleiro, -(L, C))
% Eh verdade se (L, C) sao coordenadas que pertencem ao Tabuleiro 
% e tem relva ou nao tem nada.
% -----------------------------------------------------------
celulaVazia(Tabuleiro, (L, C)):-
    % Ver se a coordenada existe no tabuleiro
    todasCelulas(Tabuleiro, Todas), 
    member((L, C), Todas),
    % Obter o objeto da coordenada e verificar se e variavel ou r
    nth1(L, Tabuleiro, Linha), 
    nth1(C, Linha, Elem),
    (Elem == r; var(Elem)).

/*-----------------------------------------------------------*/
/*-------------Insercao de tendas e relva--------------------*/
/*-----------------------------------------------------------*/

% -----------------------------------------------------------
% insereObjectoCelula(+Tabuleiro, +TendaOuRelva, +(L, C))
% (L, C) sao coordenadas do Tabuleiro em que se pretende colocar
% uma relva ou uma tenda e TendaOuRelva eh o tipo de objecto
% que queremos colocar.
% -----------------------------------------------------------
% Verificar se a coordenada pertence ao Tabuleiro
insereObjectoCelula(Tabuleiro ,_, (L,C)) :-
    length(Tabuleiro,N),
    (L > N; C > N; L < 1; C < 1),!.
% Verificar se ja existe um objecto na coordenada
insereObjectoCelula(Tabuleiro, _, (L, C)):-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Elem),
    nonvar(Elem).
% Colocar TendaOuRelva na coordenada se esta existir e nao
% tiver um objecto
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)):-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Elem),
    var(Elem),
    Elem = TendaOuRelva.

% -----------------------------------------------------------
% insereObjectoEntrePosicoes(+Tabuleiro, +TendaOuRelva, +(L, C1), +(L, C2))
% Permite colocar objectos do tipo TendaOuRelva entre as coordenadas
% (L, C1) e (L, C2) do Tabuleiro.
% -----------------------------------------------------------
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    findall((L, Y), between(C1, C2, Y), Pos),
    maplist(insereObjectoCelula(Tabuleiro, TendaOuRelva), Pos).

/*-----------------------------------------------------------*/
/*-----------------------Estrategias-------------------------*/
/*-----------------------------------------------------------*/

% -----------------------------------------------------------
% insereObjectoLinha(+Tabuleiro, +N, +Objecto, +Linha)
% Auxiliar: usada no relva e no aproveita
% Permite inserir objectos do tipo Objecto numa linha do 
% Tabuleiro.
% -----------------------------------------------------------
insereObjectoLinha(Tabuleiro, N, Objecto, Linha):-
    insereObjectoEntrePosicoes(Tabuleiro, Objecto, (Linha, 1), (Linha, N)).

% -----------------------------------------------------------
% relva(+(Tabuleiro, Linhas, Colunas))
% Permite inserir relva em todas as coordenadas das linhas e colunas
% do Tabuleiro que ja tem o numero de tendas suposto.
% -----------------------------------------------------------
relva((Tabuleiro, Linhas, Colunas)):-
    % Obter o tamanho do tabuleiro
    length(Tabuleiro, N),
    % Obter numero de tendas em cada coluna e linha do tabuleiro
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, t),
    % Encontrar as linhas do tabuleiro que tem o numero pretendido
    % de tendas
    findall(
        Index, 
        (nth1(Index, Linhas, Elem), nth1(Index, CLinhas, Elem)), 
        IndexesLinhas
    ),
    % Colocar relva nas linhas
    maplist(insereObjectoLinha(Tabuleiro, N, r), IndexesLinhas),
    % Repetir o processo com o Tabuleiro transposto
    transpose(Tabuleiro, TTrans),
    findall(
        Index, 
        (nth1(Index, Colunas, Elem), nth1(Index, CColunas, Elem)), 
        IndexesColunas
    ),
    % Colocar relva nas colunas
    maplist(insereObjectoLinha(TTrans, N, r), IndexesColunas),
    transpose(TTrans, Tabuleiro).

% -----------------------------------------------------------
% inacessiveis(+Tabuleiro)
% Coloca relva em todas as coordenadas do Tabuleiro que estao 
% inacessiveis, isto eh, posicoes que nao estao na vizinhanca 
% de uma arvore.
% -----------------------------------------------------------
inacessiveis(Tabuleiro):-
    % Obter todas as celulas do tabuleiro
    todasCelulas(Tabuleiro, TodasCelulas),
    % Obter todas as arvores do tabuleiro
    todasCelulas(Tabuleiro, TodasArvores, a),
    % Obter todas as vizinhancas das arvores do tabuleiro
    maplist(vizinhanca, TodasArvores, Vizinhancas),
    flatten(Vizinhancas, V),
    % Encontrar todas as celulas que estao em TodasCelulas mas nao em Vizinhancas
    findall(
        (X, Y), 
        (member((X, Y), TodasCelulas), not(member((X, Y), V))),
        CelulasRelva
    ),
    % Colocar relva em todas as celulas de CelulasRelva
    maplist(insereObjectoCelula(Tabuleiro, r), CelulasRelva).

% -----------------------------------------------------------
% aproveita(+(Tabuleiro, Linhas, Colunas))
% Permite inserir tendas nas linhas e colunas do Tabuleiro
% em que faltava colocar N tendas e possuem exatamente N
% posicoes livres.
% -----------------------------------------------------------
aproveita((Tabuleiro, Linhas, Colunas)):-
    length(Tabuleiro, N),
    % Obter o numero de tendas e celulas vazias em cada 
    % coluna e linha do tabuleiro
    calculaObjectosTabuleiro(Tabuleiro, LinhasVazias, ColunasVazias, _),
    calculaObjectosTabuleiro(Tabuleiro, LinhasTendas, ColunasTendas, t), !,
    % Encontrar os indices das listas do numero de tendas que tenham o
    % mesmo valor 
    findall(
        Index,
        (nth1(Index, LinhasVazias, Vazias), 
        nth1(Index, LinhasTendas, Tendas),
        nth1(Index, Linhas, NumSuposto),
        Restantes is NumSuposto-Tendas, Restantes \== 0, Vazias == Restantes
        ), 
    AproveitaLinhas), 
    % Colocar tendas nas linhas
    maplist(insereObjectoLinha(Tabuleiro, N, t), AproveitaLinhas),
    % Repetir o processo para as colunas
    findall(
        Index,
        (nth1(Index, ColunasVazias, Vazias), 
        nth1(Index, ColunasTendas, Tendas),
        nth1(Index, Colunas, NumSuposto),
        Restantes is NumSuposto-Tendas, Restantes \== 0, Vazias == Restantes
        ), 
    AproveitaColunas),
    % Colocar tendas nas colunas
    transpose(Tabuleiro, TTrans),
    maplist(insereObjectoLinha(TTrans, N, t), AproveitaColunas),
    transpose(TTrans, Tabuleiro).

% -----------------------------------------------------------
% limpaVizinhancas(+(Tabuleiro, _, _))
% Coloca relva em todas as coordenadas das vizinhancas alargadas 
% das tendas do Tabuleiro.
% -----------------------------------------------------------
limpaVizinhancas((Tabuleiro, _, _)):-
    % Obter todas as celulas e tendas do tabuleiro
    todasCelulas(Tabuleiro, TodasCelulas, t),
    todasCelulas(Tabuleiro, Todas),
    % Obter as vizinhancas alargadas de todas as tendas
    maplist(vizinhancaAlargada, TodasCelulas, VizinhancaAlargada),
    flatten(VizinhancaAlargada, Vizinhancas),
    % Encontrar as celulas que pertencem ao Tabuleiro e e estao
    % nas vizinhacas alargadas das tendas
    findall(
        (X, Y),
        (member((X, Y), Vizinhancas), member((X, Y), Todas)),
        TodasVizinhancas
    ),
    % Colocar relva em todas as coordenadas das vizinhancas alargadas
    maplist(insereObjectoCelula(Tabuleiro, r), TodasVizinhancas).

% -----------------------------------------------------------
% pertenceTabuleiro(+TodasCelulas, +Vizinhanca, -Todas)
% Auxiliar: usada no unicaHipotese
% Todas eh uma lista ordenada das celulas da Vizinhanca que 
% pertencem a TodasCelulas do tabuleiro. 
% -----------------------------------------------------------
pertenceTabuleiro(TodasCelulas, Vizinhanca, Todas):-
    findall(
        (X, Y), 
        (member((X, Y), Vizinhanca), member((X, Y), TodasCelulas)), 
        Todas
    ).

% -----------------------------------------------------------
% semTenda(+Tabuleiro, +Vizinhanca)
% Auxiliar: usada no unicaHipotese
% Verifica se a lista Vizinhanca nao tem nenhuma tenda
% -----------------------------------------------------------
semTenda(Tabuleiro, Vizinhanca):-
    findall(
        (X, Y), 
        (member((X, Y), Vizinhanca), 
        nth1(X, Tabuleiro, Linha), 
        nth1(Y, Linha, Elem), 
        Elem \== t),
        SemTenda
    ), length(Vizinhanca, N), length(SemTenda, N).

% -----------------------------------------------------------
% celulaSemNada(+Tabuleiro, +Vizinhanca, -SemNada)
% Auxiliar: usada no unicaHipotese
% SemNada eh uma lista ordenada de todas as celulas de Vizinhanca
% que nao tem nenhum objecto.
% -----------------------------------------------------------
celulaSemNada(Tabuleiro, Vizinhanca, SemNada):-
    findall(
        (X, Y),
        (member((X, Y), Vizinhanca),
        nth1(X, Tabuleiro, Linha), 
        nth1(Y, Linha, var)),
        SemNada
    ).

% -----------------------------------------------------------
% unicaHipotese(+(Tabuleiro, _, _))
% Colocar uma tenda nas vizinhancas de todas as arvores que tinham apenas
% uma posicao livre na sua vizinhanca que lhes permitia ficar ligadas a 
% uma tenda.
% -----------------------------------------------------------
unicaHipotese((Tabuleiro, _, _)):- 
    % Obter todas as celulas do tabuleiro
    todasCelulas(Tabuleiro, TodasCelulas),
    % Obter todas as arvores do tabuleiro
    todasCelulas(Tabuleiro, TodasArvores, a),
    % Obter todas as vizinhancas das arvores do tabuleiro
    maplist(vizinhanca, TodasArvores, VizinhancasArvores),    
    % Encontrar todas as celulas que pertencem ao tabuleiro
    maplist(pertenceTabuleiro(TodasCelulas),
    VizinhancasArvores, TodasVizinhancas),
    % Descobrir as arvores que nao tem tendas na sua vizinhanca
    findall(
        Vizinhanca, (
        member(Vizinhanca, TodasVizinhancas), 
        semTenda(Tabuleiro, Vizinhanca)
    ), SemTenda),
    % Descobrir as celulas vazias de cada vizinhanca
    maplist(celulaSemNada(Tabuleiro), SemTenda, SemNada),
    % Encontrar as vizinhacas com tamanho 1
    findall(V, (member(V, SemNada), length(V, N), N == 1), Viz),
    flatten(Viz, CelulasTenda),
    % Colocar relva em todas as celulas de CelulasTenda
    maplist(insereObjectoCelula(Tabuleiro, t), CelulasTenda).

/*-----------------------------------------------------------*/
/*---------------------Tentativa e Erro----------------------*/
/*-----------------------------------------------------------*/

% -----------------------------------------------------------
% removeElemento(+Elemento, +Coordenadas, -Res)
% Auxiliar: usada no valida
% Res eh uma lista igual a lista Coordenadas sem o elemento
% Elemento.
% -----------------------------------------------------------
removeElemento(_, [], []).
removeElemento(Elemento, [Coordenada|RestoCoordenadas], Res):-
    Coordenada == Elemento,
    removeElemento(Elemento, RestoCoordenadas, Res).
removeElemento(Elemento, [Coordenada|RestoCoordenadas], [Coordenada|Resto]):-
    removeElemento(Elemento, RestoCoordenadas, Resto).

% -----------------------------------------------------------
% restoTendas(+LTen, +Vizinhanca, -RestoTendas)
% Auxiliar: usada no valida
% Remove tendas da lista LTen se esta estiverem contidas na Vizinhanca.
% RestoTendas eh uma lista com os elementos de LTen sem as tendas
% contidas na Vizinhanca.
% -----------------------------------------------------------
restoTendas(_, [], _):- fail.
restoTendas(LTen, [C|_], RestoTendas):-
    member(C, LTen),
    removeElemento(C, LTen, RestoTendas).
restoTendas(LTen, [_|R], RestoTendas):-
    restoTendas(LTen, R, RestoTendas).

% -----------------------------------------------------------
% valida(+LArv, +LTen)
% Verifica se eh possivel estabelecer uma relacao em que existe 
% uma e uma unica tenda para cada arvore nas suas vizinhancas, sendo 
% LArv e LTen listas com todas as coordenadas em que existem, 
% respectivamente, arvores e tendas.
% -----------------------------------------------------------
% Se as listas tiverem tamanhos diferentes, o predicado falha
valida(LArv, LTen):-
    length(LArv, N1), length(LTen, N2), N1 \== N2, fail.
valida([], []):- !.
valida([Arvore|RestoArvores], LTen):-
    % Verificar que a Arvore nao esta na lista das tendas e obter 
    % a sua vizinhanca
    not(member(Arvore, LTen)),
    vizinhanca(Arvore, Vizinhanca),
    % Remover tendas que estejam na vizinhanca da Arvore
    restoTendas(LTen, Vizinhanca, RestoTendas),
    % Aplicar o predicado no resto das arvores e das tendas
    valida(RestoArvores, RestoTendas), !.

% -----------------------------------------------------------
% resolve(+(Tabuleiro,Linhas,Colunas))
% Resolve o puzzle.
% -----------------------------------------------------------
resolve((Tabuleiro,Linhas,Colunas)):-
    % Verificar se o Tabuleiro tem o numero de tendas suposto
    % em cada linha e coluna e se contem uma combinacao valida
    % de tendas
    relva((Tabuleiro,Linhas,Colunas)),
    calculaObjectosTabuleiro(Tabuleiro,Linhas,Colunas,t),
    todasCelulas(Tabuleiro, LArv,a),
    todasCelulas(Tabuleiro, LTen,t),
    valida(LArv, LTen), !.

resolve((Tabuleiro,Linhas,Colunas)):-
    % Obter as celulas vazias iniciais
    todasCelulas(Tabuleiro, VaziasInicio, _),
    % Aplicar os predicados
    relva((Tabuleiro,Linhas,Colunas)),
    inacessiveis(Tabuleiro),
    aproveita((Tabuleiro,Linhas,Colunas)),
    limpaVizinhancas((Tabuleiro,Linhas,Colunas)),
    unicaHipotese((Tabuleiro,Linhas,Colunas)),
    % Obter as celulas vazias finais
    todasCelulas(Tabuleiro, VaziasFinal, _),
    % Se os tamanhos das listas de celulas vazias final e inicial
    % forem iguais, nao foram feitas alteracoes ao Tabuleiro
    % e recorremos ao predicado tentativaErro
    length(VaziasInicio, N),
    (length(VaziasFinal, N) -> tentativaErro(Tabuleiro);true),
    resolve((Tabuleiro,Linhas,Colunas)),!.

% -----------------------------------------------------------
% tentativaErro(+Tabuleiro)
% Auxiliar: usada no resolve
% Coloca uma tenda numa posicao ao acaso e, se nao for possivel 
% resolver o puzzle resultante, faz backtracking e coloca uma tenda
% numa nova posicao.
% -----------------------------------------------------------
tentativaErro(Tabuleiro):-
    todasCelulas(Tabuleiro, Vazias, _),
    member((L, C), Vazias),
    insereObjectoCelula(Tabuleiro, t, (L, C)).