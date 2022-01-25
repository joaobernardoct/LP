%Joao Bernardo 86443  --  LP 2016/17
%Solucionador de problemas de Sudoku

:- include('SUDOKU').

%---------------------------------------------------------------------
%Predicados para a propagacao de mudancas
%---------------------------------------------------------------------
tira_num_aux(Num,Puz,Pos,N_Puz) :-
	puzzle_ref(Puz,Pos,Cont),		%obter o conteudo da posicao Pos
	select(Num,Cont,NovoCont),		%retirar o numero Num a esse conteudo
	puzzle_muda_propaga(Puz,Pos,NovoCont,N_Puz).	%propagar os efeitos desta mudanca
tira_num_aux(_,Puz,_,Puz).

	
tira_num(Num,Puz,Posicoes,N_Puz) :-
	percorre_muda_Puz(Puz,tira_num_aux(Num),Posicoes,N_Puz).	%aplicar tira_num_aux a todas as posicoes de Posicoes

	
puzzle_muda_propaga(Puz,Pos,Cont,N_Puz) :-
	length(Cont,1),		%se for uma lista unitaria, vamos propagar a mudanca a todas as posicoes relacionadas
	puzzle_muda(Puz,Pos,Cont,Puz_Mud),
	nth0(0,Cont,Num),	%Num e o nr que vamos remover das posicoes relacionadas
	posicoes_relacionadas(Pos,Posicoes_Rel),			 
	tira_num(Num,Puz_Mud,Posicoes_Rel,N_Puz),
	!.
puzzle_muda_propaga(Puz,Pos,Cont,N_Puz) :-
	puzzle_muda(Puz,Pos,Cont,N_Puz).	%se a lista nao for unitaria nao propagamos a mudanca

	
%---------------------------------------------------------------------
%Predicados para a inicializacao de puzzles
%---------------------------------------------------------------------
possibilidades(Pos,Puz,Conteudo) :-
	puzzle_ref(Puz,Pos,Conteudo),
	length(Conteudo,1).		%se a posicao Pos tiver um conteudo unitario esse conteudo e a unica possibilidade
possibilidades(Pos,Puz,Poss) :-
	posicoes_relacionadas(Pos,Posicoes_Rel),
	conteudos_posicoes(Puz,Posicoes_Rel,Conteudos),		%lista dos conteudos das posicoes relacionadas
	numeros(Nums),		%lista de todos os numeros possiveis
	possibilidades_aux(Conteudos,Nums,Poss).

possibilidades_aux([],Nums,Nums).	%apos inspecionarmos todas as Posicoes_Rel a lista Nums vai conter as possibilidades
possibilidades_aux([P|R],Nums,Poss) :-
	length(P,1),		%se o conteudo da posicao P for unitario,
	nth0(0,P,Num),
	select(Num,Nums,NovoNums),		%retiramos a Nums esse conteudo
	possibilidades_aux(R,NovoNums,Poss).
possibilidades_aux([_|R],Nums,Poss) :-
	possibilidades_aux(R,Nums,Poss).


inicializa_aux(Puz,Pos,Puz) :-
	puzzle_ref(Puz,Pos,Conteudo),
	length(Conteudo,1).			%se o cont for uma lista unitaria, nada e alterado
inicializa_aux(Puz,Pos,N_Puz) :-
	possibilidades(Pos,Puz,Poss),
	puzzle_muda_propaga(Puz, Pos, Poss, N_Puz).	%colocar as possibilidades Poss para a posicao Pos no conteudo dessa posicao


inicializa(Puz,N_Puz) :-
	todas_posicoes(Todas_Posicoes),
	percorre_muda_Puz(Puz,inicializa_aux,Todas_Posicoes,N_Puz).	%aplicar inicializa_aux a todas as posicoes de Todas_Posicoes

	
%---------------------------------------------------------------------
%Predicados para a inspeccao de puzzles
%---------------------------------------------------------------------
so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num) :-
										 %Lista_aux
	so_aparece_uma_vez_aux(Puz,Num,Posicoes,[],Lista_Posicoes), %Funcao aux q vai construir Pos_Num, uma lista das posicoes em q ocorre Num
	length(Lista_Posicoes,1),			%Se o numero ocorrer uma vez devolvemos a sua posicao
	nth0(0,Lista_Posicoes,Pos_Num).

so_aparece_uma_vez_aux(_,_,[],Lista_aux,Lista_aux).
so_aparece_uma_vez_aux(Puz,Num,[P|R],Lista_aux,Lista_Posicoes) :-
	puzzle_ref(Puz, P, Cont),
	member(Num,Cont),	%se o numero Num estiver no conteudo Cont da posicao P	
	nth0(0,Lista,P,Lista_aux), 		%adicionamos P a nossa lista de posicoes onde ocorre o numero 							
	so_aparece_uma_vez_aux(Puz,Num,R,Lista,Lista_Posicoes),
	!.													
so_aparece_uma_vez_aux(Puz,Num,[_|R],Lista_aux,Lista_Posicoes) :-
	so_aparece_uma_vez_aux(Puz,Num,R,Lista_aux,Lista_Posicoes).	

	
inspecciona_num(Posicoes,Puz,Num,N_Puz) :- 
	so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num),	%se der false, o puzzle nao se altera. Se der Pos_Num:
	puzzle_ref(Puz,Pos_Num,Conteudo),
	\+ length(Conteudo,1),		%e se o conteudo dessa posicao nao for unitario,										
	puzzle_muda_propaga(Puz,Pos_Num,[Num],N_Puz).	%colocamos Num no conteudo e propagamos a mudanca
inspecciona_num(_,Puz,_,Puz).	
	
	
inspecciona_grupo(Puz,Gr,N_Puz) :-
	numeros(Nums),
	insp_grupo_aux(Puz,Gr,Nums,N_Puz).
	
insp_grupo_aux(Puz,_,[],Puz).
insp_grupo_aux(Puz,Gr,[P|R],N_Puz) :-	
	inspecciona_num(Gr,Puz,P,Puz_IN),
	insp_grupo_aux(Puz_IN,Gr,R,N_Puz).


inspecciona(Puz,N_Puz) :- 
	grupos(Gr),		%Gr vai ser uma lista de listas
	insp_aux(Puz,Gr,N_Puz).
	
insp_aux(Puz_IG,[],Puz_IG).
insp_aux(Puz,[P|R],N_Puz) :-
	inspecciona_grupo(Puz,P,Puz_IG),	%aplicamos a inspeciona_grupo para uma das listas de Gr
	insp_aux(Puz_IG,R,N_Puz).	%devolvemos as restantes listas
	
	
%---------------------------------------------------------------------
%Predicados para a verificacao de solucoes
%---------------------------------------------------------------------
grupo_correcto(Puz,Nums,Gr) :- 
	conteudos_posicoes(Puz,Gr,Conteudos),
	sort(Conteudos,Cont_Ordenados),	%ordena a lista que contem o conteudo das posicoes
	grupo_correcto_aux(Cont_Ordenados,[],Lista_Final),	%retira as listas de listas de Cont_Ordenados, ficando tudo numa so lista
	reverse(Lista_Final,Nums).	%verifica se a lista dos conteudos ordenados e igual a Nums.

grupo_correcto_aux([],Nova_Lista,Nova_Lista).								
grupo_correcto_aux([P|R],Lista,Lista_Final) :-
	nth0(0,P,Conteudo),	%retira o conteudo da 1a posicao da lista
	nth0(0,Nova_Lista,Conteudo,Lista),	%cria uma Lista que vai estar ordenada do maior para o menor elemento
	grupo_correcto_aux(R,Nova_Lista,Lista_Final).

	
solucao(Puz) :-
	numeros(Nums),
	grupos(Gr),
	solucao_aux(Puz,Nums,Gr).	%funcao aux que vai realizar grupo_correcto para cada lista da lista Gr

solucao_aux(_,_,[]).
solucao_aux(Puz,Nums,[P|R]) :-
	grupo_correcto(Puz,Nums,P),
	solucao_aux(Puz,Nums,R).

	
%---------------------------------------------------------------------
%Predicado Resolve
%---------------------------------------------------------------------
resolve(Puz,Sol) :- 
	inicializa(Puz,PuzInic),	%inicializacao do puzzle Puz
	inspecciona(PuzInic,PuzInsp),		%inspecao do puzzle PuzInic
	todas_posicoes(Todas_Posicoes),
	resolve_aux(PuzInsp,Todas_Posicoes,Sol).	%auxiliar que vai obter a solucao 

resolve_aux(Puz,[],Puz) :-		%caso de paragem que verifica se foi encontrada uma solucao
	solucao(Puz).	
resolve_aux(Puz,[P|R],Sol) :-
	puzzle_ref(Puz,P,Conteudo),
	\+ length(Conteudo,1),		%posicao P tem uma sequencia nao unitaria
	member(Num,Conteudo),		%atribuir a P um dos numeros dessa sequencia
	puzzle_muda_propaga(Puz,P,[Num],Res),	%propagar os seus efeitos
	resolve_aux(Res,R,Sol).	
resolve_aux(Puz,[P|R],Sol) :-
	puzzle_ref(Puz,P,Conteudo),
	length(Conteudo,1),		%caso a sequencia da posicao P seja unitaria,
	resolve_aux(Puz,R,Sol).		%passamos a' auxiliar as restantes posicoes