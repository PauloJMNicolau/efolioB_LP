%Declarar pedicados como DinÂmicos
:- dynamic cliente/3.
:- dynamic artigo/3.
:- dynamic inventario/2.
:- dynamic vendas/3.

%%%%%%%%%%%%%%%%%%%%%%%%%
%   Produzir os factos  %
%%%%%%%%%%%%%%%%%%%%%%%%%

%Ler Ficheiro de Factos
ler_ficheiro(Ficheiro, Dados) :-
    setup_call_cleanup(
        open(Ficheiro, read, In),    %Abrir o Ficheiro
        ler_linhas(In, Dados),       %Ler as Linhas
        close(In) 
    ).

%Ler as Linhas do Ficheiro e Converter numa Lista de Linhas
ler_linhas(In, Linhas) :-
    read_string(In, _, Linha),               %Ler Linhas
    split_string(Linha, "\n", "", Linhas).  %Separar Linhas por caracter de mudança de linha 

%Gravar dados no ficheiro de clientes
escrever_ficheiro_cliente:-
    findall([X,Y,Z], cliente(X,Y,Z), Dados),
    open('dados/factosCliente.txt', write, Out),
    processar_lista_cliente(Out, Dados),
    close(Out).

%Processar os dados do cliente a escrever no ficheiro
processar_lista_cliente(Output, Dados):-
    [H|T] = Dados,
    [X,Y,Z] = H,
    format(Output,'~s;~s;~s', [X,Y,Z]),
    T \= [],
    write(Output, "\n"),
    processar_lista_cliente(Output, T),!;
    !.

%Gravar dados no ficheiro de artigos
escrever_ficheiro_artigo:-
    findall([X,Y,Z], artigo(X,Y,Z), Dados),
    open('dados/factosArtigos.txt', write, Out),
    processar_lista_artigo_venda(Out, Dados),
    close(Out).

%Processar os dados dos artigos e das vendas a escrever no ficheiro
processar_lista_artigo_venda(Output, Dados):-
    [H|T] = Dados,
    [X,Y,Z] = H,
    format(Output,'~s;~s;~d', [X,Y,Z]),
    T \= [],
    write(Output, "\n"),
    processar_lista_artigo_venda(Output, T),!;
    !.

%Gravar dados no ficheiro de inventario
escrever_ficheiro_inventario:-
    inventario_relatorio(Dados),
    open('dados/factosInventarios.txt', write, Out),
    processar_lista_inventario(Out, Dados),
    close(Out).

%Processar os dados do cliente a escrever no ficheiro
processar_lista_inventario(Output, Dados):-
    [H|T] = Dados,
    [X,Y] = H,
    format(Output,'~s;~d;', [X,Y]),
    T \= [],
    write(Output, "\n"),
    processar_lista_inventario(Output, T),!;
    !.

%Gravar dados no ficheiro de clientes
escrever_ficheiro_vendas:-
    findall([X,Y,Z], vendas(X,Y,Z), Dados),
    open('dados/factosVendas.txt', write, Out),
    processar_lista_artigo_venda(Out, Dados),
    close(Out).

%Guardar os dados no ficheiro de texto
gravar_dados :-
    escrever_ficheiro_artigo,
    escrever_ficheiro_cliente,
    escrever_ficheiro_inventario,
    escrever_ficheiro_vendas,
    halt.

%Cria as estruturas dos Factos de Clientes
criar_estrutura_cliente([]).
criar_estrutura_cliente(Dados):-
    [H|T] = Dados,
    split_string(H, ";", "", Valores),
    [X,Y,Z] = Valores,
    assertz(cliente(X,Y,Z)),
    criar_estrutura_cliente(T).

%Cria as estruturas dos Factos de Artigos
criar_estrutura_artigo([]).
criar_estrutura_artigo(Dados):-
    [H|T] = Dados,
    split_string(H, ";", "", Valores),
    [X,Y,Z] = Valores,
    atom_number(Z,V),
    assertz(artigo(X,Y,V)),
    criar_estrutura_artigo(T).

%Cria as estruturas dos Factos de Inventários
criar_estrutura_inventario([]).
criar_estrutura_inventario(Dados):-
    [H|T] = Dados,
    split_string(H, ";", "", Valores),
    [X,Y] = Valores,
    atom_number(Y,V),
    assertz(inventario(X,V)),
    criar_estrutura_inventario(T).

%Cria as estruturas dos Factos de Vendas
criar_estrutura_vendas([]).
criar_estrutura_vendas(Dados):-
    [H|T] = Dados,
    split_string(H, ";", "", Valores),
    [X,Y,Z] = Valores,
    atom_number(Z,V),
    assertz(vendas(X,Y,V)),
    criar_estrutura_vendas(T).

%Executar o Programa
main :-
    ler_ficheiro('dados/factosCliente.txt',Clientes),
    ler_ficheiro('dados/factosArtigos.txt',Artigos),
    ler_ficheiro('dados/factosInventario.txt',Inventario),
    ler_ficheiro('dados/factosVendas.txt',Vendas),
    criar_estrutura_cliente(Clientes),
    criar_estrutura_artigo(Artigos),
    criar_estrutura_inventario(Inventario),
    criar_estrutura_vendas(Vendas).

%Iniciar o Programa no main
:- initialization(main).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Regras               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Devolve uma lista com o nome dos clientes todos.
listar_cliente(Lista) :-
    findall(X, cliente(X,_,_), Lista).

%Mostra uma lista com o nome dos clientes todos.
listar_cliente :-
    listar_cliente(Lista),
    write("Os clientes são: "), nl,
    imprimir_lista(Lista).

%Devolve uma lista com o nome de todos os clientes que possuam um bom risco de credito
listar_cliente_bom(Lista) :-
    findall(X, cliente(X,_,"aaa"), Lista).

%Mostra uma lista com o nome de todos os clientes que possuam um bom risco de credito
listar_cliente_bom :-
    listar_cliente_bom(Lista),
    write("Os clientes com bom risco de crédito são: "), nl,
    imprimir_lista(Lista).

%Devolve o total de clientes numa cidade
total_cliente_cidade(Cidade, Total) :-
    findall(_, cliente(_,Cidade,_), Lista),
    length(Lista, Total).
    
%Mostra a quantidade de clientes numa Cidade
total_cliente_cidade(Cidade) :-
    total_cliente_cidade(Cidade,Total),
    write("Existem "),
    write(Total),
    write(" clientes na cidade de "),
    write(Cidade).    

%Devolver a lista de clientes a que foram vendidos produtos
listar_cliente_vendas(Lista) :-
    findall(X, vendas(X,_, _), Lista).

%Mostra a lista de clientes a que foram vendidos produtos
listar_cliente_vendas :-
    listar_cliente_vendas(Lista),
    write("Os clientes que compraram algum produto foram: "), nl,
    imprimir_lista(Lista).

%Imprimir a Lista no terminal
imprimir_lista([]):- !. 
imprimir_lista(Lista) :-
    [H|T] = Lista,
    write(H), nl,
    imprimir_lista(T).

%Imprimir a Lista no terminal
imprimir_lista_inventario([]):- !. 
imprimir_lista_inventario(Lista) :-
    [H|T] = Lista,
    [X,Y] = H,
    write("Produto: "),
    write(X),
    write("\tQuantidade: "),
    write(Y), nl,
    imprimir_lista_inventario(T).

%Devolve Lista de Inventário
inventario_relatorio(Lista):-
    findall([X,Y], inventario(X,Y), Lista).

%Mostrar a Lista de inventário
inventario_relatorio :-
    inventario_relatorio(Lista),
    write("A lista de produtos é: "), nl,
    imprimir_lista_inventario(Lista).

%Atualiza Artigo no inventário
inventario_atualiza_artigo(Artigo,Quantidade):-
    call(inventario(Artigo,_)), !,
    retract(inventario(Artigo,_)),
    assertz(inventario(Artigo,Quantidade)).
    
%Verificar Stock de artigo em Alerta
artigo_verificar_abaixo_min_alerta(Artigo):-
    artigo(Artigo,_,X),
    inventario(Artigo,Y),
    X > Y,
    write("Produto com stock abaixo do limite minimo"), !.

%Verificar Stock de artigo em Alerta
artigo_verificar_abaixo_min_alerta(Artigo):-
    artigo(Artigo,_,X),
    inventario(Artigo,Y),
    X =< Y,
    write("Produto com stock acima do limite minimo").

%Validar venda ao Cliente
venda_validar_artigo_cliente(Cliente, Artigo, Quantidade):-
    inventario(Artigo,X),
    Quantidade =< X,
    cliente(Cliente,_, "aaa"), true;
    write("Não são cumpridos os requisitos para venda de produtos"),
    fail.

%Realizar Venda de artigo ao cliente na quantidade indicada pelo utilizador
venda_artigo_cliente :-
    seeing(Input),
    write("Qual o cliente: "),
    read_string(Input, ".", "\n", _, Cliente),
    write("Qual o artigo: "),
    read_string(Input, ".", "\n",_ ,Nome),
    write("Qual a quantidade: "),
    read(Quantidade),
    artigo(X,Nome,_),
    venda_validar_artigo_cliente(Cliente,X,Quantidade)->
    inventario(X,Stock),
    Q is Stock - Quantidade,
    inventario_atualiza_artigo(X,Q),
    assertz(vendas(Cliente,X,Quantidade)); fail.

%Devolver Quantidade ou adicionar artigo no inventário
inventario_quantidade_stock(Artigo, Quantidade):-
    inventario(Artigo,Quantidade), ! ;
    (inventario(Artigo,_),
        inventario_atualiza_artigo(Artigo,Quantidade),!;
        assertz(inventario(Artigo,Quantidade)),!
    ).