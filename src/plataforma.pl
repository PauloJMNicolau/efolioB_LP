%Declarar pedicados como DinÂmicos
:- dynamic cliente/3.
:- dynamic artigo/3.
:- dynamic inventario/2.
:- dynamic vendas/3.

%Iniciar o Programa no main
:- initialization(main).

%%%%%%%%%%%%%%%%%%%%%%%%%
%   Produzir os factos  %
%%%%%%%%%%%%%%%%%%%%%%%%%

%Ler Ficheiro de Factos
ler_ficheiro(Ficheiro, Dados) :-
    setup_call_cleanup(
        open(Ficheiro, read, In),                               %Abrir o Ficheiro
        ler_linhas(In, Dados),                                  %Ler as Linhas
        close(In)).                                             %Fechar o ficheiro

%Ler as Linhas do Ficheiro e Converter numa Lista de Linhas
ler_linhas(In, Linhas) :-
    read_string(In, _, Linha),                                  %Ler Linhas todas como uma unica string
    split_string(Linha, "\n", "", Linhas).                      %Separar Linhas por caracter de mudança de linha 

%Gravar dados no ficheiro de clientes
escrever_ficheiro_cliente:-
    findall([X,Y,Z], cliente(X,Y,Z), Dados),                    %Procurar todos os factos e colocar numa lista
    open('dados/factosCliente.txt', write, Out),                %Abrir ficheiro para escrita
    processar_lista_cliente(Out, Dados),                        %Escrever dados no ficheiro
    close(Out).                                                 %Fechar o ficheiro

%Processar os dados do cliente a escrever no ficheiro
processar_lista_cliente(Output, Dados):-
    [H|T] = Dados,                                              %Separa a cabeça da lista do resto da lista
    [X,Y,Z] = H,                                                %Obtem os dados na cabeça
    format(Output,'~s;~s;~s', [X,Y,Z]),                         %Formata a string e escreve no ficheiro
    T \= [],                                                    %Verifica se a cauda da lista não está vazia
    write(Output, "\n"),                                        %Escreve uma mudança de linha no ficheiro
    processar_lista_cliente(Output, T),!;                       %Executa Recursivamente com o resto da lista
    !.                                                          %Termina a execução por ter chegado ao fim da lista

%Gravar dados no ficheiro de artigos
escrever_ficheiro_artigo:-
    findall([X,Y,Z], artigo(X,Y,Z), Dados),                     %Procurar todos os factos e colocar numa lista
    open('dados/factosArtigos.txt', write, Out),                %Abrir ficheiro para escrita
    processar_lista_artigo_venda(Out, Dados),                   %Escrever dados no ficheiro
    close(Out).                                                 %Fechar o ficheiro

%Processar os dados dos artigos e das vendas a escrever no ficheiro
processar_lista_artigo_venda(Output, Dados):-
    [H|T] = Dados,                                              %Separa a cabeça da lista do resto da lista
    [X,Y,Z] = H,                                                %Obtem os dados na cabeça
    format(Output,'~s;~s;~d', [X,Y,Z]),                         %Formata a string e escreve no ficheiro
    T \= [],                                                    %Verifica se a cauda da lista não está vazia
    write(Output, "\n"),                                        %Escreve uma mudança de linha no ficheiro
    processar_lista_artigo_venda(Output, T),!;                  %Executa Recursivamente com o resto da lista
    !.                                                          %Termina a execução por ter chegado ao fim da lista

%Gravar dados no ficheiro de inventario
escrever_ficheiro_inventario:-
    inventario_relatorio(Dados),                                %Procurar todos os factos e colocar numa lista
    open('dados/factosInventario.txt', write, Out),             %Abrir ficheiro para escrita
    processar_lista_inventario(Out, Dados),                     %Escrever dados no ficheiro
    close(Out).                                                 %Fechar o ficheiro

%Processar os dados do cliente a escrever no ficheiro
processar_lista_inventario(Output, Dados):-
    [H|T] = Dados,                                              %Separa a cabeça da lista do resto da lista
    [X,Y] = H,                                                  %Obtem os dados na cabeça
    format(Output,'~s;~d;', [X,Y]),                             %Formata a string e escreve no ficheiro
    T \= [],                                                    %Verifica se a cauda da lista não está vazia
    write(Output, "\n"),                                        %Escreve uma mudança de linha no ficheiro
    processar_lista_inventario(Output, T),!;                    %Executa Recursivamente com o resto da lista
    !.                                                          %Termina a execução por ter chegado ao fim da lista

%Gravar dados no ficheiro de clientes
escrever_ficheiro_vendas:-
    findall([X,Y,Z], vendas(X,Y,Z), Dados),                     %Procurar todos os factos e colocar numa lista
    open('dados/factosVendas.txt', write, Out),                 %Abrir ficheiro para escrita
    processar_lista_artigo_venda(Out, Dados),                   %Escrever dados no ficheiro
    close(Out).                                                 %Fechar o ficheiro

%Guardar os dados no ficheiro de texto
gravar_dados :-
    escrever_ficheiro_artigo,
    escrever_ficheiro_cliente,
    escrever_ficheiro_inventario,
    escrever_ficheiro_vendas,
    halt.                                                       %Termina o Programa

%Cria as estruturas dos Factos de Clientes
criar_estrutura_cliente([]).                                    %Não faz nada se receber uma lista vazia
criar_estrutura_cliente(Dados):-                    
    [H|T] = Dados,                                              %Separa os dados da Cabeça da lista, do resto da lista
    split_string(H, ";", "", Valores),                          %Separa por ; os dados da cabeça criando uma lista de strings
    [X,Y,Z] = Valores,                                          %Obter cada valor da lista de strings
    assertz(cliente(X,Y,Z)),                                    %Cria um novo facto - cliente
    criar_estrutura_cliente(T).                                 %Executa Recursivamente com o resto da lista

%Cria as estruturas dos Factos de Artigos
criar_estrutura_artigo([]).                                     %Não faz nada se receber uma lista vazia
criar_estrutura_artigo(Dados):-
    [H|T] = Dados,                                              %Separa os dados da Cabeça da lista, do resto da lista
    split_string(H, ";", "", Valores),                          %Separa por ; os dados da cabeça criando uma lista de strings
    [X,Y,Z] = Valores,                                          %Obter cada valor da lista de strings
    atom_number(Z,V),                                           %Converte para valor numérico o valor introduzido
    assertz(artigo(X,Y,V)),                                     %Cria um novo facto - artigo
    criar_estrutura_artigo(T).                                  %Executa Recursivamente com o resto da lista

%Cria as estruturas dos Factos de Inventários
criar_estrutura_inventario([]).                                 %Não faz nada se receber uma lista vazia
criar_estrutura_inventario(Dados):-
    [H|T] = Dados,                                              %Separa os dados da Cabeça da lista, do resto da lista
    split_string(H, ";", "", Valores),                          %Separa por ; os dados da cabeça criando uma lista de strings
    [X,Y] = Valores,                                            %Obter cada valor da lista de strings
    atom_number(Y,V),                                           %Converte para valor numérico o valor introduzido
    assertz(inventario(X,V)),                                   %Cria um novo facto - inventario
    criar_estrutura_inventario(T).                              %Executa Recursivamente com o resto da lista

%Cria as estruturas dos Factos de Vendas
criar_estrutura_vendas([]).                                     %Não faz nada se receber uma lista vazia
criar_estrutura_vendas(Dados):-
    [H|T] = Dados,                                              %Separa os dados da Cabeça da lista, do resto da lista
    split_string(H, ";", "", Valores),                          %Separa por ; os dados da cabeça criando uma lista de strings
    [X,Y,Z] = Valores,                                          %Obter cada valor da lista de strings
    atom_number(Z,V),                                           %Converte para valor numérico o valor introduzido
    assertz(vendas(X,Y,V)),                                     %Cria um novo facto - vendas
    criar_estrutura_vendas(T).                                  %Executa Recursivamente com o resto da lista

%Executar o Programa
main :-
    %Elimina todos os factos registados em memória, permitindo corrigir um problema causado durante a compilação que lê os ficheiros em 
    %tempo de compilação, e depois em tempo de execução, os ficheiros são lidos novamente, causando assim duplicados dos factos.
    %Este código permite assim, que o código seja executado tanto por compilador como por interpretador sem que existam erros.
    retractall(cliente(_,_,_)),                                 %Elimina todos os factos de cliente registados em memória
    retractall(artigo(_,_,_)),                                  %Elimina todos os factos de artigo registados em memória
    retractall(inventario(_,_)),                                %Elimina todos os factos de inventario registados em memória
    retractall(vendas(_,_,_)),                                  %Elimina todos os factos de vendas registados em memória
    %Carrega os dados em memória, criando os factos
    ler_ficheiro('dados/factosCliente.txt',Clientes),           %Obtem os dados de clientes em lista
    ler_ficheiro('dados/factosArtigos.txt',Artigos),            %Obtem os dados de artigo em lista
    ler_ficheiro('dados/factosInventario.txt',Inventario),      %Obtem os dados de inventario em lista
    ler_ficheiro('dados/factosVendas.txt',Vendas),              %Obtem os dados de vendas em lista
    criar_estrutura_cliente(Clientes),                          %Cria os factos de cliente
    criar_estrutura_artigo(Artigos),                            %Cria os factos de artigos
    criar_estrutura_inventario(Inventario),                     %Cria os factos de inventario
    criar_estrutura_vendas(Vendas).                             %Cria os factos de vendas



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Regras               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Devolve uma lista com o nome dos clientes todos.
listar_cliente(Lista) :-
    findall(X, cliente(X,_,_), Lista).                          %Procurar todos os factos de cliente e colocar numa lista que é retornada

%Mostra uma lista com o nome dos clientes todos.
listar_cliente :-
    listar_cliente(Lista),                                      %Obter a lista de clientes
    write("Os clientes são: "), nl,                             %Escrever no ecrã os dados obtidos
    imprimir_lista(Lista).

%Devolve uma lista com o nome de todos os clientes que possuam um bom risco de credito
listar_cliente_bom(Lista) :-
    findall(X, cliente(X,_,"aaa"), Lista).                      %Procurar todos os factos de cliente com bom risco de crédito e colocar numa lista que é retornada

%Mostra uma lista com o nome de todos os clientes que possuam um bom risco de credito
listar_cliente_bom :-
    listar_cliente_bom(Lista),                                  %Obter a lista de clientes com bom risco de crédito
    write("Os clientes com bom risco de crédito são: "), nl,    %Escrever no ecrã os dados obtidos
    imprimir_lista(Lista).

%Devolve o total de clientes numa cidade
total_cliente_cidade(Cidade, Total) :-
    findall(_, cliente(_,Cidade,_), Lista),                     %Procurar todos os factos de cliente na cidade indicada e colocar numa lista
    length(Lista, Total).                                       %Conta os elementos da lista e retorna o total.
    
%Mostra a quantidade de clientes numa Cidade
total_cliente_cidade(Cidade) :-
    total_cliente_cidade(Cidade,Total),                         %Obter a quantidade de clientes
    write("Existem "),                                          %Escrever no ecrã os dados obtidos
    write(Total),
    write(" clientes na cidade de "),
    write(Cidade).    

%Devolver a lista de clientes a que foram vendidos produtos
listar_cliente_vendas(Lista) :-
    findall(X, vendas(X,_, _), Lista).                          %Procurar todos os factos de vendas e colocar numa lista que é retornada

%Mostra a lista de clientes a que foram vendidos produtos
listar_cliente_vendas :-
    listar_cliente_vendas(Lista),                                   %Obter a lista de vendas
    write("Os clientes que compraram algum produto foram: "), nl,   %Escrever no ecrã os dados obtidos
    imprimir_lista(Lista).

%Imprimir a Lista no terminal
imprimir_lista([]):- !. 
imprimir_lista(Lista) :-
    [H|T] = Lista,                                              %Separa os dados da Cabeça da lista, do resto da lista
    write(H), nl,                                               %Escreve no ecrã os dados na cabeça da lista
    imprimir_lista(T).                                          %Executa Recursivamente com o resto da lista

%Imprimir a Lista no terminal
imprimir_lista_inventario([]):- !.                              %Não faz nada se receber uma lista vazia
imprimir_lista_inventario(Lista) :-
    [H|T] = Lista,                                              %Separa os dados da Cabeça da lista, do resto da lista
    [X,Y] = H,                                                  %Obter cada valor da lista na cabeça da lista recebida
    write("Produto: "),                                         %Escreve no ecrã os dados na cabeça da lista
    write(X),
    write("\tQuantidade: "),
    write(Y), nl,
    imprimir_lista_inventario(T).                               %Executa Recursivamente com o resto da lista

%Devolve Lista de Inventário
inventario_relatorio(Lista):-
    findall([X,Y], inventario(X,Y), Lista).                     %Procurar todos os factos de inventario e colocar numa lista que é retornada

%Mostrar a Lista de inventário
inventario_relatorio :-
    inventario_relatorio(Lista),                                %Obter uma lista com os dados do inventário
    write("A lista de produtos é: "), nl,                       %Escreve no ecrã os dados obtidos
    imprimir_lista_inventario(Lista).

%Atualiza Artigo no inventário
inventario_atualiza_artigo(Artigo,Quantidade):-
    call(inventario(Artigo,_)), !,                              %Verifica se existe artigo
    retract(inventario(Artigo,_)),                              %Elimina facto existente do artigo
    assertz(inventario(Artigo,Quantidade)).                     %Cria novo facto do artigo, com a nova quantidade
    
%Verificar Stock de artigo em Alerta
artigo_verificar_abaixo_min_alerta(Artigo):-
    artigo(Artigo,_,X),                                         %Obter a quantidade minima do artigo
    inventario(Artigo,Y),                                       %Obter a quantidade em stock do artigo
    X > Y,                                                      %Verificar se quantidade minima é superior à quantidade de stock
    write("Produto com stock abaixo do limite minimo"), !.      %Escreve mensagem a indicar valor de stock inferior 

%Verificar Stock de artigo em Alerta
artigo_verificar_abaixo_min_alerta(Artigo):-
    artigo(Artigo,_,X),                                         %Obter a quantidade minima do artigo
    inventario(Artigo,Y),                                       %Obter a quantidade em stock do artigo
    X =< Y,                                                     %Verificar se quantidade minima é inferior ou igual à quantidade de stock
    write("Produto com stock acima do limite minimo").          %Escreve mensagem a indicar valor de stock superior 

%Validar venda ao Cliente
venda_validar_artigo_cliente(Cliente, Artigo, Quantidade):-
    inventario_quantidade_stock(Artigo,X),                              %Obter quantidade de artigo emstock
    Quantidade =< X,                                                    %Verifica se quantidade indicada é inferior ou igual à quatidade em stock
    cliente(Cliente,_, "aaa"), true;                                    %Verifica se cliente possui um bom risco de crédito e retorna true caso possua.
    write("Não são cumpridos os requisitos para venda de produtos"),    %Escreve mensagem de erro e retorna falha.
    fail.

%Realizar Venda de artigo ao cliente na quantidade indicada pelo utilizador
venda_artigo_cliente :-
    seeing(Input),                                              %Obter a stream de input atual
    write("Qual o cliente: "),                                  %Escrever no ecrã a mensagem
    read_string(Input, ".", "\n", _, Cliente),                  %Ler uma string da stream de input
    write("Qual o artigo: "),                                   %Escrever no ecrã a mensagem
    read_string(Input, ".", "\n",_ ,Nome),                      %Ler uma string da stream de input
    write("Qual a quantidade: "),                               %Escrever no ecrã a mensagem
    read(Quantidade),                                           %Ler a quantidade introduzida pelo utilizador
    artigo(X,Nome,_),                                           %Obter a referencia do artigo introduzido pelo utilizador
    venda_validar_artigo_cliente(Cliente,X,Quantidade)->        %Valida possibilidade de venda com os dados introduzidos pelo utilizado
    inventario(X,Stock),                                        %Obter quantidade em stock do artigo
    Q is Stock - Quantidade,                                    %Subtrai o valor da quantidade introduzida pelo utilizador à quantidade em stock
    inventario_atualiza_artigo(X,Q),                            %Atualiza valor de quantidade em stock
    artigo_verificar_abaixo_min_alerta(X),                      %Apresenta mensagem sobre o limite minimo do artigo
    assertz(vendas(Cliente,X,Quantidade)); fail.                %cria novo facto de venda em caso de validação falhar retorna falha.

%Devolver Quantidade ou adicionar artigo no inventário
inventario_quantidade_stock(Artigo, Quantidade):-
    inventario(Artigo,Quantidade), ! ;                          %Obter a quantidade do artigo no inventário
    (inventario(Artigo,_),                                      %Verifica se existe um facto  do artigo introduzido
        inventario_atualiza_artigo(Artigo,Quantidade),!;        %Caso exista, atualiza valor da quantidade em stock
        assertz(inventario(Artigo,Quantidade)),!                %Caso não exista regista novo facto no inventario
    ).