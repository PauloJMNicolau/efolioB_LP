**************
* Instalação *
**************

O programa foi desenvolvido em linguagem Prolog com a versão swi-prolog 8.2.4 e
testado no sistema operativo Fedora 34 (linux)

O modo de instalação utilizado para o sistema operativo anteriormente referido é: 

$  dnf install pl

**************
* Compilação *
**************

Para compilar o programa basta escrever na linha de comandos, no diretorio do ficheiro, um dos seguintes comandos:


$   swipl -o plataformaVendas -c plataforma.pl           //Compilador - Cria Ficheiro Executável com base no código-fonte
$   swipl -s plataforma.pl                               //Interpretador - Executa ficheiro de código-fonte diretamente

**************
*  Execução  *
**************

Para executar o programa na versão compilada basta seguir os seguintes passos:

$   ./plataformaVendas

Após a execução do comando anterior o programa irá executar com os dados carregados e mantem o interpretador aberto para 
receber comandos.

Se executar o comando do Interpretador o programa será carregado no interpretador de Prolog, preparando 
os factos com os dados que leu dos 4 ficheiros na pasta "dados", mantendo no final, o interpretador aberto
para receber comandos do utilizador.

Estão definidos os seguintes predicados:

    main/0                                  #Carrega os dados dos ficheiros no interpretador
    ler_ficheiro/2                          #Ler Ficheiro recebido como argumento
    ler_linhas/2                            #Ler as linhas do ficheiro
    gravar_dados/0                          #Gravar dados e Encerrar o programa
    escrever_ficheiro_cliente/0             #Executar a gravação do ficheiro de clientes
    escrever_ficheiro_artigo/0              #Executar a gravação do ficheiro de artigos
    escrever_ficheiro_inventario/0          #Executar a gravação do ficheiro de inventário
    escrever_ficheiro_vendas/0              #Executar a gravação do ficheiro de vendas
    processar_lista_cliente/2               #Gravar no ficheiro de cliente
    processar_lista_artigo_venda/2          #Gravar no ficheiro de artigos ou vendas
    processar_lista_inventario/2            #Gravar no ficheiro de inventário
    criar_estrutura_cliente/1               #Criar os predicados de cliente
    criar_estrutura_artigo/1                #Criar os predicados de artigo
    criar_estrutura_inventario/1            #Criar os predicados de inventário
    criar_estrutura_vendas/1                #Criar os predicados de vendas
    listar_cliente/0                        #Apresenta o nome dos clientes
    listar_cliente/1                        #Devolve uma lista com o nome dos clientes
    listar_cliente_bom/0                    #Apresenta o nome dos clientes com bom risco de crédito
    listar_cliente_bom/1                    #Devolve uma lista com o nome dos clientes com bom risco de crédito
    total_cliente_cidade/1                  #Apresenta o total de clientes na cidade recebida como parâmetro
    total_cliente_cidade/2                  #Devolve a quantidade de clientes na cidade recebida como parâmetro 
    listar_cliente_vendas/0                 #Apresentar uma lista com os nomes dos clientes que compraram produtos
    listar_cliente_vendas/1                 #Devolve a lista com o nome dos clientes que compraram produtos
    imprimir_lista/1                        #Imprimir a Lista recebida como parâmetro
    imprimir_lista_inventario/1             #Imprimir a lista de inventários recebido como parâmetro
    inventario_relatorio/0                  #Apresenta a lista com os dados do inventário
    inventario_relatorio/1                  #Devolve a lista dos dados do inventário
    inventario_atualiza_artigo/2            #Atualiza o valor da quantidade do artigo no inventário
    artigo_verificar_abaixo_min_alerta/1    #Mostra se o artigo se encontra abaixo do limite mínimo 
    venda_validar_artigo_cliente/3          #Valida se são cumpridos os requisitos de venda de produtos
    venda_artigo_cliente/0                  #Realizar Venda ao Cliente - Solicita dados de operação
    inventario_quantidade_stock/2           #Mostra o stock do inventário.

Para terminar o programa basta executar o predicado gravar_dados/0 que irá guardar os dados nos ficheiros 
correspondentes e fechar o interpretador de Prolog.

É necessário possuir o diretório "dados" (que possui os ficheiros lidos pelo programa) no mesmo diretorio do arquivo de
código-fonte ("plataforma.pl") na execução em Interpretador, ou do executável ("plataformaVendas") na versão Compilador 
para que o programa execute sem erros. Os ficheiros de dados necessitam estar no formato e com o mesmo nome dos ficheiros
incluidos como ficheiros de teste.
