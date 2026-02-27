# Macrophage Polarization in PAMP and DAMP Contexts

Below is the algorithm in R language that simulates the construction of a complex signaling
network followed by theoretical knockouts and the calculation of the RME (Relative Mean Error)
index proposed by Miranda (2016).

# Usage Tutorial

To execute the algorithm, we recommend using the RStudio IDE.

## Prerequisites and Initial Setup

Before starting, ensure you have R and RStudio installed on your machine.

You will need to install the following packages. If you don't have them yet, run this command in the RStudio console:

```R
install.packages(c("igraph", "Matrix")) 
```

## Data File Structure

The script requires a .csv file without a header (header=FALSE),
separated by semicolons (;), containing three columns in the following order:

+ Source Signal/Vertex

+ Receptor Vertex

+ Mediator (Edge Name)

## How to insert your file into the script
Copy the folder path where the .csv files are located and place it in the following line:

```R
B<-read.table('C:/Users/YourUser/Documents/R/file.csv', sep=";", header=FALSE)
```

# Expected Results

Upon finishing the execution of the entire script, you will have two
new files in your working folder (the same directory where RStudio is running the project):

+ CellsKO.csv: A report ranked in descending order containing the
importance (RME) of each vertex, as well as the system's connectivity status after its removal.

+ SignalKO.csv: A similar report, but quantifying the importance of each edge/signal in the multigraph.

**Tip**: Use the getwd() command in the R console to find out exactly in which folder these .csv files were saved!

# Working Principle

The main objective of the script is to quantify the importance of each component (vertices and edges) within a network (multigraph). It does this by simulating the failure or absence of each piece and measuring the impact this causes on the general system flow via the RME index.

It operates by following these main steps:

1. Network Construction: Imports data from your CSV file and builds the directed multigraph.

2. System Closure: If the network has "loose ends" (is not strongly connected),
the code creates an artificial node called Environment. This ensures that all flow 
entering the system also exits, creating a conservative network.

3. Standard Flow Calculation: Transforms the network connections into a probability transition matrix.
From this, it extracts the principal eigenvector to discover the "normal flow" or equilibrium state
of the intact network.

4. Knock-out Simulation: The code executes two major simulation cycles:

    + First, it "deletes" one vertex (node) at a time and redoes all flow and connectivity calculations.

    + Then, it repeats the process, but deleting one edge (signal) at a time.

5. Error Calculation (RME): For each removed piece, it compares the new network flow with the original standard flow. This difference generates the RME (Relative Mean Error). The higher the RME, the more vital that component is for network stability.

6. Results Export: Saves two reports (CellsKO.csv for vertices and SignalKO.csv for edges), ranking all components from most critical to least critical.


# Polarização de macrófagos em contextos PAMPS e DAMPS -- Versão em Português

Abaixo segue o algoritmo em linguagem R que simula a construção de uma rede complexa de sinalização seguida dos
knockouts teóricos e cálculo do índice RME ( Relative Mean Error) proposto por Miranda (2016).

# Tutorial de Utilização

Para execução do algoritmo, recomendamos utilizar a IDE RStudio.

## Pré-requisitos e Configuração Inicial

Antes de começar, certifique-se de ter o R e o RStudio instalados em sua máquina. 

Você precisará instalar os seguintes pacotes. Se ainda não os tem, execute este comando no console do RStudio:
```R
install.packages(c("igraph", "Matrix")) 
```

## Estrutura do Arquivo de Dados

O script exige um arquivo .csv sem cabeçalho (header=FALSE), separado por ponto e vírgula (;),
contendo três colunas na seguinte ordem:

+ Sinal/Vértice de Origem

+ Vértice Receptor

+ Mediador (Nome da Aresta)

## Como inserir seu arquivo no script

Copie o caminho da pasta aonde estão os  arquivos .csv e coloque na seguinte linha

```R
 B<-read.table('C:/Users/YourUser/Documents/R/file.csv', sep=";", header=FALSE)
```



# Resultados Esperados
Ao finalizar a execução do script inteiro, você terá dois novos arquivos na sua pasta de trabalho (o mesmo diretório onde o RStudio está rodando o projeto):

+ CellsKO.csv: Um relatório ranqueado decrescentemente contendo a importância (RME) de cada vértice, além do status de conectividade do sistema após sua remoção.

+ SignalKO.csv: Um relatório semelhante, mas quantificando a importância de cada aresta/sinal no multigrafo.

**Dica:** Use o comando getwd() no console do R para descobrir exatamente em qual pasta estes arquivos .csv foram salvos!

# Princípio de funcionamento

O objetivo principal do script é quantificar a importância de cada componente (vértices e arestas) dentro de uma rede (multigrafo). Ele faz isso simulando a falha ou ausência de cada peça e medindo o impacto que isso causa no fluxo geral do sistema por meio do índice RME.

Ele opera seguindo estes passos principais:

1. Construção da Rede: Importa os dados do seu arquivo CSV e monta o multigrafo direcionado.

2. Fechamento do Sistema: Se a rede possuir "pontas soltas" (não for fortemente conexa), o código cria um nó artificial chamado Environment (Ambiente). Isso garante que todo o fluxo que entra no sistema também saia, criando uma rede conservativa.

3. Cálculo do Fluxo Padrão: Transforma as conexões da rede em uma matriz de transição de probabilidades. A partir dela, extrai o autovetor principal para descobrir o "fluxo normal" ou estado de equilíbrio da rede intacta.

4. Simulação de Nocautes (Knock-outs): O código executa dois grandes ciclos de simulação:

Primeiro, ele "deleta" um vértice (nó) de cada vez e refaz todos os cálculos de fluxo e conectividade.

Depois, ele repete o processo, mas deletando uma aresta (sinal) de cada vez.

5. Cálculo do Erro (RME): Para cada peça removida, ele compara o novo fluxo da rede com o fluxo padrão original. Essa diferença gera o RME (Relative Mean Error). Quanto maior o RME, mais vital é aquele componente para a estabilidade da rede.

6. Exportação de Resultados: Salva dois relatórios (CellsKO.csv para os vértices e SignalKO.csv para as arestas), ranqueando todos os componentes do mais crítico ao menos crítico.
