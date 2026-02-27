# Polarização de macrófagos em contextos PAMPS e DAMPS

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

## Resultados Esperados
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