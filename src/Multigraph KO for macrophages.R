#Objetive: to automatically generate the RME of all vertices and edges of a 
#given multigraph.

#install the following packages: igraph, Matrix

# Load the usual packages

library(igraph, Matrix)

#Load external file input (three column matrix: signal, receiver, mediator)

B<-read.table('C:/Users/PC-Pedro/Documents/R/file(base).csv', sep=";", header=FALSE)

base<-as.matrix(B); base<-unname(base) #prep matrix and remove all headers

#Get an edge-list (matrix) from original matrix

edges<-base[,-c(3)] #remove third column

#create multigraph from edgelist

g<-graph_from_edgelist(edges, directed=T)

#Now we must add multiple attributes to edges that must represent the 'mediator'

arrows<-c(base[,3]) #vector object with the names of the directed edges

E(g)$name<-arrows #add name attribute to the edges

V(g)$color<-"lightblue"

# Vizualize the resultant multigraph
tkplot(g, vertex.label=V(g)$name, edge.label=E(g)$name)

# Adding the Environment to the graph: conservative network

if(is_connected(g, mode="strong")==FALSE){
  
  ge<-add_vertices(g,1, name="Environment"); 
  
  D<-length(degree(g)); F<-D+1;
  
  for(d in 1:D){
    
    if(degree(g, mode="out")[d]==0)
      
    {ge<-add_edges(ge, c(d,F)); E(ge)[length(E(ge))]$name<-"output"};
    
    if(degree(g, mode="in")[d]==0)
      
    {ge<-add_edges(ge, c(F,d)); E(ge)[length(E(ge))]$name<-"input"} 
    
  }
  
}

V(ge)[F]$color<-"red"

is.connected(ge, mode="strong")

tkplot(ge, edge.label=E(ge)$name) #verificação gráfica do grafo com environment

g<-ge #the protocol uses the g graph for calculations

# Generating the standart flux vector #

A <- as_adjacency_matrix(g) # transforma o grafo em matriz de adjacencia 0 E 1

D <- degree(g,mode=c("out"))# um vetor que tem a informação do grau de saida dos vertices

P<-degree(g, mode=c("in")) #vetor de graus de entrada

J <- length(V(g)) #cria um indice para varredura das colunas

I <- length(V(g))  #cria um indice para varredura das linhas

for(i in 1:I){v<-D[i];

for(j in 1:J){A[i,j]<-A[i,j]/v} # transforma a matriz de adjacencia em matriz de transição

}

A #this is a row-stochastic matrix, because sum(i,)=1.

mysub <- function(x) {sub(",",".",x)} #função para mudar separador decimal

mydata <- (apply(A, 2, mysub )) #aplicão de função de mudança de separador decimal

rownames(A)<-NULL  #retira o rotulo das linhas

colnames(A)<-NULL #retira o rotulo das colunas

A[is.nan(A)] = 0 #troca NAN por 0

# até aqui a matriz a é a matriz de transição dinamica do grafo g

B <- t(A) #this is a column-stochastic matrix, because sum(,j)=1.

e <- eigen(B)  # autovalores e autovetores

v <-e$vectors[,1] #autovetor de valor 1

av <- abs(v) #transforma os valores em absoluto

cv <- sum(av) #soma de todas as componentes de av

c <- 1/cv  #coeficiente de normalização

standart <- c*av  #autovetor normalizado sum(vc) = 1

standart<-as.vector.data.frame(standart) # transforma o vetor normalizado em matriz

agents<-V(g)$name #atribui novamente os nomes (nam era função do R)

agents<-as.vector.data.frame(agents) #transformamos os rótulos em matriz

flux<-cbind(agents, standart) #juntamos os fluxos (vc) com seus nomes (nam)

print(flux) #this is the standart flux of cell's activation

#this will be used as the criteria to measure the KOs

sum(standart) #must be 1 in order to conserve probability

# Knock-outs generalized/optimized for VERTICES

#Objetive: calculate RME (Miranda et al., 2015) e add a index for knocked-out multigraphs that 
#are not connected

#Step 1. Choose any change in the initial multigraph g, in this case,
#speceifically the list of vertices

J<-NULL; I<-length(V(g)) #creation of the index I for calculation in looping

Cells<-V(g) #list of the names of the components of the system

RME<-rep(0, length(V(g))) #Relative Mean Error - quantifies the importance of agents

Connectivity<-rep("not analysed", length(V(g))) #vector to receive the values: connected, not connected.

Numb.Comm.classes<-rep("not analysed", length(V(g))) #number of communicating classes

CellsKO<-data.frame(Cells, RME, Connectivity, Numb.Comm.classes)

print(CellsKO)

I<-length(V(g)) #creation of the index I for looping calculation

for(i in 1:I){
  
  gko<-delete_vertices(g, V(g)[i]); #creates a KO-multigraph - the number of KO-multigraphs is I
  
  if(is.connected(gko, mode=c("strong"))==TRUE){CellsKO[i,4]<-1;
  
  CellsKO[i,3]<-"Connected"
  
  A <- as.matrix(get.adjacency(gko)) # transforms the multigraph into an adjacenty matrix
  
  D <- degree(gko,mode=c("out")) # vector that contains the out-degree of vertices
  
  M <- length(V(gko)) #index to run over all columns
  
  N <- length(V(gko))  #cria um indice para varredura das linhas
  
  for(n in 1:N){v<-D[n];
  
  for(m in 1:M){A[n,m]<-A[n,m]/v} #here we obtain the transition matrix
  
  }
  
  A # the sum of rows must be 1 (sum(A[i,])) == 1
  
  mysub <- function(x) {sub(",",".",x)} #função para mudar separador decimal
  
  mydata <- (apply(A, 2, mysub )) #aplicão de função de mudança de separador decimal
  
  rownames(A)<-NULL  #retira o rotulo das linhas
  
  colnames(A)<-NULL #retira o rotulo das colunas
  
  A[is.nan(A)] = 0 #troca NAN por 0
  
  # até aqui a matriz a é a matriz de transição dinamica do grafo g
  
  B <- t(A) # transpomos a matriz A em B
  
  e <- eigen(B)  # autovalores e autovetores
  
  v <-e$vectors[,1] #autovetor de valor 1
  
  av <- abs(v) #transforma os valores em absoluto
  
  cv <- sum(av) #soma de todas as componentes de av
  
  c <- 1/cv  #coeficiente de normalização
  
  KO <- c*av  #autovetor normalizado sum(vc) = 1
  
  if((i-1)==0){fko<-append(KO, 0); fko<-replace(fko, c(1, I), fko[c(I, 1)])}
  
  else{fko<-append(KO, 0, after=i-1)} #trocar essa função after pelo índice#
  
  dif<-standart-fko #vetor diferença de fluxos
  
  K<-length(dif) #contador de cálculos de mi
  
  for(k in 1:K){if(isFALSE(dif[k]>0)==FALSE){CellsKO[k,2]<-CellsKO[k,2]+(dif[k]/(standart[k]*length(dif)))
  
  }
    
    else{CellsKO[k,2]<-CellsKO[k,2]+(abs(dif[k])/(fko[k]*length(dif)))}
    
  }
  
  }
  
  else{
    
    CellsKO[i,2]<-(count_components(gko, mode=c("strong")))/I #número de classes comunicantes dividido pelo total de possíveis
    
    CellsKO[i,3]<-"Not Connected"
    
    CellsKO[i,4]<-(count_components(gko, mode=c("strong")))
    
  }
  
}; print(CellsKO)

CellsKO <- CellsKO[rownames(CellsKO) != "Environment", ] #remove row of environment

CellsKO <- CellsKO[order(CellsKO$RME, decreasing = TRUE), ] #descreasing order

CellsKO$Cells <- NULL #remove Cells row

print(CellsKO) #check if environment is removed before saving

write.table(CellsKO, file="CellsKO.csv", col.names = NA, sep=";", dec=".")

# Knock-outs generalized/optimized for EDGES

#Objetive: calculate RME (Miranda et al., 2015) e add a index for knocked-out multigraphs that 
#are not connected

#Step 1. Choose any change in the initial multigraph g, in this case,
#specifically the list of edges

arr<-as.matrix(table(E(g)$name)); V<-row.names(arr)

J<-NULL; I<-length(V) #creation of the index I for calculation in looping

Signal<-V #list of the names of the components of the system

RME<-rep(0, length(V)) #Relative Mean Error - quantifies the importance of agents

Connectivity<-rep("not analysed", length(V)) #vector to receive the values: connected, not connected.

Numb.Comm.classes<-rep("not analysed", length(V)) #number of communicating classes

SignalKO<-data.frame(Signal, RME, Connectivity, Numb.Comm.classes)

print(SignalKO)

I<-length(V) #creation of the index I for looping calculation

for(i in 1:I){
  
  gko <- delete_edges(g, E(g)[name == V[i]]); #creates a KO-multigraph - the number of KO-multigraphs is I
  
  if(is.connected(gko, mode=c("strong"))==TRUE){SignalKO[i,4]<-1;
  
  SignalKO[i,3]<-"Connected"
  
  A <- as.matrix(get.adjacency(gko)) # transforms the multigraph into an adjacenty matrix
  
  D <- degree(gko,mode=c("out")) # vector that contains the out-degree of vertices
  
  M <- length(V(gko)) #index to run over all columns
  
  N <- length(V(gko))  #cria um indice para varredura das linhas
  
  for(n in 1:N){v<-D[n];
  
  for(m in 1:M){A[n,m]<-A[n,m]/v} #here we obtain the transition matrix
  
  }
  
  A # the sum of rows must be 1 (sum(A[i,])) == 1
  
  mysub <- function(x) {sub(",",".",x)} #função para mudar separador decimal
  
  mydata <- (apply(A, 2, mysub )) #aplicão de função de mudança de separador decimal
  
  rownames(A)<-NULL  #retira o rotulo das linhas
  
  colnames(A)<-NULL #retira o rotulo das colunas
  
  A[is.nan(A)] = 0 #troca NAN por 0
  
  # até aqui a matriz a é a matriz de transição dinamica do grafo g
  
  B <- t(A) # transpomos a matriz A em B
  
  e <- eigen(B)  # autovalores e autovetores
  
  v <-e$vectors[,1] #autovetor de valor 1
  
  av <- abs(v) #transforma os valores em absoluto
  
  cv <- sum(av) #soma de todas as componentes de av
  
  c <- 1/cv  #coeficiente de normalização
  
  KO <- c*av  #autovetor normalizado sum(KO) = 1
  
  dif<-standart-KO #vetor diferença de fluxos
  
  K<-length(V(gko)) #contador de cálculos de mi
  
  for(k in 1:K){if(isFALSE(dif[k]>0)==FALSE){SignalKO[i,2]<-SignalKO[i,2]+(dif[k]/(standart[k]*length(dif)))
  
  }
    
    else{SignalKO[i,2]<-SignalKO[i,2]+(abs(dif[k])/(KO[k]*length(dif)))}
    
  }
  
  }
  
  else{
    
    SignalKO[i,2]<-(count_components(gko, mode=c("strong")))/I #número de classes comunicantes dividido pelo total de possíveis
    
    SignalKO[i,3]<-"Not Connected"
    
    SignalKO[i,4]<-(count_components(gko, mode=c("strong")))
    
  }
  
}; print(SignalKO)

SignalKO <- SignalKO[!SignalKO$Signal %in% c("input", "output"), ]

rownames(SignalKO) <- NULL

SignalKO <- SignalKO[order(SignalKO$RME, decreasing = TRUE), ] #decreasing order

print(SignalKO) #check if environment is removed before saving

write.table(SignalKO, file="SignalKO.csv", col.names = NA, sep=";", dec=".")

