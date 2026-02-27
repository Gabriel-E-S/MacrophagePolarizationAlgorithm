#Objective: to automatically generate the RME of all vertices and edges of a 
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

tkplot(ge, edge.label=E(ge)$name) #graphical verification of the graph with environment

g<-ge #the protocol uses the g graph for calculations

# Generating the standard flux vector #

A <- as_adjacency_matrix(g) # transforms the graph into an adjacency matrix: 0 and 1

D <- degree(g,mode=c("out")) # a vector that contains information about the out-degree of the vertices

P<-degree(g, mode=c("in")) # in-degree vector

J <- length(V(g)) #Creates an index to scan the columns

I <- length(V(g))  # creates a index to scan the lines

for(i in 1:I){v<-D[i];

for(j in 1:J){A[i,j]<-A[i,j]/v} #  transforms the adjacency matrix into a transition matrix.

}

A #this is a row-stochastic matrix, because sum(i,)=1.

mysub <- function(x) {sub(",",".",x)} # function to change decimal separator

mydata <- (apply(A, 2, mysub )) # function to change decimal separator

rownames(A)<-NULL  #remove the label from the lines

colnames(A)<-NULL #remove the label from the columns

A[is.nan(A)] = 0 # changes NAN into 0

# up to here matrix A is the dynamic transition matrix of graph g

B <- t(A) #this is a column-stochastic matrix, because sum(,j)=1.

e <- eigen(B)  # eigenvalues and eigenvectors

v <-e$vectors[,1] #eigenvector with value 1

av <- abs(v) #transforms values to absolute

cv <- sum(av) #sum of all components of av

c <- 1/cv  #normalization coefficient

standart <- c*av  #normalized eigenvector sum(vc) = 1

standart<-as.vector.data.frame(standart) # transforms the normalized vector into a matrix

agents<-V(g)$name #assigns names again (nam was an R function)

agents<-as.vector.data.frame(agents) #we transform the labels into a matrix

flux<-cbind(agents, standart) #we join the fluxes (vc) with their names (nam)

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
  
  N <- length(V(gko))  #creates an index to scan the rows
  
  for(n in 1:N){v<-D[n];
  
  for(m in 1:M){A[n,m]<-A[n,m]/v} #here we obtain the transition matrix
  
  }
  
  A # the sum of rows must be 1 (sum(A[i,])) == 1
  
  mysub <- function(x) {sub(",",".",x)} #function to change decimal separator
  
  mydata <- (apply(A, 2, mysub )) #application of function to change decimal separator
  
  rownames(A)<-NULL  #removes row labels
  
  colnames(A)<-NULL #removes column labels
  
  A[is.nan(A)] = 0 #changes NAN to 0
  
  # up to here matrix A is the dynamic transition matrix of graph g
  
  B <- t(A) # we transpose matrix A into B
  
  e <- eigen(B)  # eigenvalues and eigenvectors
  
  v <-e$vectors[,1] #eigenvector with value 1
  
  av <- abs(v) #transforms values to absolute
  
  cv <- sum(av) #sum of all components of av
  
  c <- 1/cv  #normalization coefficient
  
  KO <- c*av  #normalized eigenvector sum(vc) = 1
  
  if((i-1)==0){fko<-append(KO, 0); fko<-replace(fko, c(1, I), fko[c(I, 1)])}
  
  else{fko<-append(KO, 0, after=i-1)} #replace this 'after' function with the index#
  
  dif<-standart-fko #flux difference vector
  
  K<-length(dif) #counter for mi calculations
  
  for(k in 1:K){if(isFALSE(dif[k]>0)==FALSE){CellsKO[k,2]<-CellsKO[k,2]+(dif[k]/(standart[k]*length(dif)))
  
  }
    
    else{CellsKO[k,2]<-CellsKO[k,2]+(abs(dif[k])/(fko[k]*length(dif)))}
    
  }
  
  }
  
  else{
    
    CellsKO[i,2]<-(count_components(gko, mode=c("strong")))/I #number of communicating classes divided by total possible
    
    CellsKO[i,3]<-"Not Connected"
    
    CellsKO[i,4]<-(count_components(gko, mode=c("strong")))
    
  }
  
}; print(CellsKO)

CellsKO <- CellsKO[rownames(CellsKO) != "Environment", ] #remove row of environment

CellsKO <- CellsKO[order(CellsKO$RME, decreasing = TRUE), ] #decreasing order

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
  
  N <- length(V(gko))  #creates an index to scan the rows
  
  for(n in 1:N){v<-D[n];
  
  for(m in 1:M){A[n,m]<-A[n,m]/v} #here we obtain the transition matrix
  
  }
  
  A # the sum of rows must be 1 (sum(A[i,])) == 1
  
  mysub <- function(x) {sub(",",".",x)} #function to change decimal separator
  
  mydata <- (apply(A, 2, mysub )) #application of function to change decimal separator
  
  rownames(A)<-NULL  #removes row labels
  
  colnames(A)<-NULL #removes column labels
  
  A[is.nan(A)] = 0 #changes NAN to 0
  
  # up to here matrix A is the dynamic transition matrix of graph g
  
  B <- t(A) # we transpose matrix A into B
  
  e <- eigen(B)  # eigenvalues and eigenvectors
  
  v <-e$vectors[,1] #eigenvector with value 1
  
  av <- abs(v) #transforms values to absolute
  
  cv <- sum(av) #sum of all components of av
  
  c <- 1/cv  #normalization coefficient
  
  KO <- c*av  #normalized eigenvector sum(KO) = 1
  
  dif<-standart-KO #flux difference vector
  
  K<-length(V(gko)) #counter for mi calculations
  
  for(k in 1:K){if(isFALSE(dif[k]>0)==FALSE){SignalKO[i,2]<-SignalKO[i,2]+(dif[k]/(standart[k]*length(dif)))
  
  }
    
    else{SignalKO[i,2]<-SignalKO[i,2]+(abs(dif[k])/(KO[k]*length(dif)))}
    
  }
  
  }
  
  else{
    
    SignalKO[i,2]<-(count_components(gko, mode=c("strong")))/I #number of communicating classes divided by total possible
    
    SignalKO[i,3]<-"Not Connected"
    
    SignalKO[i,4]<-(count_components(gko, mode=c("strong")))
    
  }
  
}; print(SignalKO)

SignalKO <- SignalKO[!SignalKO$Signal %in% c("input", "output"), ]

rownames(SignalKO) <- NULL

SignalKO <- SignalKO[order(SignalKO$RME, decreasing = TRUE), ] #decreasing order

print(SignalKO) #check if environment is removed before saving

write.table(SignalKO, file="SignalKO.csv", col.names = NA, sep=";", dec=".")