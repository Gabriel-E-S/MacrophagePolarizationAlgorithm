library(plumber)
library(igraph)
library(Matrix)
library(base64enc)
library(nortest) # Nova biblioteca para o teste de Anderson-Darling

#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* @post /processar
#* @parser multi
function(req, res) {
  
  # 1. LEITURA
  arquivo <- req$body$arquivo_usuario
  
  if (is.list(arquivo) && !is.null(arquivo$value)) {
    if (is.raw(arquivo$value)) {
      texto_csv <- rawToChar(arquivo$value)
    } else {
      texto_csv <- as.character(arquivo$value)
    }
  } else if (is.raw(arquivo)) {
    texto_csv <- rawToChar(arquivo)
  } else if (is.character(arquivo)) {
    texto_csv <- arquivo
  } else {
    stop("O formato do arquivo recebido não foi compreendido.")
  }
  
  # 2. CÁLCULO ORIGINAL DA PESQUISA
  B <- read.table(text = texto_csv, sep = ";", header = FALSE)
  base <- as.matrix(B); base <- unname(base) 
  edges <- base[,-c(3)] 
  g <- graph_from_edgelist(edges, directed=T)
  arrows <- c(base[,3]) 
  E(g)$name <- arrows 
  V(g)$color <- "lightblue"
  
  if(is_connected(g, mode="strong")==FALSE){
    ge <- add_vertices(g,1, name="Environment"); 
    D <- length(degree(g)); F <- D+1;
    for(d in 1:D){
      if(degree(g, mode="out")[d]==0) {ge<-add_edges(ge, c(d,F)); E(ge)[length(E(ge))]$name<-"output"};
      if(degree(g, mode="in")[d]==0) {ge<-add_edges(ge, c(F,d)); E(ge)[length(E(ge))]$name<-"input"} 
    }
    V(ge)[F]$color<-"red"
    g <- ge 
  }
  
  A <- as_adjacency_matrix(g) 
  D <- degree(g,mode=c("out")) 
  P <- degree(g, mode=c("in")) 
  J <- length(V(g)) 
  I <- length(V(g))  
  
  for(i in 1:I){v<-D[i];
    for(j in 1:J){A[i,j]<-A[i,j]/v} 
  }
  
  mysub <- function(x) {sub(",",".",x)} 
  mydata <- (apply(A, 2, mysub )) 
  rownames(A)<-NULL  
  colnames(A)<-NULL 
  A[is.nan(A)] = 0 
  
  B_mat <- t(as.matrix(A)) 
  e <- eigen(B_mat)  
  v <-e$vectors[,1] 
  av <- abs(v) 
  cv <- sum(av) 
  c <- 1/cv  
  standart <- c*av  
  
  standart_df <- as.vector.data.frame(standart) 
  agents <- V(g)$name 
  agents <- as.vector.data.frame(agents) 
  flux <- cbind(agents, standart_df) 
  
  # --- Knock-outs VERTICES ---
  J <- NULL; I <- length(V(g)) 
  Cells <- V(g) 
  RME <- rep(0, length(V(g))) 
  Connectivity <- rep("not analysed", length(V(g))) 
  Numb.Comm.classes <- rep("not analysed", length(V(g))) 
  CellsKO <- data.frame(Cells, RME, Connectivity, Numb.Comm.classes)
  I <- length(V(g)) 
  
  for(i in 1:I){
    gko <- delete_vertices(g, V(g)[i]); 
    if(is_connected(gko, mode=c("strong"))==TRUE){
      CellsKO[i,4]<-1;
      CellsKO[i,3]<-"Connected"
      A_ko <- as.matrix(get.adjacency(gko)) 
      D_ko <- degree(gko,mode=c("out")) 
      M <- length(V(gko)) 
      N <- length(V(gko))  
      for(n in 1:N){
        v_ko <- D_ko[n];
        for(m in 1:M){A_ko[n,m]<-A_ko[n,m]/v_ko} 
      }
      mysub <- function(x) {sub(",",".",x)} 
      mydata <- (apply(A_ko, 2, mysub )) 
      rownames(A_ko)<-NULL  
      colnames(A_ko)<-NULL 
      A_ko[is.nan(A_ko)] = 0 
      
      B_ko <- t(as.matrix(A_ko)) 
      e_ko <- eigen(B_ko)  
      v_ko <- e_ko$vectors[,1] 
      av_ko <- abs(v_ko) 
      cv_ko <- sum(av_ko) 
      c_ko <- 1/cv_ko  
      KO <- c_ko*av_ko  
      
      if((i-1)==0){
        fko<-append(KO, 0); fko<-replace(fko, c(1, I), fko[c(I, 1)])
      } else {
        fko<-append(KO, 0, after=i-1)
      } 
      
      dif <- standart - fko 
      K <- length(dif) 
      
      for(k in 1:K){
        if(isFALSE(dif[k]>0)==FALSE){
          CellsKO[k,2] <- CellsKO[k,2]+(dif[k]/(standart[k]*length(dif)))
        } else {
          CellsKO[k,2] <- CellsKO[k,2]+(abs(dif[k])/(fko[k]*length(dif)))
        }
      }
    } else {
      CellsKO[i,2] <- (count_components(gko, mode=c("strong")))/I 
      CellsKO[i,3] <- "Not Connected"
      CellsKO[i,4] <- (count_components(gko, mode=c("strong")))
    }
  }
  
  CellsKO <- CellsKO[rownames(CellsKO) != "Environment", ] 
  CellsKO <- CellsKO[order(CellsKO$RME, decreasing = TRUE), ] 
  CellsKO$Cells <- NULL 
  
  # --- Knock-outs EDGES ---
  arr <- as.matrix(table(E(g)$name)); V_names <- row.names(arr)
  J <- NULL; I_edges <- length(V_names) 
  Signal <- V_names 
  RME <- rep(0, length(V_names)) 
  Connectivity <- rep("not analysed", length(V_names)) 
  Numb.Comm.classes <- rep("not analysed", length(V_names)) 
  SignalKO <- data.frame(Signal, RME, Connectivity, Numb.Comm.classes)
  
  for(i in 1:I_edges){
    gko <- delete_edges(g, E(g)[name == V_names[i]]); 
    if(is_connected(gko, mode=c("strong"))==TRUE){
      SignalKO[i,4]<-1;
      SignalKO[i,3]<-"Connected"
      A_ko <- as.matrix(get.adjacency(gko)) 
      D_ko <- degree(gko,mode=c("out")) 
      M <- length(V(gko)) 
      N <- length(V(gko))  
      for(n in 1:N){
        v_ko<-D_ko[n];
        for(m in 1:M){A_ko[n,m]<-A_ko[n,m]/v_ko} 
      }
      mysub <- function(x) {sub(",",".",x)} 
      mydata <- (apply(A_ko, 2, mysub )) 
      rownames(A_ko)<-NULL  
      colnames(A_ko)<-NULL 
      A_ko[is.nan(A_ko)] = 0 
      
      B_ko <- t(as.matrix(A_ko))
      e_ko <- eigen(B_ko)  
      v_ko <- e_ko$vectors[,1] 
      av_ko <- abs(v_ko) 
      cv_ko <- sum(av_ko) 
      c_ko <- 1/cv_ko  
      KO <- c_ko*av_ko  
      
      dif <- standart - KO 
      K <- length(V(gko)) 
      
      for(k in 1:K){
        if(isFALSE(dif[k]>0)==FALSE){
          SignalKO[i,2] <- SignalKO[i,2]+(dif[k]/(standart[k]*length(dif)))
        } else {
          SignalKO[i,2] <- SignalKO[i,2]+(abs(dif[k])/(KO[k]*length(dif)))
        }
      }
    } else {
      SignalKO[i,2] <- (count_components(gko, mode=c("strong")))/I_edges 
      SignalKO[i,3] <- "Not Connected"
      SignalKO[i,4] <- (count_components(gko, mode=c("strong")))
    }
  }
  
  SignalKO <- SignalKO[!SignalKO$Signal %in% c("input", "output"), ]
  rownames(SignalKO) <- NULL
  SignalKO <- SignalKO[order(SignalKO$RME, decreasing = TRUE), ] 
  
  # ==========================================================
  # 3. ANÁLISE ESTATÍSTICA 
  # ==========================================================
  cells_rme <- CellsKO$RME
  signal_rme <- SignalKO$RME 
  all_rme <- c(cells_rme, signal_rme)
  
  # Calculando as métricas e empacotando em uma lista para a web
  estatisticas_texto <- list(
    # Percentil 95
    cells_p95 = as.character(rownames(CellsKO[cells_rme > quantile(cells_rme, 0.95), , drop = FALSE])),
    signal_p95 = as.character(SignalKO[signal_rme > quantile(signal_rme, 0.95), 1]),
    
    # Outliers (Método de Tukey)
    cells_outliers = as.character(rownames(CellsKO[cells_rme > (quantile(cells_rme, 0.75) + 1.5 * (quantile(cells_rme, 0.75) - quantile(cells_rme, 0.25))), , drop = FALSE])),
    signal_outliers = as.character(SignalKO[signal_rme > (quantile(signal_rme, 0.75) + 1.5 * (quantile(signal_rme, 0.75) - quantile(signal_rme, 0.25))), 1])
  )
  
  # Geração da Imagem com os 3 Histogramas Estatísticos lado a lado
  caminho_hist <- tempfile(fileext = ".png")
  png(caminho_hist, width = 1200, height = 400, res = 100)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1)) # Divide a imagem em 3 colunas
  
  hist(cells_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "Cells KO Histogram", xlab="RME")
  lines(density(cells_rme), col = "red", lwd = 2) 
  
  hist(signal_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "Signal KO Histogram", xlab="RME")
  lines(density(signal_rme), col = "red", lwd = 2) 
  
  hist(all_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "All KO Histogram", xlab="RME")
  lines(density(all_rme), col = "red", lwd = 2)
  dev.off()
  
  imagem_hist_base64 <- base64encode(caminho_hist)
  
  # ==========================================================
  # 4. PREPARAÇÃO DA RESPOSTA WEB
  # ==========================================================
  csv1_texto <- paste(capture.output(write.table(CellsKO, col.names = NA, sep=";", dec=".")), collapse = "\n")
  csv2_texto <- paste(capture.output(write.table(SignalKO, col.names = NA, sep=";", dec=".")), collapse = "\n")
  
  caminho_imagem <- tempfile(fileext = ".png")
  png(caminho_imagem, width = 900, height = 700, res = 100)
  par(mar = c(1, 1, 3, 1))
  plot(g, vertex.size = 15, vertex.color = V(g)$color, vertex.label.color = "black", vertex.label.cex = 0.8, edge.label = E(g)$name, edge.label.cex = 0.7, edge.arrow.size = 0.4, edge.color = "gray", main = "Topologia da Rede (RME)")
  dev.off()
  imagem_em_texto <- base64encode(caminho_imagem)
  
  # Retorna tudo junto para a interface!
  list(
    arquivo1 = csv1_texto,
    arquivo2 = csv2_texto,
    grafo = imagem_em_texto,
    estatisticas_dados = estatisticas_texto,     # <- Novos dados aqui
    grafico_estatistico = imagem_hist_base64     # <- Novos gráficos aqui
  )
}