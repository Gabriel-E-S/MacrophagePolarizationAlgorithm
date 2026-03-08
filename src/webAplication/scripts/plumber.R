library(plumber)
library(igraph)
library(base64enc)

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
  
  dados_rede <- read.csv(text = texto_csv, sep = ";", header = FALSE)
  
  colnames(dados_rede) <- c("Origem", "Destino", "Interacao")
  
  grafo <- graph_from_data_frame(d = dados_rede[, 1:2], directed = TRUE)
  
  tabela_vertices <- data.frame(
    Vertice = V(grafo)$name, 
    Grau_Entrada = degree(grafo, mode = "in"),
    Grau_Saida = degree(grafo, mode = "out"),
    RME_Calculado = runif(vcount(grafo))
  )
  
  tabela_arestas <- data.frame(
    Origem = dados_rede$Origem, 
    Destino = dados_rede$Destino, 
    Interacao = dados_rede$Interacao, 
    RME_Calculado = runif(ecount(grafo)) 
  )
  
  csv1_texto <- paste(capture.output(write.csv(tabela_vertices, row.names = FALSE)), collapse = "\n")
  csv2_texto <- paste(capture.output(write.csv(tabela_arestas, row.names = FALSE)), collapse = "\n")
  
  caminho_imagem <- tempfile(fileext = ".png")
  
  png(caminho_imagem, width = 800, height = 600, res = 100)
  
  par(mar = c(1, 1, 3, 1))
  
  plot(grafo, 
       vertex.size = 12, 
       vertex.color = "#3498db", 
       vertex.label.color = "black",
       vertex.label.cex = 0.8,
       edge.arrow.size = 0.4, 
       edge.color = "gray",
       main = "Topologia da Rede de Sinalização")
       
  dev.off()
  
  imagem_em_texto <- base64encode(caminho_imagem)
  imagem_em_texto <- base64encode(caminho_imagem)
  
  list(
    arquivo1 = csv1_texto,
    arquivo2 = csv2_texto,
    grafo = imagem_em_texto
  )
}
