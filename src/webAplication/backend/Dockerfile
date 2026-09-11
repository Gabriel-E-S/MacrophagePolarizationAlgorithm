# Usa uma imagem oficial do R com Plumber pré-instalado
FROM rstudio/plumber:latest

# Instala as dependências do sistema necessárias para compilar o igraph
RUN apt-get update -qq && apt-get install -y \
    libglpk-dev \
    libgmp3-dev \
    libxml2-dev

# Instala os pacotes do R
RUN R -e "install.packages(c('igraph', 'Matrix', 'base64enc'))"

# Copia a nossa API para dentro do container
COPY plumber.R /app/plumber.R
WORKDIR /app

# Expõe a porta 8000
EXPOSE 8000

# Roda o servidor Plumber
CMD ["/app/plumber.R"]