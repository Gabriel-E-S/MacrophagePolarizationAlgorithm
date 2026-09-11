# Usa a imagem oficial do Apache
FROM httpd:2.4

# Copia as configurações do Proxy que criamos
COPY apache-proxy.conf /usr/local/apache2/conf/extra/apache-proxy.conf

# Adiciona o proxy na configuração principal do Apache
RUN echo "Include conf/extra/apache-proxy.conf" >> /usr/local/apache2/conf/httpd.conf

# Copia os arquivos HTML/CSS/JS para a pasta exata da UEPG
# Isso garante que a URL /analise-redes-biologicas funcione perfeitamente
COPY html/ /usr/local/apache2/htdocs/analise-redes-biologicas/