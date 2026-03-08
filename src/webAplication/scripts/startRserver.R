library(plumber)

setwd("//wsl.localhost/Ubuntu-24.04/home/gabriel/github/IniciacaoCientifica/src/webAplication/scripts")

# Inicia a API na porta 8000
api <- pr("plumber.R")
pr_run(api, port = 8000)
