library(plumber)


#setwd("U:/home/gabriel/github/IniciacaoCientifica/src/webAplication/scripts")
setwd("U:/home/gabriel/github/MacrophagePolarizationAlgorithm/src/webAplication/scripts")

# Inicia a API na porta 8000
api <- pr("plumber.R")
pr_run(api, port = 8000)