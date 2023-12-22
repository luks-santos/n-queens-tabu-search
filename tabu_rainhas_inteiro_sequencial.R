#-----------------------------------------------------------------------------#
#                        N RAINHAS COM BUSCA TABU                             #
#                      REPRESENTACAO VETORIAL INTEIRA                         #
#                              SEQUENCIAL                                     #
#                                                                             #
# Ciniro Ap. Leite Nametala                                                   #
# ciniro@gmail.com                                                            #
# github.com/ciniro                                                           #
# V0.01 em 08/04/2019                                                         #
#-----------------------------------------------------------------------------#

rm(list=ls())
cat("\014")
require(png)
library(compiler)
require(tictoc)
library(arrangements)

#-----------------------------------------------------------------------------#
#                                                                             #
#                        N RAINHAS COM BUSCA TABU                             #
#                       DISCIPLINA: METAHEUR?STICAS                           #
#                                                                             #
#                     CINIRO APARECIDO LEITE NAMETALA                         #
#-----------------------------------------------------------------------------#
#                   FUNCOES UTILIZADAS DURANTE A EXECUCAO                     #
#-----------------------------------------------------------------------------#

converteParaTabuleirox <- function(sol, n) {
  tab <- matrix(rep(0, n^2), nrow=n, ncol=n, byrow=TRUE)
  
  for (i in 1:n) {
    tab[i,sol[i]] <- 1 
  }
  return (tab)
}
plotTabuleirox <- function(bestsol, n) {
  tab <- converteParaTabuleirox(bestsol, n)
  
  plot(1:(n+1), 1:(n+1), type = "n", xaxt="n", yaxt="n", xlab="", ylab="", main=paste("Melhor solucao: ",funobj(bestsol)," conflito(s)",sep=""))
  mtext(paste("Melhor solucao: [",paste(bestsol,collapse=","),"]"), side = 3, cex = 0.8)
  for (i in 1:n) {
    col <- if (i %% 2) c("white", "grey36") else c("grey36", "white")
    rect(i, 1:(n+1), i+1, (n+1), col = col, border = "brown")
  }
  
  img<-readPNG("queen.png")
  rainhas <- which(tab == 1, arr.ind = T)
  y <- rev(seq(n:1))
  for (i in 1:n) {
    rasterImage(img,rainhas[i,2]+0.3,y[rainhas[i,1]]+0.1,rainhas[i,2]+1-0.3,y[rainhas[i,1]]+1-0.1)
  } 
}
funobjx <- function(bestsol) {
  
  n <- length(bestsol)
  
  #inicializa com zero o numero de rainhas nas diagonais positiva e negativa
  dn <- cbind(seq(2,(2*n)),rep(0,(2*n)-1))
  dp <- cbind(seq(2-n-1,(2*n)-n-1),rep(0,(2*n)-1))
  
  #calcula quantas rainhas estao em cada diagonal para a solucao avaliada
  for (i in 1:n) {
    ind <- which(dn[,1]==(i + bestsol[i]))
    dn[ind,2] <- dn[ind,2] + 1
    
    ind <- which(dp[,1]==(i - bestsol[i]))
    dp[ind,2] <- dp[ind,2] + 1
  }
  
  #rainhaspassando aqui quer dizer quantidade de rainhas a mais do que deveria
  #existir em uma diagonal, ou seja, se tem 2, tem uma a mais, logo
  #a rainhaspassando nessa diagonal ? 1, queremos que tenha zero rainhaspassando
  rainhaspassando <- 0
  #avaliando colisoes na diagonal positiva
  for (i in 1:((2*n)-1)) {
    rainhaspassando <- rainhaspassando + max(0,(dp[i,2]-1))
  }
  
  #avaliando colisoes na diagonal negativa
  for (i in 1:((2*n)-1)) {
    rainhaspassando <- rainhaspassando + max(0,(dn[i,2]-1))
  }
  
  return(rainhaspassando)
}
geraProximaSolucaox <- function(bestsol, fitbestsol, tempo, n, tipoanalise, tempofreq, registraTabuMelhor, forcarConv) {

  vizinhos <- list()
  cont <- 0
  
  if (tipoanalise == 1)
    indmovpossiveis <- which(tabu[,3]==0)
  else if (tipoanalise == 2)
    indmovpossiveis <- which(tabu[,4]==0)
  else
    indmovpossiveis <- which(tabu[,3]==0 & tabu[,4]==0)
  
  print(paste("Movimentos possiveis:",length(indmovpossiveis)))
  
  for (i in 1:length(indmovpossiveis)) {
    vizinho <- bestsol
    vizinho[tabu[indmovpossiveis[i],1]] <- bestsol[tabu[indmovpossiveis[i],2]]
    vizinho[tabu[indmovpossiveis[i],2]] <- bestsol[tabu[indmovpossiveis[i],1]]
    
    cont <- cont + 1
    #se o vizinho for melhor que a bestsol ja retorna o mesmo logo
    #analisa para forcar ou nao a convergencia
    fitness <- funobj(vizinho)
    
    aceitaVizinho <- fitness<=fitbestsol
    if (forcarConv == TRUE)
      aceitaVizinho <- fitness<fitbestsol
    
    if (aceitaVizinho) {
      rm(vizinhos)
      
      if (registraTabuMelhor == TRUE) {
        tabu[indmovpossiveis[i],3] <<- tempo
        tabu[indmovpossiveis[i],4] <<- tabu[indmovpossiveis[i],4] + 1
      }
      
      return (vizinho) 
    }
    else {
      if(fitness == fitbestsol)
        vizinhos[[cont]] <- vizinho 
    }
  }

  #no caso de nao houver qualquer vizinho
  if (length(vizinhos)!=0) {
    #avaliacao de todos os vizinhos gerados
    fitvizinhos <- sapply(vizinhos,funobj)
    
    #captura o indice do melhor vizinho
    indmin <- sample(which(fitvizinhos==min(fitvizinhos)),1)
    melhorvizinho <- vizinhos[[indmin]]
    mov <- rev(which((melhorvizinho == bestsol)==FALSE))
    indmovtabu <- which((tabu[,1]==mov[1] & tabu[,2]==mov[2]) | 
                        (tabu[,1]==mov[2] & tabu[,2]==mov[1]))
  
    #atualiza tempo e frequencia
    tabu[indmovtabu,3] <<- tempo
    tabu[indmovtabu,4] <<- tabu[indmovtabu,4] + 1
    
    return (melhorvizinho)
  }
  else {
    #se nao houver vizinhos, retorna a solucao atual
    return (bestsol)
  }
  
}

atualizaListaTabux <- function() {
  indtempo <- which(tabu[,3] > 0)
  tabu[indtempo,3] <<- tabu[indtempo,3] - 1
}
#-----------------------------------------------------------------------------#
#                                                                             #
#                        N RAINHAS COM BUSCA TABU                             #
#                       DISCIPLINA: METAHEUR?STICAS                           #
#                                                                             #
#                     CINIRO APARECIDO LEITE NAMETALA                         #
#-----------------------------------------------------------------------------#
#                       COMPILACOES PARA VELOCIDADE                           #
#-----------------------------------------------------------------------------#

converteParaTabuleiro <- compiler::cmpfun(converteParaTabuleirox)
plotTabuleiro <- compiler::cmpfun(plotTabuleirox)
funobj <- compiler::cmpfun(funobjx)
geraProximaSolucao <- compiler::cmpfun(geraProximaSolucaox)
atualizaListaTabu <- compiler::cmpfun(atualizaListaTabux)
#-----------------------------------------------------------------------------#
#                                                                             #
#                        N RAINHAS COM BUSCA TABU                             #
#                       DISCIPLINA: METAHEUR?STICAS                           #
#                                                                             #
#                     CINIRO APARECIDO LEITE NAMETALA                         #
#-----------------------------------------------------------------------------#
#                          CODIGO PRINCIPAL                                   #
#-----------------------------------------------------------------------------#

#PARAMETRIZACAO DO EXPERIMENTO-------------------------------------------------
n <- 100                       #quantidade de rainhas e dimensoes nxn do tabuleiro
nexp <- 1                   #quantidade de execucoes do algoritmo
analisarexpisolado  <- TRUE #analisa o ultimo da bateria
analisaexperimentos <- TRUE #somente para mais que dois

#PARAMETRIZACOES DO ALGORITMO
tempo <- 5                  #tempo maximo que um movimento fica na lista tabu
maxit <- 1000                #criterio de parada maximo de iteracoes

#tipo de analise da busca
#1: somente por lista tabu
#2: somente por frequencia
#3: Por lista tabu e por frequencia
tipoanalise <- 3
tempofreq <- 5      #frequencia maxima permitida para um movimento

#registra na lista tabu os movimentos das melhores solucoes
registraTabuMelhor <- TRUE

#forcar convergencia
#a convergencia pode ser forca desconsiderando solucoes <=
#e considerando apenas solucoes < que a melhor, no entanto,
#o custo aumenta pois mais solucoes precisam ser avaliadas
forcarConv <- FALSE

#dados de experimentos
exp_melhorsolucao <- list()
exp_melhorfitness <- rep(NA, nexp)
exp_iteracaoparada <- rep(NA, nexp)

tic()
for (exp in 1:nexp) {
  
  #ALOCACAO DE MEMORIA---------------------------------------------------------
  #gera uma solucao inicial aleatoria
  solatual <- sample(n, replace=F)
  
  #aplica a funcao objetivo: Numero de rainhas a mais por diagonal
  fitatual <- funobj(solatual)
  
  #inicializa a matrix com tabu e frequencia
  movspossiveis <- combinations(n, 2)
  nmovs <- length(movspossiveis[,1])
  tabu <<- cbind(movspossiveis,rep(0,nmovs),rep(0,nmovs))
  rm(nmovs)
  rm(movspossiveis)
  
  #aloca as variaveis de monitoramento da qualidade do algoritmo
  iter_atualsol <- list(solatual)     #fitness da solucao corrente por iteracao
  iter_atualfit <- rep(NA, maxit)     #vetor que armazena a fitness da solucao corrente por iteracao 
  iter_atualfit[1] <- fitatual
  iter_bestsol <- list(solatual)     #melhor individuo iteracao por iteracao
  iter_bestfit <- rep(NA, maxit)     #vetor que armazena a melhor fitness por iteracao 
  iter_bestfit[1] <- fitatual
  
  bestind <- solatual
  bestfitind <- fitatual
  
  #EXECUCAO DO ALGORITMO--------------------------------------------------------
  it <- 1
  while ((it <= maxit) && (fitatual != 0)) {
    cat('\014')
    print(paste("EXPERIMENTO",exp,": Executando a iteracao", it, "de", maxit))
    
    #atualiza a solucao atual com a busca em vizinhanca
    solatual <- geraProximaSolucao(solatual, 
                                   fitatual, 
                                   tempo, 
                                   n, 
                                   tipoanalise, 
                                   tempofreq, 
                                   registraTabuMelhor,
                                   forcarConv)
    fitatual <- funobj(solatual)
    
    #analisa se houve melhoramento
    if (fitatual < bestfitind){
      bestind <- solatual
      bestfitind <- fitatual
    }
    
    #atualizacao dos indices de iteracao e armazenamento das solucoes
    #encontradas na iteracao corrente
    it <- it + 1
    iter_atualsol[[it]] <- solatual
    iter_atualfit[it] <- fitatual
    iter_bestsol[[it]] <- bestind
    iter_bestfit[it] <- bestfitind
    
    #atualiza a lista tabu decrementando 1 nos periodos registrados nos movimentos
    atualizaListaTabu()
  }
  
  if(it==101) 
    st <- (it-1) 
  else 
    st <-it
  
  exp_melhorsolucao[[exp]] <- bestind
  exp_melhorfitness[exp] <- bestfitind
  exp_iteracaoparada[exp] <- st
}
toc()

#ANALISE DE RESULTADOS-------------------------------------------------

if (analisarexpisolado == TRUE) {
  plot(iter_atualfit, type="l", lwd=2, xlab="",ylab="", main="", cex.axis=0.8, panel.first=grid())
  mtext(paste("Parada do algoritmo: ",st,"o iteracao"), side = 3, cex = 0.8)
  abline(v=it, col="blue", lty=2)
  title("Valores de fitness por iteracao", xlab="Iteracao", ylab="Fitness", cex.lab=0.9, cex.main=0.9)
  par(new=T)
  plot(iter_bestfit, type="l", lwd=2, col="forestgreen", xlab="",ylab="", main="", xaxt="n", yaxt="n")
  legend("topright", legend = c("Corrente","Melhor"), col=c("black","forestgreen"), pch=15, cex = 0.6)
  
  if (n<=20) {
    plotTabuleiro(bestind, n)
  }
  
  print(bestind)
  print(bestfitind)
}

if ((analisaexperimentos == TRUE) && (nexp != 1)) {
  
  x <- exp_iteracaoparada
  h<-barplot(x, xlab="Experimento", ylab="Iteracao", main=paste("Iteracoes de parada por experimento",sep=""), col=rainbow(8), names.arg = seq(1:nexp), cex.lab=0.9, cex.main=0.9, cex.axis = 0.8, ylim=c(0, max(x)+10))
  abline(h=mean(exp_iteracaoparada), col="blue", lty=2)
  
  x <- data.frame(table(exp_melhorfitness))
  
  vals <- seq(0, (n-1))
  freq <- rep(0, n)
  for (i in 1:length(x[,1])) {
    ind <- which(vals==x[,1][i])
    freq[ind] <- x[,2][i]
  }
  
  h<-barplot(freq, xlab="Melhor fitness", ylab="Frequ?ncia", main=paste("Frequencia de fitness",sep=""), col=rainbow(8), cex.lab=0.9, cex.main=0.9, cex.axis = 0.7, names.arg = vals, cex.names=0.7, ylim=c(0, max(freq+5)))
  text(h, freq, labels=freq, pos=3, cex = 0.7)
  legend("topright", legend = c(paste("Media =", round(mean(exp_melhorfitness), 2)),
                                paste("Mediana =",round(median(exp_melhorfitness), 2)),
                                paste("Desvio Padrao =", round(sd(exp_melhorfitness), 2)),
                                paste("Experimentos =",length(exp_melhorfitness))), 
         bty = "n", cex=0.6)
  
}