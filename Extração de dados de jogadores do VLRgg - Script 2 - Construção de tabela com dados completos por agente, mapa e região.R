#Preâmbulo
library(stringr)

#Definição da pasta usada no computador pessoal
setwd('C:\\Users\\arviz\\Dropbox\\André\\Valorant\\Estatísticas')

#Leitura dos dados
dados=read.csv2('Dados de jogadores por região, agente e mapa.csv')

#Limpeza dos dados
dados=dados[-1]
linhas.com.nomes.com.caracteres.com.barra.ou.time=(grepl(pattern="[\n]",x=dados[,1])+grepl(pattern="[\t]",x=dados[,1])+grepl(pattern="[\r]",x=dados[,1]))>1
funcao.para.limpar.os.nomes=function(x){
  primeira.posicao.do.primeiro.caracter=unlist(gregexpr('[\n]',x))[1]
  primeira.posicao.do.segundo.caracter=unlist(gregexpr('[\r]',x))[1]
  primeira.posicao.do.terceiro.caracter=unlist(gregexpr('[\t]',x))[1]
  vetor.das.posicoes=c(primeira.posicao.do.primeiro.caracter,primeira.posicao.do.segundo.caracter,primeira.posicao.do.terceiro.caracter)
  if(sum(vetor.das.posicoes==-1)==3){
    return(x)
  } else{
    return(substring(text=x,first=1,last=min(vetor.das.posicoes[which(vetor.das.posicoes!=-1)])-1))
  }
}
nomes.sem.caracteres.com.barra.ou.time=apply(X=dados[1],MARGIN=1,FUN=funcao.para.limpar.os.nomes)
dados[,1]=nomes.sem.caracteres.com.barra.ou.time
dados=dados[-c(2,5,8,9,10,11,15)]
dados=cbind(dados,0,0)
dados[which(dados$CL!=''),c(17:18)]=do.call(rbind,strsplit(dados$CL[which(dados$CL!='')],split='/'))
dados=dados[c(1,14,15,16,2,9,11,10,12,13,4,6,5,3,17,18)]
dados=cbind(dados,(as.numeric(dados[,15])/as.numeric(dados[,16])))
colnames(dados)[c(15,16,17)]=c('C','CS','C%')
for(coluna in 1:17){
  dados[which(dados[,coluna]==''),coluna]=NA
}
dados.originais=dados

#Adição de variaveis importantes
taxa.de.assistencias.por.abate=dados$A/dados$K
taxa.de.abates.por.morte=dados$K/dados$D
taxa.de.primeiros.abates.por.primeira.morte=dados$FK/dados$FD
rodadas.de.impacto=round(dados$Rnd*(as.numeric(sub(x=dados$KAST,pattern="%",replacement=''))/100),digits=0)
abates.com.tiro.na.cabeca=round(dados$K*(as.numeric(sub(x=dados$HS.,pattern="%",replacement=''))/100),digits=0)
dano.causado=dados$ADR*dados$Rnd
pontuacao.de.combate=dados$ACS*dados$Rnd
abates.por.rodada=dados$K/dados$Rnd
assistencias.por.rodada=dados$A/dados$Rnd
mortes.por.rodada=dados$D/dados$Rnd
primeiros.abates.por.rodada=dados$FK/dados$Rnd
primeiras.mortes.por.rodada=dados$FD/dados$Rnd
rodadas.de.impacto.por.rodada=rodadas.de.impacto/dados$Rnd
proporcao.de.abates.com.tiro.na.cabeca=abates.com.tiro.na.cabeca/dados$K
situacoes.de.clutch.por.rodada=as.numeric(dados$CS)/dados$Rnd
dados=cbind(dados[,c(1:7)],taxa.de.assistencias.por.abate,dados[,8],taxa.de.abates.por.morte,dados[,c(9:10)],taxa.de.primeiros.abates.por.primeira.morte,dados[,11],rodadas.de.impacto,dados[,12],abates.com.tiro.na.cabeca,dados[,13],dano.causado,dados[,14],pontuacao.de.combate,abates.por.rodada,assistencias.por.rodada,mortes.por.rodada,primeiros.abates.por.rodada,primeiras.mortes.por.rodada,rodadas.de.impacto.por.rodada,proporcao.de.abates.com.tiro.na.cabeca,as.numeric(dados[,15]),as.numeric(dados[,16]),situacoes.de.clutch.por.rodada,dados[,17])
colnames(dados)[c(9,14,16,18,20,29,30,32)]=c('D','KAST%','HS%','ADR','ACS','C','CS','C%')
dados.expandidos=dados

#Funções para cálculo das medidas de resumo
funcao.para.calculo.de.percentil.01=function(x){
  return(quantile(x=x,probs=0.01,na.rm=T))
}
funcao.para.calculo.de.percentil.05=function(x){
  return(quantile(x=x,probs=0.05,na.rm=T))
}
funcao.para.calculo.de.percentil.10=function(x){
  return(quantile(x=x,probs=0.1,na.rm=T))
}
funcao.para.calculo.de.percentil.25=function(x){
  return(quantile(x=x,probs=0.25,na.rm=T))
}
funcao.para.calculo.de.percentil.50=function(x){
  return(quantile(x=x,probs=0.5,na.rm=T))
}
funcao.para.calculo.de.percentil.75=function(x){
  return(quantile(x=x,probs=0.75,na.rm=T))
}
funcao.para.calculo.de.percentil.90=function(x){
  return(quantile(x=x,probs=0.9,na.rm=T))
}
funcao.para.calculo.de.percentil.95=function(x){
  return(quantile(x=x,probs=0.95,na.rm=T))
}
funcao.para.calculo.de.percentil.99=function(x){
  return(quantile(x=x,probs=0.99,na.rm=T))
}
funcao.para.calculo.de.soma=function(x){
  return(sum(x=x,na.rm=T))
}
funcao.para.calculo.de.dp=function(x){
  return(sd(x=x,na.rm=T))
}
funcao.para.calculo.das.medidas.de.resumo=function(regiao,agente,mapa){
  if(regiao=='All' & agente=='All' & mapa=='All'){
    dados.com.os.filtros=dados
  }
  if(regiao=='All' & agente=='All' & mapa!='All'){
    dados.com.os.filtros=dados[which(dados$Map==mapa),]
  }
  if(regiao=='All' & agente!='All' & mapa=='All'){
    dados.com.os.filtros=dados[which(dados$Agent==agente),]
  }
  if(regiao!='All' & agente=='All' & mapa=='All'){
    dados.com.os.filtros=dados[which(dados$Region==regiao),]
  }
  if(regiao=='All' & agente!='All' & mapa!='All'){
    dados.com.os.filtros=dados[which(dados$Agent==agente & dados$Map==mapa),]
  }
  if(regiao!='All' & agente=='All' & mapa!='All'){
    dados.com.os.filtros=dados[which(dados$Region==regiao & dados$Map==mapa),]
  }
  if(regiao!='All' & agente!='All' & mapa=='All'){
    dados.com.os.filtros=dados[which(dados$Region==regiao & dados$Agent==agente),]
  }
  if(regiao!='All' & agente!='All' & mapa!='All'){
    dados.com.os.filtros=dados[which(dados$Region==regiao & dados$Agent==agente & dados$Map==mapa),]
  }
  dados.para.calculos=dados.com.os.filtros[-c(1:4,14,16)]
  linha.dos.percentis.01=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.01)
  linha.dos.percentis.05=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.05)
  linha.dos.percentis.10=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.10)
  linha.dos.percentis.25=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.25)
  linha.dos.percentis.50=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.50)
  linha.dos.percentis.75=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.75)
  linha.dos.percentis.90=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.90)
  linha.dos.percentis.95=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.95)
  linha.dos.percentis.99=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.percentil.99)
  linha.das.somas=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.soma)
  linha.dos.dps=apply(X=dados.para.calculos,MARGIN=2,FUN=funcao.para.calculo.de.dp)
  todas.as.linhas=rbind(linha.dos.percentis.01,linha.dos.percentis.05,linha.dos.percentis.10,linha.dos.percentis.25,linha.dos.percentis.50,linha.dos.percentis.75,linha.dos.percentis.90,linha.dos.percentis.95,linha.dos.percentis.99,linha.das.somas,linha.dos.dps)
  todas.as.linhas.com.as.variaveis.categoricas=cbind(c('Percentil 1%','Percentil 5%','Percentil 10%','Percentil 25%','Percentil 50%','Percentil 75%','Percentil 90%','Percentil 95%','Percentil 99%','Soma','DP'),regiao,agente,mapa,todas.as.linhas[,c(1:9)],NA,todas.as.linhas[,10],NA,todas.as.linhas[,c(11:26)])
  colnames(todas.as.linhas.com.as.variaveis.categoricas)=colnames(dados)
  return(todas.as.linhas.com.as.variaveis.categoricas)
}

#Criação de todas as linhas com medidas resumo
dados.completos=dados
regioes=c(rownames(table(dados$Region)),'All')
agentes=c(rownames(table(dados$Agent)),'All')
mapas=c(rownames(table(dados$Map)),'All')
for(regiao in 1:length(regioes)){
  for(agente in 1:length(agentes)){
    for(mapa in 1:length(mapas)){
      dados.completos=rbind(dados.completos,funcao.para.calculo.das.medidas.de.resumo(regiao=regioes[regiao],agente=agentes[agente],mapa=mapas[mapa]))
    }
  }
}

#Criação do csv com os dados
write.csv2(dados.completos,'Dados completos de jogadores por região, agente e mapa.csv')