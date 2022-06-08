#Preâmbulo
library(stringr)

#Definição da pasta usada no computador pessoal
setwd('C:\\Users\\arviz\\Dropbox\\André\\Valorant\\Estatísticas')

#Leitura dos dados
dados=read.csv2('Dados completos de jogadores por região, agente e mapa.csv')

#Seleção apenas das medidas de resumo
regioes=rownames(table(dados$Region))
numero.de.regioes=length(regioes)-1
agentes=rownames(table(dados$Agent))
numero.de.agentes=length(agentes)-1
mapas=rownames(table(dados$Map))
numero.de.mapas=length(mapas)-1
numero.de.medidas.de.resumo=11
numero.de.linhas.de.medidas.de.resumo=(numero.de.regioes+1)*(numero.de.agentes+1)*(numero.de.mapas+1)*numero.de.medidas.de.resumo
medidas.de.resumo_data.frame=dados[c((length(dados[,1])-(numero.de.linhas.de.medidas.de.resumo-1)):(length(dados[,1]))),]

#Somas para cada caso
somas=medidas.de.resumo_data.frame[medidas.de.resumo_data.frame[,2]=='Soma',]

#Separação dos totais de cada região
totais.de.cada.regiao=somas[which(somas$Region!="All" & somas$Agent=='All' & somas$Map=="All"),]

#Números de rodadas jogadas para todos agentes em cada região
rodadas.por.regiao=as.numeric(totais.de.cada.regiao$Rnd)
pdf(file='Gráficos do VLRgg//Gráfico 1 - Rodadas jogadas em cada região.pdf')
gb=barplot(height=rodadas.por.regiao,names.arg=regioes[-1],las=2,col=rainbow(numero.de.regioes),cex.names=0.6,ylim=c(0,max(rodadas.por.regiao)*1.2),main='Rodadas jogadas em cada região')
text(x=gb,y=rodadas.por.regiao,labels=formatC(rodadas.por.regiao,format="f",big.mark = ",",digits=0),cex=0.8,pos=3)
dev.off()

#Taxas de escolha de cada agente no mundo
dados.de.agentes=somas[which(somas$Region=='All' & somas$Agent!='All' & somas$Map=="All"),]
rodadas.por.agente=as.numeric(dados.de.agentes$Rnd)
taxas.de.escolha=rodadas.por.agente/sum(rodadas.por.agente)
pdf(file='Gráficos do VLRgg//Gráfico 2 - Taxas de escolha de cada agente  - Região = All.pdf')
gb=barplot(height=rodadas.por.agente,names.arg=agentes[-1],las=2,col=rainbow(numero.de.agentes),cex.names=0.8,ylim=c(0,max(rodadas.por.agente)*1.2),main='Taxas de escolha de cada agente  - Região = All',cex.main=0.8)
text(x=gb,y=rodadas.por.agente,labels=paste0(as.character(formatC(rodadas.por.agente,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha*100,2)),'%'),')'),cex=0.5,pos=3)
dev.off()

#Taxas de escolha de cada agente por região
for(regiao in 2:length(regioes)){
  dados.da.regiao=somas[which(somas$Region==regioes[regiao] & somas$Agent!='All' & somas$Map=="All"),]
  rodadas.por.agente.na.regiao=as.numeric(dados.da.regiao$Rnd)
  taxas.de.escolha.na.regiao=rodadas.por.agente.na.regiao/sum(rodadas.por.agente.na.regiao)
  pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(regiao+1),' - Taxas de escolha de cada agente  - Região = ',regioes[regiao],'.pdf'))
  gb=barplot(height=rodadas.por.agente.na.regiao,names.arg=agentes[-1],las=2,col=rainbow(numero.de.agentes),cex.names=0.8,ylim=c(0,max(rodadas.por.agente.na.regiao)*1.2),main=paste0('Taxas de escolha de cada agente  - Região = ',regioes[regiao]),cex.main=0.8)
  text(x=gb,y=rodadas.por.agente.na.regiao,labels=paste0(as.character(formatC(rodadas.por.agente.na.regiao,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha.na.regiao*100,2)),'%'),')'),cex=0.5,pos=3)
  dev.off()
}

#Taxas de escolha de cada agente por mapa
for(mapa in 2:length(mapas)){
  dados.do.mapa=somas[which(somas$Region=='All' & somas$Agent!='All' & somas$Map==mapas[mapa]),]
  rodadas.por.agente.no.mapa=as.numeric(dados.do.mapa$Rnd)
  taxas.de.escolha.no.mapa=rodadas.por.agente.no.mapa/sum(rodadas.por.agente.no.mapa)
  pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(mapa+9),' - Taxas de escolha de cada agente  - Mapa = ',mapas[mapa],'.pdf'))
  gb=barplot(height=rodadas.por.agente.no.mapa,names.arg=agentes[-1],las=2,col=rainbow(numero.de.agentes),cex.names=0.8,ylim=c(0,max(rodadas.por.agente.no.mapa)*1.2),main=paste0('Taxas de escolha de cada agente  - Mapa = ',mapas[mapa]),cex.main=0.8)
  text(x=gb,y=rodadas.por.agente.no.mapa,labels=paste0(as.character(formatC(rodadas.por.agente.no.mapa,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha.no.mapa*100,2)),'%'),')'),cex=0.5,pos=3)
  dev.off()
}

#Taxas de escolha de cada agente por mapa e região
for(agente in 2:length(agentes)){
  if(agente!=8){
    dados.do.agente=somas[which(somas$Region!="All" & somas$Agent==agentes[agente] & somas$Map!='All'),]
    rodadas.com.o.agente=as.numeric(dados.do.agente$Rnd)
    taxas.de.escolha.na.regiao=rodadas.com.o.agente
    for(regiao in 2:length(regioes)){
      taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])]=taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])]/sum(taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])])
    }
    combinacoes.de.mapa.e.regiao=paste(dados.do.agente$Region,dados.do.agente$Map,sep=' - ')
    pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(agente+16),' - Taxas de escolha de cada mapa e região - Agente = ',agentes[agente],'.pdf'))
    gb=barplot(height=rodadas.com.o.agente,names.arg=combinacoes.de.mapa.e.regiao,las=2,col=rainbow(numero.de.regioes*numero.de.mapas),cex.names=0.4,ylim=c(0,max(rodadas.com.o.agente)*1.2),main=paste0('Taxas de escolha de cada mapa e região - Agente = ',agentes[agente]),cex.main=0.8)
    text(x=gb,y=rodadas.com.o.agente,labels=paste0(as.character(formatC(rodadas.com.o.agente,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha.na.regiao*100,2)),'%'),')'),cex=0.5,pos=3)
    dev.off()
  } else{
    dados.do.agente=somas[which(somas$Region!="All" & somas$Agent==agentes[agente] & somas$Map!='All'),]
    rodadas.com.o.agente=as.numeric(dados.do.agente$Rnd)
    taxas.de.escolha.na.regiao=rodadas.com.o.agente
    for(regiao in 2:length(regioes)){
      taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])]=taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])]/sum(taxas.de.escolha.na.regiao[which(dados.do.agente$Region==regioes[regiao])])
    }
    combinacoes.de.mapa.e.regiao=paste(dados.do.agente$Region,dados.do.agente$Map,sep=' - ')
    pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(agente+16),' - Taxas de escolha de cada mapa e região - Agente = Kayo.pdf'))
    gb=barplot(height=rodadas.com.o.agente,names.arg=combinacoes.de.mapa.e.regiao,las=2,col=rainbow(numero.de.regioes*numero.de.mapas),cex.names=0.4,ylim=c(0,max(rodadas.com.o.agente)*1.2),main=paste0('Taxas de escolha de cada mapa e região - Agente = ',agentes[agente]),cex.main=0.8)
    text(x=gb,y=rodadas.com.o.agente,labels=paste0(as.character(formatC(rodadas.com.o.agente,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha.na.regiao*100,2)),'%'),')'),cex=0.5,pos=3)
    dev.off()
  }
}

#Definição de variáveis importantes
variaveis.importantes=cbind(somas[,c(2:5)],as.numeric(somas[,6]),(as.numeric(somas[,7])/as.numeric(somas[,6])),(as.numeric(somas[,8])/as.numeric(somas[,6])),(as.numeric(somas[,8])/as.numeric(somas[,7])),(as.numeric(somas[,10])/as.numeric(somas[,6])),(as.numeric(somas[,7])/as.numeric(somas[,10])),(as.numeric(somas[,12])/as.numeric(somas[,6])),(as.numeric(somas[,13])/as.numeric(somas[,6])),(as.numeric(somas[,12])/as.numeric(somas[,13])),(as.numeric(somas[,16])/as.numeric(somas[,6])),(as.numeric(somas[,18])/as.numeric(somas[,7])),(as.numeric(somas[,20])/as.numeric(somas[,6])),(as.numeric(somas[,22])/as.numeric(somas[,6])),(as.numeric(somas[,31])/as.numeric(somas[,6])),(as.numeric(somas[,30])/as.numeric(somas[,31])))
colnames(variaveis.importantes)[-c(1:4)]=c('Rnd','K','A','A/K','D','K/D','FK','FD','FK/FD','KAST%','HS%','ADR','ACS','CS','C%')

#Dados dos agentes no mundo
dados.dos.agentes=variaveis.importantes[which(variaveis.importantes$Region=='All' & variaveis.importantes$Agent!='All' & variaveis.importantes$Map=='All'),]
#É necessário padronizar as variáveis, para que fiquem na mesma escala
#Para isso, serão geradas as médias e os desvios padrões de cada coluna
medias=apply(X=dados.dos.agentes[,-c(1:5)],FUN=mean,MARGIN=2)
dps=apply(X=dados.dos.agentes[,-c(1:5)],FUN=sd,MARGIN=2)
valores.padronizados=dados.dos.agentes[,-c(1:5)]
for(coluna in 1:14){
  valores.padronizados[,coluna]=(valores.padronizados[,coluna]-medias[coluna])/dps[coluna]
}
#Vamos plotar primeiro a primeira variável, e adicionaremos as outras
#gradualmente
pdf(file='Gráficos do VLRgg//Gráfico 36 - Variáveis por agente no mundo.pdf')
plot(x=rep(1,times=numero.de.agentes),y=valores.padronizados[,1],col=rainbow(numero.de.agentes),xlim=c(0,28),ylim=c(min(valores.padronizados)*1.2,max(valores.padronizados)*1.2),axes=F,pch=20,xlab='Variáveis',ylab='',main='Variáveis por agente no mundo')
text(x=2,y=seq(from=min(valores.padronizados)*1.1,to=max(valores.padronizados)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes[,6],2),sep=' - ')[order(dados.dos.agentes[,6],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes[,6],decreasing=F)],cex=0.2)
axis(side=2,at=seq(from=min(valores.padronizados)*1.1,to=max(valores.padronizados)*1.1,length.out=10),las=2,labels=round(seq(from=min(valores.padronizados)*1.1,to=max(valores.padronizados)*1.1,length.out=10),2),cex.axis=0.5)
axis(side=1,at=seq(from=1,to=27,by=2),labels=colnames(valores.padronizados),cex.axis=0.5)
for(coluna in 2:14){
  points(x=rep(seq(from=1,to=27,by=2)[coluna],times=numero.de.agentes),y=valores.padronizados[,coluna],col=rainbow(numero.de.agentes),pch=20)
  text(x=seq(from=2,to=28,by=2)[coluna],y=seq(from=min(valores.padronizados)*1.1,to=max(valores.padronizados)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes[,(5+coluna)],2),sep=' - ')[order(dados.dos.agentes[,(5+coluna)],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes[,(5+coluna)],decreasing=F)],cex=0.2)
}
dev.off()

#Funções necessárias para os próximos passos
funcao.para.calculo.de.media=function(x){
  return(mean(x,na.rm=T))
}
funcao.para.calculo.de.dp=function(x){
  return(sd(x,na.rm=T))
}

#Dados dos agentes por região
for(regiao in 2:length(regioes)){
  dados.dos.agentes.na.regiao=variaveis.importantes[which(variaveis.importantes$Region==regioes[regiao] & variaveis.importantes$Agent!='All' & variaveis.importantes$Map=='All'),]
  #É necessário padronizar as variáveis, para que fiquem na mesma escala
  #Para isso, serão geradas as médias e os desvios padrões de cada coluna
  medias=apply(X=dados.dos.agentes.na.regiao[,-c(1:5)],FUN=funcao.para.calculo.de.media,MARGIN=2)
  dps=apply(X=dados.dos.agentes.na.regiao[,-c(1:5)],FUN=funcao.para.calculo.de.dp,MARGIN=2)
  valores.padronizados=dados.dos.agentes.na.regiao[,-c(1:5)]
  for(coluna in 1:14){
    valores.padronizados[,coluna]=(valores.padronizados[,coluna]-medias[coluna])/dps[coluna]
  }
  #Vamos plotar primeiro a primeira variável, e adicionaremos as outras
  #gradualmente
  pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(35+regiao),' - Variáveis por agente - Região = ',regioes[regiao],'.pdf'))
  plot(x=rep(1,times=numero.de.agentes),y=valores.padronizados[,1],col=rainbow(numero.de.agentes),xlim=c(0,28),ylim=c(min(valores.padronizados,na.rm=T)*1.2,max(valores.padronizados,na.rm=T)*1.2),axes=F,pch=20,xlab='Variáveis',ylab='',main=paste0('Variáveis por agente - Região = ',regioes[regiao]))
  text(x=2,y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes.na.regiao[,6],2),sep=' - ')[order(dados.dos.agentes.na.regiao[,6],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes.na.regiao[,6],decreasing=F)],cex=0.2)
  axis(side=2,at=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),las=2,labels=round(seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),2),cex.axis=0.5)
  axis(side=1,at=seq(from=1,to=27,by=2),labels=colnames(valores.padronizados),cex.axis=0.5)
  for(coluna in 2:14){
    points(x=rep(seq(from=1,to=27,by=2)[coluna],times=numero.de.agentes),y=valores.padronizados[,coluna],col=rainbow(numero.de.agentes),pch=20)
    text(x=seq(from=2,to=28,by=2)[coluna],y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes.na.regiao[,(5+coluna)],2),sep=' - ')[order(dados.dos.agentes.na.regiao[,(5+coluna)],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes.na.regiao[,(5+coluna)],decreasing=F)],cex=0.2)
  }
  dev.off()
}

#Dados dos agentes por mapa
for(mapa in 2:length(mapas)){
  dados.dos.agentes.no.mapa=variaveis.importantes[which(variaveis.importantes$Region=='All' & variaveis.importantes$Agent!='All' & variaveis.importantes$Map==mapas[mapa]),]
  #É necessário padronizar as variáveis, para que fiquem na mesma escala
  #Para isso, serão geradas as médias e os desvios padrões de cada coluna
  medias=apply(X=dados.dos.agentes.no.mapa[,-c(1:5)],FUN=funcao.para.calculo.de.media,MARGIN=2)
  dps=apply(X=dados.dos.agentes.no.mapa[,-c(1:5)],FUN=funcao.para.calculo.de.dp,MARGIN=2)
  valores.padronizados=dados.dos.agentes.no.mapa[,-c(1:5)]
  for(coluna in 1:14){
    valores.padronizados[,coluna]=(valores.padronizados[,coluna]-medias[coluna])/dps[coluna]
  }
  #Vamos plotar primeiro a primeira variável, e adicionaremos as outras
  #gradualmente
  pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(43+mapa),' - Variáveis por agente - Mapa = ',mapas[mapa],'.pdf'))
  plot(x=rep(1,times=numero.de.agentes),y=valores.padronizados[,1],col=rainbow(numero.de.agentes),xlim=c(0,28),ylim=c(min(valores.padronizados,na.rm=T)*1.2,max(valores.padronizados,na.rm=T)*1.2),axes=F,pch=20,xlab='Variáveis',ylab='',main=paste0('Variáveis por agente - Mapa = ',mapas[mapa]))
  text(x=2,y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes.no.mapa[,6],2),sep=' - ')[order(dados.dos.agentes.no.mapa[,6],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes.no.mapa[,6],decreasing=F)],cex=0.2)
  axis(side=2,at=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),las=2,labels=round(seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),2),cex.axis=0.5)
  axis(side=1,at=seq(from=1,to=27,by=2),labels=colnames(valores.padronizados),cex.axis=0.5)
  for(coluna in 2:14){
    points(x=rep(seq(from=1,to=27,by=2)[coluna],times=numero.de.agentes),y=valores.padronizados[,coluna],col=rainbow(numero.de.agentes),pch=20)
    text(x=seq(from=2,to=28,by=2)[coluna],y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=numero.de.agentes),labels=paste(agentes[-1],round(dados.dos.agentes.no.mapa[,(5+coluna)],2),sep=' - ')[order(dados.dos.agentes.no.mapa[,(5+coluna)],decreasing=F)],col=rainbow(numero.de.agentes)[order(dados.dos.agentes.no.mapa[,(5+coluna)],decreasing=F)],cex=0.2)
  }
  dev.off()
}

#Dados dos agentes por mapa e região
for(agente in 2:length(agentes)){
  if(agente!=8){
    dados.por.agente=variaveis.importantes[which(variaveis.importantes$Region!='All' & variaveis.importantes$Agent==agentes[agente] & variaveis.importantes$Map!='All'),]
    #É necessário padronizar as variáveis, para que fiquem na mesma escala
    #Para isso, serão geradas as médias e os desvios padrões de cada coluna
    medias=apply(X=dados.por.agente[,-c(1:5)],FUN=funcao.para.calculo.de.media,MARGIN=2)
    dps=apply(X=dados.por.agente[,-c(1:5)],FUN=funcao.para.calculo.de.dp,MARGIN=2)
    valores.padronizados=dados.por.agente[,-c(1:5)]
    for(coluna in 1:14){
      valores.padronizados[,coluna]=(valores.padronizados[,coluna]-medias[coluna])/dps[coluna]
    }
    #Vamos plotar primeiro a primeira variável, e adicionaremos as outras
    #gradualmente
    pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(50+agente),' - Variáveis por agente - Agente = ',agentes[agente],'.pdf'))
    plot(x=rep(1,times=(numero.de.regioes*numero.de.mapas)),y=valores.padronizados[,1],col=rainbow(numero.de.agentes),xlim=c(0,28),ylim=c(min(valores.padronizados,na.rm=T)*1.2,max(valores.padronizados,na.rm=T)*1.2),axes=F,pch=20,xlab='Variáveis',ylab='',main=paste0('Variáveis por agente - Agente = ',agentes[agente]))
    text(x=2,y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=(numero.de.regioes*numero.de.mapas)),labels=paste(dados.por.agente[,2],dados.por.agente[,4],round(dados.por.agente[,6],2),sep=' - ')[order(dados.por.agente[,6],decreasing=F)],col=rainbow((numero.de.regioes*numero.de.mapas))[order(dados.por.agente[,6],decreasing=F)],cex=0.05)
    axis(side=2,at=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),las=2,labels=round(seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),2),cex.axis=0.5)
    axis(side=1,at=seq(from=1,to=27,by=2),labels=colnames(valores.padronizados),cex.axis=0.5)
    for(coluna in 2:14){
      points(x=rep(seq(from=1,to=27,by=2)[coluna],times=(numero.de.regioes*numero.de.mapas)),y=valores.padronizados[,coluna],col=rainbow((numero.de.regioes*numero.de.mapas)),pch=20)
      text(x=seq(from=2,to=28,by=2)[coluna],y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=(numero.de.regioes*numero.de.mapas)),labels=paste(dados.por.agente[,2],dados.por.agente[,4],round(dados.por.agente[,(5+coluna)],2),sep=' - ')[order(dados.por.agente[,(5+coluna)],decreasing=F)],col=rainbow((numero.de.regioes*numero.de.mapas))[order(dados.por.agente[,(5+coluna)],decreasing=F)],cex=0.05)
    }
    dev.off()
  } else{
    dados.por.agente=variaveis.importantes[which(variaveis.importantes$Region!='All' & variaveis.importantes$Agent==agentes[agente] & variaveis.importantes$Map!='All'),]
    #É necessário padronizar as variáveis, para que fiquem na mesma escala
    #Para isso, serão geradas as médias e os desvios padrões de cada coluna
    medias=apply(X=dados.por.agente[,-c(1:5)],FUN=funcao.para.calculo.de.media,MARGIN=2)
    dps=apply(X=dados.por.agente[,-c(1:5)],FUN=funcao.para.calculo.de.dp,MARGIN=2)
    valores.padronizados=dados.por.agente[,-c(1:5)]
    for(coluna in 1:14){
      valores.padronizados[,coluna]=(valores.padronizados[,coluna]-medias[coluna])/dps[coluna]
    }
    #Vamos plotar primeiro a primeira variável, e adicionaremos as outras
    #gradualmente
    pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(50+agente),' - Variáveis por agente - Agente = Kayo.pdf'))
    plot(x=rep(1,times=(numero.de.regioes*numero.de.mapas)),y=valores.padronizados[,1],col=rainbow(numero.de.agentes),xlim=c(0,28),ylim=c(min(valores.padronizados,na.rm=T)*1.2,max(valores.padronizados,na.rm=T)*1.2),axes=F,pch=20,xlab='Variáveis',ylab='',main=paste0('Variáveis por agente - Agente = ',agentes[agente]))
    text(x=2,y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=(numero.de.regioes*numero.de.mapas)),labels=paste(dados.por.agente[,2],dados.por.agente[,4],round(dados.por.agente[,6],2),sep=' - ')[order(dados.por.agente[,6],decreasing=F)],col=rainbow((numero.de.regioes*numero.de.mapas))[order(dados.por.agente[,6],decreasing=F)],cex=0.05)
    axis(side=2,at=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),las=2,labels=round(seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=10),2),cex.axis=0.5)
    axis(side=1,at=seq(from=1,to=27,by=2),labels=colnames(valores.padronizados),cex.axis=0.5)
    for(coluna in 2:14){
      points(x=rep(seq(from=1,to=27,by=2)[coluna],times=(numero.de.regioes*numero.de.mapas)),y=valores.padronizados[,coluna],col=rainbow((numero.de.regioes*numero.de.mapas)),pch=20)
      text(x=seq(from=2,to=28,by=2)[coluna],y=seq(from=min(valores.padronizados,na.rm=T)*1.1,to=max(valores.padronizados,na.rm=T)*1.1,length.out=(numero.de.regioes*numero.de.mapas)),labels=paste(dados.por.agente[,2],dados.por.agente[,4],round(dados.por.agente[,(5+coluna)],2),sep=' - ')[order(dados.por.agente[,(5+coluna)],decreasing=F)],col=rainbow((numero.de.regioes*numero.de.mapas))[order(dados.por.agente[,(5+coluna)],decreasing=F)],cex=0.05)
    }
    dev.off()
  }
}

#Taxas de escolha de cada agente no mundo
dados.de.mapas=somas[which(somas$Region=='All' & somas$Agent=='All' & somas$Map!="All"),]
rodadas.por.mapa=as.numeric(dados.de.mapas$Rnd)
taxas.de.escolha=rodadas.por.mapa/sum(rodadas.por.mapa)
pdf(file='Gráficos do VLRgg//Gráfico 70 - Taxas de escolha de cada mapa  - Região = All.pdf')
gb=barplot(height=rodadas.por.mapa,names.arg=mapas[-1],las=2,col=rainbow(numero.de.mapas),cex.names=0.8,ylim=c(0,max(rodadas.por.mapa)*1.2),main='Taxas de escolha de cada mapa  - Região = All',cex.main=0.8)
text(x=gb,y=rodadas.por.mapa,labels=paste0(as.character(formatC(rodadas.por.mapa,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha*100,2)),'%'),')'),cex=0.5,pos=3)
dev.off()

#Taxas de escolha de cada mapa por região
for(regiao in 2:length(regioes)){
  dados.da.regiao=somas[which(somas$Region==regioes[regiao] & somas$Agent=='All' & somas$Map!="All"),]
  rodadas.por.mapa.na.regiao=as.numeric(dados.da.regiao$Rnd)
  taxas.de.escolha.na.regiao=rodadas.por.mapa.na.regiao/sum(rodadas.por.mapa.na.regiao)
  pdf(file=paste0('Gráficos do VLRgg//Gráfico ',as.character(regiao+69),' - Taxas de escolha de cada mapa  - Região = ',regioes[regiao],'.pdf'))
  gb=barplot(height=rodadas.por.mapa.na.regiao,names.arg=mapas[-1],las=2,col=rainbow(numero.de.mapas),cex.names=0.8,ylim=c(0,max(rodadas.por.mapa.na.regiao)*1.2),main=paste0('Taxas de escolha de cada mapa  - Região = ',regioes[regiao]),cex.main=0.8)
  text(x=gb,y=rodadas.por.mapa.na.regiao,labels=paste0(as.character(formatC(rodadas.por.mapa.na.regiao,format="f",big.mark = ",",digits=0)),' (',paste0(as.character(round(taxas.de.escolha.na.regiao*100,2)),'%'),')'),cex=0.5,pos=3)
  dev.off()
}