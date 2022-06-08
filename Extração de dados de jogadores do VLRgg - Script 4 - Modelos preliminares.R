#Preâmbulo
library(stringr)

#Definição da pasta usada no computador pessoal
setwd('C:\\Users\\arviz\\Dropbox\\André\\Valorant\\Estatísticas')

#Leitura dos dados
dados=read.csv2('Dados completos de jogadores por região, agente e mapa.csv')

#Seleção apenas de dados de jogadores
regioes=rownames(table(dados$Region))
numero.de.regioes=length(regioes)-1
agentes=rownames(table(dados$Agent))
numero.de.agentes=length(agentes)-1
mapas=rownames(table(dados$Map))
numero.de.mapas=length(mapas)-1
numero.de.medidas.de.resumo=11
numero.de.linhas.de.medidas.de.resumo=(numero.de.regioes+1)*(numero.de.agentes+1)*(numero.de.mapas+1)*numero.de.medidas.de.resumo
medidas.de.resumo_data.frame=dados[c((length(dados[,1])-(numero.de.linhas.de.medidas.de.resumo-1)):(length(dados[,1]))),]
dados=dados[-c((length(dados[,1])-(numero.de.linhas.de.medidas.de.resumo-1)):(length(dados[,1]))),]

#Definição de variáveis importantes
variaveis.importantes=cbind(dados[,c(2:5)],as.numeric(dados[,6]),(as.numeric(dados[,7])/as.numeric(dados[,6])),(as.numeric(dados[,8])/as.numeric(dados[,6])),(as.numeric(dados[,8])/as.numeric(dados[,7])),(as.numeric(dados[,10])/as.numeric(dados[,6])),(as.numeric(dados[,7])/as.numeric(dados[,10])),(as.numeric(dados[,12])/as.numeric(dados[,6])),(as.numeric(dados[,13])/as.numeric(dados[,6])),(as.numeric(dados[,12])/as.numeric(dados[,13])),(as.numeric(dados[,16])/as.numeric(dados[,6])),(as.numeric(dados[,18])/as.numeric(dados[,7])),(as.numeric(dados[,20])/as.numeric(dados[,6])),(as.numeric(dados[,22])/as.numeric(dados[,6])),(as.numeric(dados[,31])/as.numeric(dados[,6])),(as.numeric(dados[,30])/as.numeric(dados[,31])))
colnames(variaveis.importantes)[-c(1:4)]=c('Rnd','K','A','A/K','D','K/D','FK','FD','FK/FD','KAST%','HS%','ADR','ACS','CS','C%')

#Primeiros modelos - Agente, mapa e região como variáveis separadas
modelos=list()
medias.de.regioes_modelos=list()
medias.de.agentes_modelos=list()
medias.de.mapas_modelos=list()
r2.dos.modelos=c()
variaveis.para.correlacao=variaveis.importantes[,-c(1:5,7,9,12)]
contador=0
for(variavel in 1:14){
  variaveis.do.modelo=variaveis.importantes[,c(2,3,4,5,(variavel+5))]
  variaveis.do.modelo=variaveis.do.modelo[which(apply(X=is.na(variaveis.do.modelo),FUN=sum,1)==0),]
  variaveis.do.modelo_valores.finitos=variaveis.do.modelo
  for(coluna in 1:5){
    variaveis.do.modelo_valores.finitos[,coluna]=is.finite(variaveis.do.modelo[,coluna])
  }
  valores.finitos.por.coluna=apply(variaveis.do.modelo_valores.finitos,FUN=sum,MARGIN=1)
  variaveis.do.modelo=variaveis.do.modelo[which(valores.finitos.por.coluna==2),]
  modelos[[variavel]]=lm(variaveis.do.modelo[,5]~as.factor(variaveis.do.modelo[,1])+as.factor(variaveis.do.modelo[,2])+as.factor(variaveis.do.modelo[,3]),weights=variaveis.do.modelo[,4],na.action=na.exclude)
  valores.ajustados=modelos[[variavel]]$fitted.values
  coeficientes=modelos[[variavel]]$coefficients
  medias.de.regioes=c(coeficientes[1],(coeficientes[c(2:(numero.de.regioes))]+coeficientes[1]))
  names(medias.de.regioes)=regioes[-1]
  medias.de.regioes_modelos[[variavel]]=medias.de.regioes
  medias.de.agentes=c(coeficientes[1],(coeficientes[c((numero.de.regioes+1):(numero.de.regioes+numero.de.agentes-1))]+coeficientes[1]))
  names(medias.de.agentes)=agentes[-1]
  medias.de.agentes_modelos[[variavel]]=medias.de.agentes
  medias.de.mapas=c(coeficientes[1],(coeficientes[c((numero.de.regioes+numero.de.agentes):(numero.de.regioes+numero.de.agentes+numero.de.mapas-2))]+coeficientes[1]))
  names(medias.de.mapas)=mapas[-1]
  medias.de.mapas_modelos[[variavel]]=medias.de.mapas
  r2.dos.modelos[variavel]=cor(valores.ajustados,variaveis.do.modelo[,5])
  if(variavel!=2 & variavel!=4 & variavel!=7){
    contador=contador+1
    variaveis.para.correlacao=variaveis.para.correlacao[which(is.na(variaveis.para.correlacao[,contador])==0 & is.finite(variaveis.para.correlacao[,contador])==1),]
  }
}
medias.de.regioes_matriz=do.call(cbind,medias.de.regioes_modelos)
colnames(medias.de.regioes_matriz)=colnames(variaveis.importantes)[-c(1:5)]
medias.de.agentes_matriz=do.call(cbind,medias.de.agentes_modelos)
colnames(medias.de.agentes_matriz)=colnames(variaveis.importantes)[-c(1:5)]
medias.de.mapas_matriz=do.call(cbind,medias.de.mapas_modelos)
colnames(medias.de.mapas_matriz)=colnames(variaveis.importantes)[-c(1:5)]
names(r2.dos.modelos)=colnames(variaveis.importantes)[-c(1:5)]

#Correlações, ACP e AFE
variaveis.para.correlacao=cbind(variaveis.para.correlacao,variaveis.para.correlacao$FK/variaveis.para.correlacao$K)
colnames(variaveis.para.correlacao)[12]='FK/K'
cor(variaveis.para.correlacao)
factanal(variaveis.para.correlacao,factors=5)
prcomp(variaveis.para.correlacao,scale=FALSE)
#Resultados:
#K aparece no CP11 e no F1
#A/K aparece no CP4 e no F4
#K/D aparece no CP5 e no F1
#FK aparece no CP12 e no F2
#FK/FD aparece no CP3 e no F5
#KAST% aparece no CP8 e no F3
#HS% aparece no CP9 e no F4
#ADR aparece no CP2 e no F1
#ACS aparece no CP1 e no F1
#CS aparece no CP10 e no F3
#C% aparece no CP6 e no F3
#FK/K aparece no CP7 e no F2
#Definições:
#K, K/D, KAST%, ADR e ACS ficam no mesmo grupo de variáveis
#FK, FK/FD e FK/K ficam no mesmo grupo de variáveis
#A/K fica num grupo de variáveis
#CS fica num grupo de variáveis
#C% fica num grupo de variáveis
#Portanto, as 5 variáveis mais importantes são: K, A/K, FK/K, CS e C%

#Segunds modelos - Agente, mapa e região como variáveis conjuntas
modelos.combinados=list()
medias.por.combinacao_modelos=list()
r2.dos.modelos.combinados=c()
for(variavel in 1:14){
  variaveis.do.modelo=as.data.frame(cbind(paste(variaveis.importantes[,2],variaveis.importantes[,3],variaveis.importantes[,4],sep=' - '),as.numeric(variaveis.importantes[,5]),as.numeric(variaveis.importantes[,(variavel+5)])))
  variaveis.do.modelo=variaveis.do.modelo[which(apply(X=is.na(variaveis.do.modelo),FUN=sum,1)==0 & is.finite(as.numeric(variaveis.do.modelo[,3]))==1),]
  modelos.combinados[[variavel]]=lm(as.numeric(variaveis.do.modelo[,3])~as.factor(variaveis.do.modelo[,1]),weights=as.numeric(variaveis.do.modelo[,2]),na.action=na.exclude)
  valores.ajustados=modelos.combinados[[variavel]]$fitted.values
  coeficientes=modelos.combinados[[variavel]]$coefficients
  medias.por.combinacao=c(coeficientes[1],(coeficientes[-1]+coeficientes[1]))
  names(medias.por.combinacao)=rownames(table(variaveis.do.modelo[,1]))
  medias.por.combinacao_modelos[[variavel]]=medias.por.combinacao
  r2.dos.modelos.combinados[variavel]=cor(valores.ajustados,as.numeric(variaveis.do.modelo[,3]))
}
#medias.por.combinacao_matriz=do.call(cbind,medias.por.combinacao_modelos)
#colnames(medias.por.combinacao_matriz)=colnames(variaveis.importantes)[-c(1:5)]
names(r2.dos.modelos.combinados)=colnames(variaveis.importantes)[-c(1:5)]