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
variaveis.importantes=cbind(dados[,c(2:5)],as.numeric(dados[,6]),(as.numeric(dados[,8])/as.numeric(dados[,7])),(as.numeric(dados[,12])/as.numeric(dados[,7])),(as.numeric(dados[,18])/as.numeric(dados[,7])),(as.numeric(dados[,20])/as.numeric(dados[,6])),(as.numeric(dados[,31])/as.numeric(dados[,6])),(as.numeric(dados[,30])/as.numeric(dados[,31])))
colnames(variaveis.importantes)[-c(1:4)]=c('Rnd','A/K','FK/K','HS%','ADR','CS','C%')

#Modelo final para A/K com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_assistencias=lm(variaveis.importantes$`A/K`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para FK/K com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_primeiros.abates=lm(variaveis.importantes$`FK/K`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para HS% com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_tiros.na.cabeca=lm(variaveis.importantes$`HS%`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para ADR com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_dano=lm(variaveis.importantes$`ADR`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para CS com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_situacoes.de.clutch=lm(variaveis.importantes$`CS`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para C% com jogadores como variáveis
#Modelo extremamente pesado, não precisa ser rodado sempre
#modelo_sucesso.em.clutch=lm(variaveis.importantes$`C%`~as.factor(variaveis.importantes$Player)+as.factor(paste(variaveis.importantes$Region,variaveis.importantes$Agent,variaveis.importantes$Map,sep=' - ')),weights=variaveis.importantes$Rnd,na.action=na.exclude)

#Modelo final para A/K sem jogadores como variáveis
variaveis.importantes_modelo_assistencias=variaveis.importantes[which(is.na(variaveis.importantes$`A/K`)==0 & is.finite(variaveis.importantes$`A/K`)==1),]
modelo_assistencias=lm(variaveis.importantes_modelo_assistencias$`A/K`~as.factor(paste(variaveis.importantes_modelo_assistencias$Region,variaveis.importantes_modelo_assistencias$Agent,variaveis.importantes_modelo_assistencias$Map,sep=' - ')),weights=variaveis.importantes_modelo_assistencias$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_assistencias=cbind(variaveis.importantes[,c(1:6)],NA,NA)
colnames(resultados.do.modelo_assistencias)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_assistencias[which(is.na(variaveis.importantes$`A/K`)==0 & is.finite(variaveis.importantes$`A/K`)==1),7]=modelo_assistencias$fitted.values
resultados.do.modelo_assistencias[,8]=resultados.do.modelo_assistencias[,6]-resultados.do.modelo_assistencias[,7]
write.csv2(resultados.do.modelo_assistencias,'Resultados do modelo 1 - Assistências.csv')

#Modelo final para FK/K sem jogadores como variáveis
variaveis.importantes_modelo_primeiros.abates=variaveis.importantes[which(is.na(variaveis.importantes$`FK/K`)==0 & is.finite(variaveis.importantes$`FK/K`)==1),]
modelo_primeiros.abates=lm(variaveis.importantes_modelo_primeiros.abates$`FK/K`~as.factor(paste(variaveis.importantes_modelo_primeiros.abates$Region,variaveis.importantes_modelo_primeiros.abates$Agent,variaveis.importantes_modelo_primeiros.abates$Map,sep=' - ')),weights=variaveis.importantes_modelo_primeiros.abates$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_primeiros.abates=cbind(variaveis.importantes[,c(1:5,7)],NA,NA)
colnames(resultados.do.modelo_primeiros.abates)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_primeiros.abates[which(is.na(variaveis.importantes$`FK/K`)==0 & is.finite(variaveis.importantes$`FK/K`)==1),7]=modelo_primeiros.abates$fitted.values
resultados.do.modelo_primeiros.abates[,8]=resultados.do.modelo_primeiros.abates[,6]-resultados.do.modelo_primeiros.abates[,7]
write.csv2(resultados.do.modelo_primeiros.abates,'Resultados do modelo 2 - Primeiros abates.csv')

#Modelo final para HS% sem jogadores como variáveis
variaveis.importantes_modelo_tiros.na.cabeca=variaveis.importantes[which(is.na(variaveis.importantes$`HS%`)==0 & is.finite(variaveis.importantes$`HS%`)==1),]
modelo_tiros.na.cabeca=lm(variaveis.importantes_modelo_tiros.na.cabeca$`HS%`~as.factor(paste(variaveis.importantes_modelo_tiros.na.cabeca$Region,variaveis.importantes_modelo_tiros.na.cabeca$Agent,variaveis.importantes_modelo_tiros.na.cabeca$Map,sep=' - ')),weights=variaveis.importantes_modelo_tiros.na.cabeca$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_tiros.na.cabeca=cbind(variaveis.importantes[,c(1:5,8)],NA,NA)
colnames(resultados.do.modelo_tiros.na.cabeca)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_tiros.na.cabeca[which(is.na(variaveis.importantes$`HS%`)==0 & is.finite(variaveis.importantes$`HS%`)==1),7]=modelo_tiros.na.cabeca$fitted.values
resultados.do.modelo_tiros.na.cabeca[,8]=resultados.do.modelo_tiros.na.cabeca[,6]-resultados.do.modelo_tiros.na.cabeca[,7]
write.csv2(resultados.do.modelo_tiros.na.cabeca,'Resultados do modelo 3 - Tiros na cabeça.csv')

#Modelo final para ADR sem jogadores como variáveis
variaveis.importantes_modelo_dano=variaveis.importantes[which(is.na(variaveis.importantes$`ADR`)==0 & is.finite(variaveis.importantes$`ADR`)==1),]
modelo_dano=lm(variaveis.importantes_modelo_dano$`ADR`~as.factor(paste(variaveis.importantes_modelo_dano$Region,variaveis.importantes_modelo_dano$Agent,variaveis.importantes_modelo_dano$Map,sep=' - ')),weights=variaveis.importantes_modelo_dano$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_dano=cbind(variaveis.importantes[,c(1:5,9)],NA,NA)
colnames(resultados.do.modelo_dano)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_dano[which(is.na(variaveis.importantes$`ADR`)==0 & is.finite(variaveis.importantes$`ADR`)==1),7]=modelo_dano$fitted.values
resultados.do.modelo_dano[,8]=resultados.do.modelo_dano[,6]-resultados.do.modelo_dano[,7]
write.csv2(resultados.do.modelo_dano,'Resultados do modelo 4 - Dano.csv')

#Modelo final para CS sem jogadores como variáveis
variaveis.importantes_modelo_situacoes.de.clutch=variaveis.importantes[which(is.na(variaveis.importantes$`CS`)==0 & is.finite(variaveis.importantes$`CS`)==1),]
modelo_situacoes.de.clutch=lm(variaveis.importantes_modelo_situacoes.de.clutch$`CS`~as.factor(paste(variaveis.importantes_modelo_situacoes.de.clutch$Region,variaveis.importantes_modelo_situacoes.de.clutch$Agent,variaveis.importantes_modelo_situacoes.de.clutch$Map,sep=' - ')),weights=variaveis.importantes_modelo_situacoes.de.clutch$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_situacoes.de.clutch=cbind(variaveis.importantes[,c(1:5,10)],NA,NA)
colnames(resultados.do.modelo_situacoes.de.clutch)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_situacoes.de.clutch[which(is.na(variaveis.importantes$`CS`)==0 & is.finite(variaveis.importantes$`CS`)==1),7]=modelo_situacoes.de.clutch$fitted.values
resultados.do.modelo_situacoes.de.clutch[,8]=resultados.do.modelo_situacoes.de.clutch[,6]-resultados.do.modelo_situacoes.de.clutch[,7]
write.csv2(resultados.do.modelo_situacoes.de.clutch,'Resultados do modelo 5 - Situações de clutch.csv')

#Modelo final para C% sem jogadores como variáveis
variaveis.importantes_modelo_sucesso.em.clutch=variaveis.importantes[which(is.na(variaveis.importantes$`C%`)==0 & is.finite(variaveis.importantes$`C%`)==1),]
modelo_sucesso.em.clutch=lm(variaveis.importantes_modelo_sucesso.em.clutch$`C%`~as.factor(paste(variaveis.importantes_modelo_sucesso.em.clutch$Region,variaveis.importantes_modelo_sucesso.em.clutch$Agent,variaveis.importantes_modelo_sucesso.em.clutch$Map,sep=' - ')),weights=variaveis.importantes_modelo_sucesso.em.clutch$Rnd,na.action=na.exclude)
#Criação do csv com os dados
resultados.do.modelo_sucesso.em.clutch=cbind(variaveis.importantes[,c(1:5,11)],NA,NA)
colnames(resultados.do.modelo_sucesso.em.clutch)[c(7,8)]=c('Fitted values','Residuals')
resultados.do.modelo_sucesso.em.clutch[which(is.na(variaveis.importantes$`C%`)==0 & is.finite(variaveis.importantes$`C%`)==1),7]=modelo_sucesso.em.clutch$fitted.values
resultados.do.modelo_sucesso.em.clutch[,8]=resultados.do.modelo_sucesso.em.clutch[,6]-resultados.do.modelo_sucesso.em.clutch[,7]
write.csv2(resultados.do.modelo_sucesso.em.clutch,'Resultados do modelo 6 - Sucesso em clutch.csv')