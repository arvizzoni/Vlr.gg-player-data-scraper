#Definição da pasta usada no computador pessoal
setwd('C:\\Users\\arviz\\Dropbox\\André\\Valorant\\Estatísticas')

#Leitura dos dados
dados=read.csv2('Dados completos de jogadores por região, agente e mapa.csv')
informacoes.de.agentes=read.csv2('Agentes.csv')
resultados.do.modelo_assistencias=read.csv2('Resultados do modelo 1 - Assistências.csv')
resultados.do.modelo_primeiros.abates=read.csv2('Resultados do modelo 2 - Primeiros abates.csv')
resultados.do.modelo_tiros.na.cabeca=read.csv2('Resultados do modelo 3 - Tiros na cabeça.csv')
resultados.do.modelo_dano=read.csv2('Resultados do modelo 4 - Dano.csv')
resultados.do.modelo_situacoes.de.clutch=read.csv2('Resultados do modelo 5 - Situações de clutch.csv')
resultados.do.modelo_sucesso.em.clutch=read.csv2('Resultados do modelo 6 - Sucesso em clutch.csv')

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

#Junção dos dados na linha de cada jogador
jogadores=rownames(table(dados$Player))
numero.de.regioes.por.jogador=apply(table(dados$Player,dados$Region)>0,FUN=sum,MARGIN=1)
linhas.jogadores.com.mais.de.uma.regiao=which(numero.de.regioes.por.jogador>1)
jogadores.com.mais.de.uma.regiao=jogadores[linhas.jogadores.com.mais.de.uma.regiao]
rodadas.jogadas.pelos.jogadores=c()
dados.do.jogador=list()
for(jogador in 1:length(jogadores)){
  #Definição de região(ões) do jogador
  numero.de.regioes.do.jogador=length(rownames(table(dados$Region[which(dados$Player==jogadores[jogador])])))
  if(numero.de.regioes.do.jogador>1){
    regioes.do.jogador=paste(rownames(table(dados$Region[which(dados$Player==jogadores[jogador])])),collapse=", ")
  } else{
    regioes.do.jogador=rownames(table(dados$Region[which(dados$Player==jogadores[jogador])]))
  }
  rodadas.jogadas.pelo.jogador=sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador])]),na.rm=T)
  rodadas.jogadas.pelos.jogadores[jogador]=rodadas.jogadas.pelo.jogador
  agentes.do.jogador=rownames(table(dados$Agent[which(dados$Player==jogadores[jogador])]))
  numeros.dos.agentes.do.jogador=match(x=agentes.do.jogador,table=agentes)
  #Dados de agentes
  dados.dos.agentes.do.jogador=list()
  for(agente in 1:numero.de.agentes){
    if((agente+1) %in% numeros.dos.agentes.do.jogador){
      rodadas.jogadas.pelo.jogador.com.o.agente=sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      assistencias.por.abate.com.o.agente=sum(as.numeric(dados$A[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      primeiros.abates.por.abate.com.o.agente=sum(as.numeric(dados$FK[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      abates.com.tiro.na.cabeca.por.abate.com.o.agente=sum(as.numeric(dados$abates.com.tiro.na.cabeca[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      dano.por.rodada.com.o.agente=sum(as.numeric(dados$dano.causado[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      situacoes.de.clutch.por.rodada.com.o.agente=sum(as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      sucesso.em.clutch.com.o.agente=sum(as.numeric(dados$C[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))/sum(as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]))
      assistencias.por.abate.esperadas.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_assistencias$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      primeiros.abates.por.abate.esperados.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_primeiros.abates$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      abates.com.tiro.na.cabeca.por.abate.esperados.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_tiros.na.cabeca$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      dano.por.rodada.esperado.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_dano$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      situacoes.de.clutch.por.rodada.esperadas.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_situacoes.de.clutch$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      sucesso.em.clutch.esperado.com.o.agente=weighted.mean(x=as.numeric(resultados.do.modelo_sucesso.em.clutch$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),w=as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Agent==agentes[agente+1])]),na.rm=T)
      assistencias.por.abate.acima.do.esperado.com.o.agente=assistencias.por.abate.com.o.agente-assistencias.por.abate.esperadas.com.o.agente
      primeiros.abates.por.abate.acima.do.esperado.com.o.agente=primeiros.abates.por.abate.com.o.agente-primeiros.abates.por.abate.esperados.com.o.agente
      abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.com.o.agente=abates.com.tiro.na.cabeca.por.abate.com.o.agente-abates.com.tiro.na.cabeca.por.abate.esperados.com.o.agente
      dano.por.rodada.acima.do.esperado.com.o.agente=dano.por.rodada.com.o.agente-dano.por.rodada.esperado.com.o.agente
      situacoes.de.clutch.por.rodada.acima.do.esperado.com.o.agente=situacoes.de.clutch.por.rodada.com.o.agente-situacoes.de.clutch.por.rodada.esperadas.com.o.agente
      sucesso.em.clutch.acima.do.esperado.com.o.agente=sucesso.em.clutch.com.o.agente-sucesso.em.clutch.esperado.com.o.agente
    } else{
      rodadas.jogadas.pelo.jogador.com.o.agente=0
      assistencias.por.abate.com.o.agente=0
      primeiros.abates.por.abate.com.o.agente=0
      abates.com.tiro.na.cabeca.por.abate.com.o.agente=0
      dano.por.rodada.com.o.agente=0
      situacoes.de.clutch.por.rodada.com.o.agente=0
      sucesso.em.clutch.com.o.agente=0
      assistencias.por.abate.esperadas.com.o.agente=0
      primeiros.abates.por.abate.esperados.com.o.agente=0
      abates.com.tiro.na.cabeca.por.abate.esperados.com.o.agente=0
      dano.por.rodada.esperado.com.o.agente=0
      situacoes.de.clutch.por.rodada.esperadas.com.o.agente=0
      sucesso.em.clutch.esperado.com.o.agente=0
      assistencias.por.abate.acima.do.esperado.com.o.agente=NA
      primeiros.abates.por.abate.acima.do.esperado.com.o.agente=NA
      abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.com.o.agente=NA
      dano.por.rodada.acima.do.esperado.com.o.agente=NA
      situacoes.de.clutch.por.rodada.acima.do.esperado.com.o.agente=NA
      sucesso.em.clutch.acima.do.esperado.com.o.agente=NA
    }
    linha.do.agente.para.o.jogador=c(rodadas.jogadas.pelo.jogador.com.o.agente,
                                     assistencias.por.abate.com.o.agente,
                                     primeiros.abates.por.abate.com.o.agente,
                                     abates.com.tiro.na.cabeca.por.abate.com.o.agente,
                                     dano.por.rodada.com.o.agente,
                                     situacoes.de.clutch.por.rodada.com.o.agente,
                                     sucesso.em.clutch.com.o.agente,
                                     assistencias.por.abate.esperadas.com.o.agente,
                                     primeiros.abates.por.abate.esperados.com.o.agente,
                                     abates.com.tiro.na.cabeca.por.abate.esperados.com.o.agente,
                                     dano.por.rodada.esperado.com.o.agente,
                                     situacoes.de.clutch.por.rodada.esperadas.com.o.agente,
                                     sucesso.em.clutch.esperado.com.o.agente,
                                     assistencias.por.abate.acima.do.esperado.com.o.agente,
                                     primeiros.abates.por.abate.acima.do.esperado.com.o.agente,
                                     abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.com.o.agente,
                                     dano.por.rodada.acima.do.esperado.com.o.agente,
                                     situacoes.de.clutch.por.rodada.acima.do.esperado.com.o.agente,
                                     sucesso.em.clutch.acima.do.esperado.com.o.agente)
    dados.dos.agentes.do.jogador[[agente]]=linha.do.agente.para.o.jogador
  }
  linha.dos.agentes.para.o.jogador=unlist(dados.dos.agentes.do.jogador)
  #Dados de mapas
  numero.de.mapas.do.jogador=length(table(dados$Map[which(dados$Player==jogadores[jogador])]))
  numeros.dos.mapas.do.jogador=match(x=rownames(table(dados$Map[which(dados$Player==jogadores[jogador])])),table=mapas)
  dados.dos.mapas.do.jogador=list()
  for(mapa in 1:numero.de.mapas){
    if((mapa+1) %in% numeros.dos.mapas.do.jogador){
      rodadas.jogadas.pelo.jogador.no.mapa=sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      assistencias.por.abate.no.mapa=sum(as.numeric(dados$A[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      primeiros.abates.por.abate.no.mapa=sum(as.numeric(dados$FK[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      abates.com.tiro.na.cabeca.por.abate.no.mapa=sum(as.numeric(dados$abates.com.tiro.na.cabeca[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      dano.por.rodada.no.mapa=sum(as.numeric(dados$dano.causado[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      situacoes.de.clutch.por.rodada.no.mapa=sum(as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      sucesso.em.clutch.no.mapa=sum(as.numeric(dados$C[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))/sum(as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]))
      assistencias.por.abate.esperadas.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_assistencias$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      primeiros.abates.por.abate.esperados.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_primeiros.abates$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      abates.com.tiro.na.cabeca.por.abate.esperados.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_tiros.na.cabeca$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$K[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      dano.por.rodada.esperado.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_dano$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      situacoes.de.clutch.por.rodada.esperadas.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_situacoes.de.clutch$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$Rnd[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      sucesso.em.clutch.esperado.no.mapa=weighted.mean(x=as.numeric(resultados.do.modelo_sucesso.em.clutch$Fitted.values[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),w=as.numeric(dados$CS[which(dados$Player==jogadores[jogador] & dados$Map==mapas[mapa+1])]),na.rm=T)
      assistencias.por.abate.acima.do.esperado.no.mapa=assistencias.por.abate.no.mapa-assistencias.por.abate.esperadas.no.mapa
      primeiros.abates.por.abate.acima.do.esperado.no.mapa=primeiros.abates.por.abate.no.mapa-primeiros.abates.por.abate.esperados.no.mapa
      abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.no.mapa=abates.com.tiro.na.cabeca.por.abate.no.mapa-abates.com.tiro.na.cabeca.por.abate.esperados.no.mapa
      dano.por.rodada.acima.do.esperado.no.mapa=dano.por.rodada.no.mapa-dano.por.rodada.esperado.no.mapa
      situacoes.de.clutch.por.rodada.acima.do.esperado.no.mapa=situacoes.de.clutch.por.rodada.no.mapa-situacoes.de.clutch.por.rodada.esperadas.no.mapa
      sucesso.em.clutch.acima.do.esperado.no.mapa=sucesso.em.clutch.no.mapa-sucesso.em.clutch.esperado.no.mapa
    } else{
      rodadas.jogadas.pelo.jogador.no.mapa=0
      assistencias.por.abate.no.mapa=0
      primeiros.abates.por.abate.no.mapa=0
      abates.com.tiro.na.cabeca.por.abate.no.mapa=0
      dano.por.rodada.no.mapa=0
      situacoes.de.clutch.por.rodada.no.mapa=0
      sucesso.em.clutch.no.mapa=0
      assistencias.por.abate.esperadas.no.mapa=0
      primeiros.abates.por.abate.esperados.no.mapa=0
      abates.com.tiro.na.cabeca.por.abate.esperados.no.mapa=0
      dano.por.rodada.esperado.no.mapa=0
      situacoes.de.clutch.por.rodada.esperadas.no.mapa=0
      sucesso.em.clutch.esperado.no.mapa=0
      assistencias.por.abate.acima.do.esperado.no.mapa=NA
      primeiros.abates.por.abate.acima.do.esperado.no.mapa=NA
      abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.no.mapa=NA
      dano.por.rodada.acima.do.esperado.no.mapa=NA
      situacoes.de.clutch.por.rodada.acima.do.esperado.no.mapa=NA
      sucesso.em.clutch.acima.do.esperado.no.mapa=NA
    }
    linha.do.mapa.para.o.jogador=c(rodadas.jogadas.pelo.jogador.no.mapa,
                                   assistencias.por.abate.no.mapa,
                                   primeiros.abates.por.abate.no.mapa,
                                   abates.com.tiro.na.cabeca.por.abate.no.mapa,
                                   dano.por.rodada.no.mapa,
                                   situacoes.de.clutch.por.rodada.no.mapa,
                                   sucesso.em.clutch.no.mapa,
                                   assistencias.por.abate.esperadas.no.mapa,
                                   primeiros.abates.por.abate.esperados.no.mapa,
                                   abates.com.tiro.na.cabeca.por.abate.esperados.no.mapa,
                                   dano.por.rodada.esperado.no.mapa,
                                   situacoes.de.clutch.por.rodada.esperadas.no.mapa,
                                   sucesso.em.clutch.esperado.no.mapa,
                                   assistencias.por.abate.acima.do.esperado.no.mapa,
                                   primeiros.abates.por.abate.acima.do.esperado.no.mapa,
                                   abates.com.tiro.na.cabeca.por.abate.acima.do.esperado.no.mapa,
                                   dano.por.rodada.acima.do.esperado.no.mapa,
                                   situacoes.de.clutch.por.rodada.acima.do.esperado.no.mapa,
                                   sucesso.em.clutch.acima.do.esperado.no.mapa)
    dados.dos.mapas.do.jogador[[mapa]]=linha.do.mapa.para.o.jogador
  }
  linha.dos.mapas.para.o.jogador=unlist(dados.dos.mapas.do.jogador)
  #Combinação de dados de agentes com dados de mapas
  dados.do.jogador[[jogador]]=c(regioes.do.jogador,linha.dos.agentes.para.o.jogador,linha.dos.mapas.para.o.jogador)
  #Iteração
  cat('Estamos na iteração',jogador,'\n',sep=' ')
}
dados.dos.jogadores=do.call(rbind,dados.do.jogador)
dados.dos.jogadores=cbind(jogadores,rodadas.jogadas.pelos.jogadores,dados.dos.jogadores)
dados.dos.jogadores=as.data.frame(dados.dos.jogadores)
colnames(dados.dos.jogadores)[1:3]=c('Jogador','Total de rodadas jogadas','Regiões do jogador')
for(agente in 2:length(agentes)){
  colnames(dados.dos.jogadores)[(4+(agente-2)*19):(22+(agente-2)*19)]=paste(c('Rodadas com o agente',
                                                                    'Assistências por abate com o agente',
                                                                    'Primeiros abates por abate com o agente',
                                                                    'Abates com tiro na cabeça por abate com o agente',
                                                                    'Dano por rodada com o agente',
                                                                    'Situações de clutch por rodada com o agente',
                                                                    'Sucesso em clutch com o agente',
                                                                    'Assistências por abate esperadas com o agente',
                                                                    'Primeiros abates por abate esperados com o agente',
                                                                    'Abates com tiro na cabeça por abate esperados com o agente',
                                                                    'Dano por rodada esperado com o agente',
                                                                    'Situações de clutch por rodada esperadas com o agente',
                                                                    'Sucesso em clutch esperado com o agente',
                                                                    'Assistências por abate acima do esperado com o agente',
                                                                    'Primeiros abates por abate acima do esperado com o agente',
                                                                    'Abates com tiro na cabeça por abate acima do esperado com o agente',
                                                                    'Dano por rodada acima do esperado com o agente',
                                                                    'Situações de clutch por rodada acima do esperado com o agente',
                                                                    'Sucesso em clutch acima do esperado com o agente'),agentes[agente],sep=' - ')
}
for(mapa in 2:length(mapas)){
  colnames(dados.dos.jogadores)[(346+(mapa-2)*19):(364+(mapa-2)*19)]=paste(c('Rodadas com o mapa',
                                                                         'Assistências por abate com o mapa',
                                                                         'Primeiros abates por abate com o mapa',
                                                                         'Abates com tiro na cabeça por abate com o mapa',
                                                                         'Dano por rodada com o mapa',
                                                                         'Situações de clutch por rodada com o mapa',
                                                                         'Sucesso em clutch com o mapa',
                                                                         'Assistências por abate esperadas com o mapa',
                                                                         'Primeiros abates por abate esperados com o mapa',
                                                                         'Abates com tiro na cabeça por abate esperados com o mapa',
                                                                         'Dano por rodada esperado com o mapa',
                                                                         'Situações de clutch por rodada esperadas com o mapa',
                                                                         'Sucesso em clutch esperado com o mapa',
                                                                         'Assistências por abate acima do esperado com o mapa',
                                                                         'Primeiros abates por abate acima do esperado com o mapa',
                                                                         'Abates com tiro na cabeça por abate acima do esperado com o mapa',
                                                                         'Dano por rodada acima do esperado com o mapa',
                                                                         'Situações de clutch por rodada acima do esperado com o mapa',
                                                                         'Sucesso em clutch acima do esperado com o mapa'),mapas[mapa],sep=' - ')
}

for(coluna in 1:length(dados.dos.jogadores[1,])){
  if(coluna!=1 & coluna!=3){
    dados.dos.jogadores[,coluna]=as.numeric(dados.dos.jogadores[,coluna])
  }
}
dados.dos.jogadores[,c(2,4:478)]=as.numeric(dados.dos.jogadores[,c(2,4:478)])

#Criação do csv com os dados dos jogadores
write.csv2(dados.dos.jogadores,'Dados completos por jogador no mundo - VLRgg.csv')

#Cálculo das médias ponderadas acima do esperado para cada jogador
media.ponderada.acima.do.esperado_modelo_assistencias=c()
media.ponderada.acima.do.esperado_modelo_primeiros.abates=c()
media.ponderada.acima.do.esperado_modelo_tiros.na.cabeca=c()
media.ponderada.acima.do.esperado_dano=c()
media.ponderada.acima.do.esperado_situacoes.de.clutch=c()
media.ponderada.acima.do.esperado_sucesso.em.clutch=c()
for(jogador in 1:length(jogadores)){
  residuos.do.jogador_modelo_assistencias=resultados.do.modelo_assistencias[which(resultados.do.modelo_assistencias$Player==jogadores[jogador]),]
  media.ponderada_modelo_assistencias=weighted.mean(x=residuos.do.jogador_modelo_assistencias$Residuals,w=residuos.do.jogador_modelo_assistencias$Rnd,na.rm=T)
  residuos.do.jogador_modelo_primeiros.abates=resultados.do.modelo_primeiros.abates[which(resultados.do.modelo_primeiros.abates$Player==jogadores[jogador]),]
  media.ponderada_modelo_primeiros.abates=weighted.mean(x=residuos.do.jogador_modelo_primeiros.abates$Residuals,w=residuos.do.jogador_modelo_primeiros.abates$Rnd,na.rm=T)
  residuos.do.jogador_modelo_tiros.na.cabeca=resultados.do.modelo_tiros.na.cabeca[which(resultados.do.modelo_tiros.na.cabeca$Player==jogadores[jogador]),]
  media.ponderada_modelo_tiros.na.cabeca=weighted.mean(x=residuos.do.jogador_modelo_tiros.na.cabeca$Residuals,w=residuos.do.jogador_modelo_tiros.na.cabeca$Rnd,na.rm=T)
  residuos.do.jogador_modelo_dano=resultados.do.modelo_dano[which(resultados.do.modelo_dano$Player==jogadores[jogador]),]
  media.ponderada_modelo_dano=weighted.mean(x=residuos.do.jogador_modelo_dano$Residuals,w=residuos.do.jogador_modelo_dano$Rnd,na.rm=T)
  residuos.do.jogador_modelo_situacoes.de.clutch=resultados.do.modelo_situacoes.de.clutch[which(resultados.do.modelo_situacoes.de.clutch$Player==jogadores[jogador]),]
  media.ponderada_modelo_situacoes.de.clutch=weighted.mean(x=residuos.do.jogador_modelo_situacoes.de.clutch$Residuals,w=residuos.do.jogador_modelo_situacoes.de.clutch$Rnd,na.rm=T)
  residuos.do.jogador_modelo_sucesso.em.clutch=resultados.do.modelo_sucesso.em.clutch[which(resultados.do.modelo_sucesso.em.clutch$Player==jogadores[jogador]),]
  media.ponderada_modelo_sucesso.em.clutch=weighted.mean(x=residuos.do.jogador_modelo_sucesso.em.clutch$Residuals,w=residuos.do.jogador_modelo_sucesso.em.clutch$Rnd,na.rm=T)
  media.ponderada.acima.do.esperado_modelo_assistencias[jogador]=media.ponderada_modelo_assistencias
  media.ponderada.acima.do.esperado_modelo_primeiros.abates[jogador]=media.ponderada_modelo_primeiros.abates
  media.ponderada.acima.do.esperado_modelo_tiros.na.cabeca[jogador]=media.ponderada_modelo_tiros.na.cabeca
  media.ponderada.acima.do.esperado_dano[jogador]=media.ponderada_modelo_dano
  media.ponderada.acima.do.esperado_situacoes.de.clutch[jogador]=media.ponderada_modelo_situacoes.de.clutch
  media.ponderada.acima.do.esperado_sucesso.em.clutch[jogador]=media.ponderada_modelo_sucesso.em.clutch
}
medias.ponderadas.acima.do.esperado=cbind(rodadas.jogadas.pelos.jogadores,media.ponderada.acima.do.esperado_modelo_assistencias,media.ponderada.acima.do.esperado_modelo_primeiros.abates,media.ponderada.acima.do.esperado_modelo_tiros.na.cabeca,media.ponderada.acima.do.esperado_dano,media.ponderada.acima.do.esperado_situacoes.de.clutch,media.ponderada.acima.do.esperado_sucesso.em.clutch)
colnames(medias.ponderadas.acima.do.esperado)=c('Rnd','A/K','FK/K','HS%','ADR','CS','C%')
rownames(medias.ponderadas.acima.do.esperado)=jogadores

#Criação do csv com os dados
write.csv2(medias.ponderadas.acima.do.esperado,'Médias ponderadas acima do esperado para cada jogador de acordo com região, agente e mapa.csv')

#Definições dos vetores para o cálculo das médias a posteriori
#para cada variável chave
colunas.de.assistencias.por.abate=seq(from=17,to=340,by=19)
colunas.de.primeiros.abates.por.abate=seq(from=18,to=341,by=19)
colunas.de.abates.com.tiro.na.cabeca.por.abate=seq(from=19,to=342,by=19)
colunas.de.dano.por.rodada=seq(from=20,to=343,by=19)
colunas.de.situacoes.de.clutch.por.rodada=seq(from=21,to=344,by=19)
colunas.de.sucesso.em.clutch=seq(from=22,to=345,by=19)
colunas.de.rodadas.com.os.agentes=seq(from=4,to=327,by=19)
dados.de.assistencias.por.abate=dados.dos.jogadores[,colunas.de.assistencias.por.abate]
dados.de.primeiros.abates.por.abate=dados.dos.jogadores[,colunas.de.primeiros.abates.por.abate]
dados.de.abates.com.tiro.na.cabeca.por.abate=dados.dos.jogadores[,colunas.de.abates.com.tiro.na.cabeca.por.abate]
dados.de.dano.por.rodada=dados.dos.jogadores[,colunas.de.dano.por.rodada]
dados.de.situacoes.de.clutch.por.rodada=dados.dos.jogadores[,colunas.de.situacoes.de.clutch.por.rodada]
dados.de.sucesso.em.clutch=dados.dos.jogadores[,colunas.de.sucesso.em.clutch]
dados.de.rodadas.com.os.agentes=dados.dos.jogadores[,colunas.de.rodadas.com.os.agentes]
dados.de.assistencias.por.abate_vetor=unlist(dados.de.assistencias.por.abate)
dados.de.primeiros.abates.por.abate_vetor=unlist(dados.de.primeiros.abates.por.abate)
dados.de.abates.com.tiro.na.cabeca.por.abate_vetor=unlist(dados.de.abates.com.tiro.na.cabeca.por.abate)
dados.de.dano.por.rodada_vetor=unlist(dados.de.dano.por.rodada)
dados.de.situacoes.de.clutch.por.rodada_vetor=unlist(dados.de.situacoes.de.clutch.por.rodada)
dados.de.sucesso.em.clutch_vetor=unlist(dados.de.sucesso.em.clutch)
dados.de.rodadas.com.os.agentes_vetor=unlist(dados.de.rodadas.com.os.agentes)
dados.de.rodadas.com.os.agentes_matriz=matrix(dados.de.rodadas.com.os.agentes_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Fórmula para calcular os desvios padrões de cada indivíduo
formula.para.gerar.estimativas.de.desvio.padrao=function(valores,passos){
  maximo.de.rodadas=7500
  tamanho.do.passo=7500/passos
  desvios.padroes=c()
  for(passo in 1:passos){
    if(passo<passos){
      desvios.padroes[passo]=sd(valores[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*passos & dados.de.rodadas.com.os.agentes_vetor<passo*passos)],na.rm=T)
    } else{
      desvios.padroes[passo]=sd(valores[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*passos)],na.rm=T)
    }
  }
  return(desvios.padroes)
}

#Cálculo das médias a posteriori para assistências por abate
hist(dados.de.assistencias.por.abate_vetor)
summary(dados.de.assistencias.por.abate_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.assistencias.por.abate_vetor)
numero.de.passos=50
media.a.priori_assistencias.por.abate=0
desvio.padrao.a.piori_assistencias.por.abate=0.005
possiveis.desvios.padroes.de.assistencias.por.abate_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.assistencias.por.abate_vetor,passos=numero.de.passos)
desvios.padroes.de.assistencias.por.abate_vetor=dados.de.assistencias.por.abate_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.assistencias.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.assistencias.por.abate_vetor[passo]
  } else{
    desvios.padroes.de.assistencias.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.assistencias.por.abate_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.assistencias.por.abate_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.assistencias.por.abate_vetor^(-2)+desvio.padrao.a.piori_assistencias.por.abate^(-2))^(-1))
medias.a.posteriori.de.assistencias.por.abate_vetor=(desvios.padroes.a.posteriori.de.assistencias.por.abate_vetor^2)*((media.a.priori_assistencias.por.abate*desvio.padrao.a.piori_assistencias.por.abate^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.assistencias.por.abate_vetor*desvios.padroes.de.assistencias.por.abate_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.assistencias.por.abate_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.assistencias.por.abate_vetor,dados.de.assistencias.por.abate_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.assistencias.por.abate_matriz=matrix(medias.a.posteriori.de.assistencias.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.assistencias.por.abate_matriz=matrix(desvios.padroes.a.posteriori.de.assistencias.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Cálculo das médias a posteriori para primeiros abates por abate
hist(dados.de.primeiros.abates.por.abate_vetor)
summary(dados.de.primeiros.abates.por.abate_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.primeiros.abates.por.abate_vetor)
numero.de.passos=50
media.a.priori_primeiros.abates.por.abate=0
desvio.padrao.a.piori_primeiros.abates.por.abate=0.002
possiveis.desvios.padroes.de.primeiros.abates.por.abate_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.primeiros.abates.por.abate_vetor,passos=numero.de.passos)
desvios.padroes.de.primeiros.abates.por.abate_vetor=dados.de.primeiros.abates.por.abate_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.primeiros.abates.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.primeiros.abates.por.abate_vetor[passo]
  } else{
    desvios.padroes.de.primeiros.abates.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.primeiros.abates.por.abate_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.primeiros.abates.por.abate_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.primeiros.abates.por.abate_vetor^(-2)+desvio.padrao.a.piori_primeiros.abates.por.abate^(-2))^(-1))
medias.a.posteriori.de.primeiros.abates.por.abate_vetor=(desvios.padroes.a.posteriori.de.primeiros.abates.por.abate_vetor^2)*((media.a.priori_primeiros.abates.por.abate*desvio.padrao.a.piori_primeiros.abates.por.abate^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.primeiros.abates.por.abate_vetor*desvios.padroes.de.primeiros.abates.por.abate_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.primeiros.abates.por.abate_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.primeiros.abates.por.abate_vetor,dados.de.primeiros.abates.por.abate_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.primeiros.abates.por.abate_matriz=matrix(medias.a.posteriori.de.primeiros.abates.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.primeiros.abates.por.abate_matriz=matrix(desvios.padroes.a.posteriori.de.primeiros.abates.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Cálculo das médias a posteriori para abates com tiro na cabeça por abate
hist(dados.de.abates.com.tiro.na.cabeca.por.abate_vetor)
summary(dados.de.abates.com.tiro.na.cabeca.por.abate_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.abates.com.tiro.na.cabeca.por.abate_vetor)
numero.de.passos=50
media.a.priori_abates.com.tiro.na.cabeca.por.abate=0
desvio.padrao.a.piori_abates.com.tiro.na.cabeca.por.abate=0.002
possiveis.desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.abates.com.tiro.na.cabeca.por.abate_vetor,passos=numero.de.passos)
desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor=dados.de.abates.com.tiro.na.cabeca.por.abate_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor[passo]
  } else{
    desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor^(-2)+desvio.padrao.a.piori_abates.com.tiro.na.cabeca.por.abate^(-2))^(-1))
medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor=(desvios.padroes.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor^2)*((media.a.priori_abates.com.tiro.na.cabeca.por.abate*desvio.padrao.a.piori_abates.com.tiro.na.cabeca.por.abate^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.abates.com.tiro.na.cabeca.por.abate_vetor*desvios.padroes.de.abates.com.tiro.na.cabeca.por.abate_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor,dados.de.abates.com.tiro.na.cabeca.por.abate_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_matriz=matrix(medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_matriz=matrix(desvios.padroes.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Cálculo das médias a posteriori para dano por rodada
hist(dados.de.dano.por.rodada_vetor)
summary(dados.de.dano.por.rodada_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.dano.por.rodada_vetor)
numero.de.passos=50
media.a.priori_dano.por.rodada=0
desvio.padrao.a.piori_dano.por.rodada=0.75
possiveis.desvios.padroes.de.dano.por.rodada_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.dano.por.rodada_vetor,passos=numero.de.passos)
desvios.padroes.de.dano.por.rodada_vetor=dados.de.dano.por.rodada_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.dano.por.rodada_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.dano.por.rodada_vetor[passo]
  } else{
    desvios.padroes.de.dano.por.rodada_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.dano.por.rodada_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.dano.por.rodada_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.dano.por.rodada_vetor^(-2)+desvio.padrao.a.piori_dano.por.rodada^(-2))^(-1))
medias.a.posteriori.de.dano.por.rodada_vetor=(desvios.padroes.a.posteriori.de.dano.por.rodada_vetor^2)*((media.a.priori_dano.por.rodada*desvio.padrao.a.piori_dano.por.rodada^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.dano.por.rodada_vetor*desvios.padroes.de.dano.por.rodada_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.dano.por.rodada_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.dano.por.rodada_vetor,dados.de.dano.por.rodada_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.dano.por.rodada_matriz=matrix(medias.a.posteriori.de.dano.por.rodada_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.dano.por.rodada_matriz=matrix(desvios.padroes.a.posteriori.de.dano.por.rodada_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Cálculo das médias a posteriori para situações de clutch por rodada
hist(dados.de.situacoes.de.clutch.por.rodada_vetor)
summary(dados.de.situacoes.de.clutch.por.rodada_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.situacoes.de.clutch.por.rodada_vetor)
numero.de.passos=50
media.a.priori_situacoes.de.clutch.por.rodada=0
desvio.padrao.a.piori_situacoes.de.clutch.por.rodada=0.002
possiveis.desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.situacoes.de.clutch.por.rodada_vetor,passos=numero.de.passos)
desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor=dados.de.situacoes.de.clutch.por.rodada_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor[passo]
  } else{
    desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor^(-2)+desvio.padrao.a.piori_situacoes.de.clutch.por.rodada^(-2))^(-1))
medias.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor=(desvios.padroes.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor^2)*((media.a.priori_situacoes.de.clutch.por.rodada*desvio.padrao.a.piori_situacoes.de.clutch.por.rodada^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.situacoes.de.clutch.por.rodada_vetor*desvios.padroes.de.situacoes.de.clutch.por.rodada_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor,dados.de.situacoes.de.clutch.por.rodada_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.situacoes.de.clutch.por.rodada_matriz=matrix(medias.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.situacoes.de.clutch.por.rodada_matriz=matrix(desvios.padroes.a.posteriori.de.situacoes.de.clutch.por.rodada_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Cálculo das médias a posteriori para sucesso em clutch
hist(dados.de.sucesso.em.clutch_vetor)
summary(dados.de.sucesso.em.clutch_vetor)
plot(dados.de.rodadas.com.os.agentes_vetor,dados.de.sucesso.em.clutch_vetor)
numero.de.passos=50
media.a.priori_sucesso.em.clutch=0
desvio.padrao.a.piori_sucesso.em.clutch=0.005
possiveis.desvios.padroes.de.sucesso.em.clutch_vetor=formula.para.gerar.estimativas.de.desvio.padrao(valores=dados.de.sucesso.em.clutch_vetor,passos=numero.de.passos)
desvios.padroes.de.sucesso.em.clutch_vetor=dados.de.sucesso.em.clutch_vetor
for(passo in 1:numero.de.passos){
  if(passo<numero.de.passos){
    desvios.padroes.de.sucesso.em.clutch_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos & dados.de.rodadas.com.os.agentes_vetor<passo*numero.de.passos)]=possiveis.desvios.padroes.de.sucesso.em.clutch_vetor[passo]
  } else{
    desvios.padroes.de.sucesso.em.clutch_vetor[which(dados.de.rodadas.com.os.agentes_vetor>=(passo-1)*numero.de.passos)]=possiveis.desvios.padroes.de.sucesso.em.clutch_vetor[passo]
  }
}
desvios.padroes.a.posteriori.de.sucesso.em.clutch_vetor=sqrt((dados.de.rodadas.com.os.agentes_vetor*desvios.padroes.de.sucesso.em.clutch_vetor^(-2)+desvio.padrao.a.piori_sucesso.em.clutch^(-2))^(-1))
medias.a.posteriori.de.sucesso.em.clutch_vetor=(desvios.padroes.a.posteriori.de.sucesso.em.clutch_vetor^2)*((media.a.priori_sucesso.em.clutch*desvio.padrao.a.piori_sucesso.em.clutch^(-2))+dados.de.rodadas.com.os.agentes_vetor*dados.de.sucesso.em.clutch_vetor*desvios.padroes.de.sucesso.em.clutch_vetor^(-2))
plot(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.sucesso.em.clutch_vetor)
rodadas.e.valores=cbind(dados.de.rodadas.com.os.agentes_vetor,medias.a.posteriori.de.sucesso.em.clutch_vetor,dados.de.sucesso.em.clutch_vetor)
rodadas.e.valores[order(dados.de.rodadas.com.os.agentes_vetor,decreasing=T)[1:10],]
medias.a.posteriori.de.sucesso.em.clutch_matriz=matrix(medias.a.posteriori.de.sucesso.em.clutch_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)
desvios.padroes.a.posteriori.de.sucesso.em.clutch_matriz=matrix(desvios.padroes.a.posteriori.de.sucesso.em.clutch_vetor,nrow=length(dados.de.rodadas.com.os.agentes[,1]),ncol=length(dados.de.rodadas.com.os.agentes[1,]),byrow=F)

#Criação do arquivo com os dados
dados_posterioris_jogadores=dados.dos.jogadores[1]
for(agente in 2:length(agentes)){
  dados_posterioris_jogadores[,(2+13*(agente-2))]=dados.de.rodadas.com.os.agentes_matriz[,agente-1]
  dados_posterioris_jogadores[,(3+13*(agente-2))]=medias.a.posteriori.de.assistencias.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(4+13*(agente-2))]=desvios.padroes.a.posteriori.de.assistencias.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(5+13*(agente-2))]=medias.a.posteriori.de.primeiros.abates.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(6+13*(agente-2))]=desvios.padroes.a.posteriori.de.primeiros.abates.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(7+13*(agente-2))]=medias.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(8+13*(agente-2))]=desvios.padroes.a.posteriori.de.abates.com.tiro.na.cabeca.por.abate_matriz[,agente-1]
  dados_posterioris_jogadores[,(9+13*(agente-2))]=medias.a.posteriori.de.dano.por.rodada_matriz[,agente-1]
  dados_posterioris_jogadores[,(10+13*(agente-2))]=desvios.padroes.a.posteriori.de.dano.por.rodada_matriz[,agente-1]
  dados_posterioris_jogadores[,(11+13*(agente-2))]=medias.a.posteriori.de.situacoes.de.clutch.por.rodada_matriz[,agente-1]
  dados_posterioris_jogadores[,(12+13*(agente-2))]=desvios.padroes.a.posteriori.de.situacoes.de.clutch.por.rodada_matriz[,agente-1]
  dados_posterioris_jogadores[,(13+13*(agente-2))]=medias.a.posteriori.de.sucesso.em.clutch_matriz[,agente-1]
  dados_posterioris_jogadores[,(14+13*(agente-2))]=desvios.padroes.a.posteriori.de.sucesso.em.clutch_matriz[,agente-1]
  colnames(dados_posterioris_jogadores)[(2+13*(agente-2)):(14+13*(agente-2))]=paste(c('Rodadas','Médias a posteriori de assistências por abate acima do esperado','Desvios padrões a posteriori de assistências por abate acima do esperado','Médias a posteriori de primeiros abates por abate acima do esperado','Desvios padrões a posteriori de primeiros abates por abate acima do esperado','Médias a posteriori de abates com tiro na cabeça por abate acima do esperado','Desvios padrões a posteriori de abates com tiro na cabeça por abate acima do esperado','Médias a posteriori de dano por rodada acima do esperado','Desvios padrões a posteriori de dano por rodada acima do esperado','Médias a posteriori de situações de clutch por rodada acima do esperado','Desvios padrões a posteriori de situações de clutch por rodada acima do esperado','Médias a posteriori de sucesso em clutch acima do esperado','Desvios padrões a posteriori de sucesso em clutch acima do esperado'),agentes[agente],sep=' - ')
}
write.csv2(dados_posterioris_jogadores,'Médias ponderadas a posteriori acima do esperado para cada jogador de acordo com região, agente e mapa.csv')