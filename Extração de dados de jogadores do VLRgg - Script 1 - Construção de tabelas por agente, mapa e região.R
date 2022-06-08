#Preâmbulo
library(XML)
library(tidyverse)
library(rvest)
library(robotstxt)

#Checagem de se o vlr.gg permite que seus dados sejam extraídos
paths_allowed(
  paths = c('https://www.vlr.gg/stats')
)
#Como o resultado foi positivo, continua-se com o scrapping

#Radical para as URLs
radical='https://www.vlr.gg/stats/?event_group_id=all&event_id=all'

#Possíveis valores para região dos jogadores
regioes=c('North America','Europe','Asia-Pacific','Japan','Latin America','Oceania','MENA','Game Changers')
regioes_codigos_url=c('na','eu','ap','jp','sa','oce','mn','gc')

#Possíveis valores para agentes jogados
agentes=c("Astra","Breach","Brimstone","Chamber","Cypher","Jett","Kay/O","Killjoy","Neon","Omen","Phoenix","Raze","Reyna","Sage","Skye","Sova","Viper","Yoru")
agentes_codigos_url=c("astra","breach","brimstone","chamber","cypher","jett","kayo","killjoy","neon","omen","phoenix","raze","reyna","sage","skye","sova","viper","yoru")

#Possíveis valores para mapas jogados
mapas=c('Ascent','Bind','Breeze','Haven','Icebox','Split','Fracture')
mapas_codigos_url=c('5','1','8','2','6','3','9')

#Dados de jogadores por agente, mapa e região
#Primeiro, usa-se a primeira combinação de valores para gerar a primeira
#tabela, que será unida às outras
regiao=1
agente=1
mapa=1
url.dos.dados=paste0(radical,'&region=',regioes_codigos_url[regiao],'&country=all&min_rounds=0&min_rating=0&agent=',agentes_codigos_url[agente],'&map_id=',mapas_codigos_url[mapa],'&timespan=all')
tabela.da.url=url.dos.dados %>%
  read_html() %>% 
  html_nodes('table') %>% 
  html_table(fill=T)
tabela.da.url_como.data.frame=as.data.frame(tabela.da.url)
tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,regioes[regiao])
colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Region'
tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,agentes[agente])
colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Agent'
tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,mapas[mapa])
colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Map'
tabela.completa=tabela.da.url_como.data.frame
contador=0
for(regiao in 1:length(regioes)){
  for(agente in 1:length(agentes)){
    for(mapa in 1:length(mapas)){
      if(regiao!=1 | agente!=1 | mapa!=1){
        url.dos.dados=paste0(radical,'&region=',regioes_codigos_url[regiao],'&country=all&min_rounds=0&min_rating=0&agent=',agentes_codigos_url[agente],'&map_id=',mapas_codigos_url[mapa],'&timespan=all')
        tabela.da.url=url.dos.dados %>%
          read_html() %>% 
          html_nodes('table') %>% 
          html_table(fill=T)
        tabela.da.url_como.data.frame=as.data.frame(tabela.da.url)
        if(length(tabela.da.url_como.data.frame[,1])!=0){
          tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,regioes[regiao])
          colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Region'
          tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,agentes[agente])
          colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Agent'
          tabela.da.url_como.data.frame=cbind(tabela.da.url_como.data.frame,mapas[mapa])
          colnames(tabela.da.url_como.data.frame)[length(tabela.da.url_como.data.frame[1,])]='Map'
          tabela.completa=rbind(tabela.completa,tabela.da.url_como.data.frame)
        }
        #Número atual da iteração
        contador=contador+1
        cat('Iteração - ',contador,' - Região - ',regioes[regiao],' - Agente - ',agentes[agente],' - Mapa - ',mapas[mapa],'\n')
      }
    }
  }
}

#Definição da pasta usada no computador pessoal
setwd('C:\\Users\\arviz\\Dropbox\\André\\Valorant\\Estatísticas')

#Criação do csv com os dados
write.csv2(tabela.completa,'Dados de jogadores por região, agente e mapa.csv')