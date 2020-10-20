### Dados Eleitorais

## Auxílio do pacote electionsBR

#install.packages("electionsBR")
library(electionsBR)
library(tidyverse)


# Ambito federal = "fed"
# Neste vota-se para Dep. Federal, Dep. Estatudal,
##Senador 1ª Vaga, Senador 2ª Vaga,  Governador e Presidente

# Ambito Municipal = "local"
# Neste vota-se para prefeito e vereador

########################## Começaremos com a local de 2016 #####
###  Eleições 2016
##### Candidatos a vereador por partido nos municípios da Bahia?

## Opção 1 pegar todos e depois filtrar  (demora mais para baixar)

# Candidatos dos municipais da Bahia eleição de 2016.
mun_BA <- candidate_local(2016, uf="BA")

### Erro extrair variáveis com nome NA
mun_BA=mun_BA[,1:58] # De 59 até 62 tem nomeação da variável como faltante

#### Salvando dados.
write.table(mun_Ba, "Cand_Locais_Bahia2016.txt")

### Relendo o que está salvo
mun_BA <- read.csv("Cand_Locais_Bahia2016.txt", sep="")

mun_BA=as_tibble(mun_BA)

#### Focando nos candidatos a Vereador em 2016.
### Distribuição de Candadidatos a vereador por partido na Bahia em 2016?

## opção 1 obtendo filtrando para vereador, agrupando por partido e
## visualizando Totais com geom_col

VPP=mun_BA %>% group_by(SIGLA_PARTIDO) %>% select(SIGLA_PARTIDO,DESCRICAO_CARGO) %>%
  filter(DESCRICAO_CARGO=="VEREADOR") %>%
  summarise(Total=n()) %>% arrange(desc(Total))

## vendo a grosso modo
ggplot(VPP,aes(x=SIGLA_PARTIDO,y=Total))+geom_col()


## melhorando textos das cooredenadas. Ordenando com reorder no qual o 
### sinal - significa ordem crescente (sem sinal significa ordem decrescente)
#### Devido ao grande número de partidos o ideal e girar o gráfico com coord_flip
#### Acrescentando total as barras

ggplot(VPP,aes(x=reorder(SIGLA_PARTIDO,-Total),y=Total))+geom_col() +
    labs(x="Partidos",y="Total de Candidatos",
         title="Candidatos a Vereador por Partido na Bahia em 2016")+
coord_flip() + # alterando coordenadas x e y
geom_text(aes(label = Total), hjust = -0.1) # Acrescentando total as barras

######### Outra opção usando função fct_infreq() do forcats ao inves do reorder

## opção 2 filtrando para Vereador, contando por partido e visualizando com geom_bar

VPP2=mun_BA %>%  filter(DESCRICAO_CARGO=="VEREADOR") %>% count(SIGLA_PARTIDO)

ggplot(VPP2,aes(x=reorder(SIGLA_PARTIDO,n),y=n))+geom_bar(stat="identity") +
    labs(x="Partidos",y="Total de Candidatos",
       title="Candidatos a Vereador por Partido na Bahia em 2016")+
  coord_flip()+  # alterando coordenadas x e y
geom_text(aes(label = n), hjust = -0.1) # Acrescentando total as barras



###### Foco na distribuição de candidatos por Raça e Sexo


sr=mun_BA %>%  group_by(DESCRICAO_SEXO,DESCRICAO_COR_RACA) %>% # ordem agrupamento
  select(DESCRICAO_COR_RACA,DESCRICAO_SEXO) %>%
  summarise(n = n())%>%
  mutate(perc = n %>% prop.table %>% round(4))

ggplot(sr, aes(x=reorder(DESCRICAO_COR_RACA, perc),y=perc,fill=DESCRICAO_COR_RACA))+ geom_col()+
  geom_text(aes(label = perc), vjust = -0.3)+
  facet_wrap(~DESCRICAO_SEXO)+ ### Camada de Faces 
  theme(legend.position = "none")

#### Candidatos por raça e sexo (nesta ordem)
sr=mun_BA %>%  group_by(DESCRICAO_COR_RACA,DESCRICAO_SEXO) %>%
  select(DESCRICAO_COR_RACA,DESCRICAO_SEXO) %>%
  summarise(n = n())%>%
  mutate(perc = round(prop.table(n),3))

ggplot(sr, aes(x=DESCRICAO_SEXO, y=perc,fill=DESCRICAO_SEXO))+ geom_col()+
  geom_text(aes(label = perc), vjust = -0.3)+
  facet_wrap(~DESCRICAO_COR_RACA)+
  theme(legend.position = "none")


### Alterar Cores? SIM
# ver cores padrão do ggplot
library(scales)
show_col(hue_pal()(2)) # ao usar 2 cores no gráfico
show_col(hue_pal()(4)) # ao usar 4 cores

#  alterando manualmente

cores=c("#FF3399","#33FFFF")
# ver por exemplo link http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

ggplot(sr, aes(x=DESCRICAO_SEXO, y=perc,fill=DESCRICAO_SEXO))+ geom_col()+
  geom_text(aes(label = perc), vjust = -0.3)+
  scale_fill_manual(values=cores)+
  facet_wrap(~DESCRICAO_COR_RACA)+
  theme(legend.position = "none")

########## Resultados da Eleição na Bahia em 2016

##### Votos da Eleição para prefeitos na Bahia em 2016?

############### Eleitos e seus Partidos nos municípios Baianos

veBA=vote_mun_zone_local(2016, uf = "BA")

write.table(veBA,"Elei_Locais_Bahia2016.txt")

veBA <- read.csv("Elei_Locais_Bahia2016.txt", sep="")
veBA=as_tibble(veBA)
veBA
############### Qual é o Partido dos Prefeitos dos municípios bahianos?
############### Especificamente para o primeiro turno
Prp1=veBA %>%
  filter(DESCRICAO_CARGO=="Prefeito" & NUM_TURNO==1) %>%
  select(NUMERO_ZONA,DESCRICAO_CARGO,NOME_MUNICIPIO,
         SIGLA_PARTIDO,TOTAL_VOTOS) %>%
  group_by(NOME_MUNICIPIO,SIGLA_PARTIDO) %>% # agrupar por municipio e partido
       summarise(TOTAL_VOTOS=sum(TOTAL_VOTOS)) %>%
    filter(TOTAL_VOTOS==max(TOTAL_VOTOS))

##### Para o último turno (Se tiver tido o 2º que seja ele, se não fica o 1º) 
#### OBS antes vamos ver quantos e quais municípios tiveram 2 turno

Prp2=veBA %>%
  filter(DESCRICAO_CARGO=="Prefeito" & NUM_TURNO==2) %>%
  select(NUMERO_ZONA,DESCRICAO_CARGO,NOME_MUNICIPIO,
         SIGLA_PARTIDO,TOTAL_VOTOS) %>%
  group_by(NOME_MUNICIPIO,SIGLA_PARTIDO) %>% # agrupar por municipio e partido
  summarise(TOTAL_VOTOS=sum(TOTAL_VOTOS)) %>%
  filter(TOTAL_VOTOS==max(TOTAL_VOTOS))

#### OBS: Somente Vitória da Conquista teve segundo turno

### Juntando os dois dados considerando o resultado final
Prp = Prp1 %>% filter(NOME_MUNICIPIO!="VITÓRIA DA CONQUISTA") %>% 
  bind_rows(Prp2) # Acrescentar por linha o que está no Prp2


## Olhando agora no "Prp"  

# ver, por exemplo, resultado de Salvador ACM Neto eleito
Prp %>% filter(NOME_MUNICIPIO=="SALVADOR")

### grafico de total de partidos eleitos nos municípios


### Exercícios

### 1) fazer um gráfico ordenado do (do maior para o menor)
### dos partidos eleitos

Prp %>% group_by(SIGLA_PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(SIGLA_PARTIDO,n),y=n))+geom_col()+
  coord_flip()
### Exercício: Melhorar este gráfico com nomes, cores, etc.

### 2) Mapa com sigla do partido associado a cada município

#############################################################################
####### Eleiçãos nível partidos e municícios da Bahia em 2018 #####
# base de dados extraída diretamente do repositório de dados Eleitorais:
# http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

library(tibble)

vp2018=as_tibble(votacao_partido_munzona_2018_BA) # formato alternativo de dataframe
table(vp2018$DS_CARGO)

## Votação partidária para governador por município
## Em quantos municípicios

# Partido com maior votação para governador foi o mais votado em quantos municípios?

### Votação total por partido ao Governador
vt=vp2018 %>% filter(DS_CARGO=="Governador") %>% group_by(NM_PARTIDO) %>%
  summarise(QT_VOTOS_NOMINAIS=sum(QT_VOTOS_NOMINAIS))

### Maior votação por partido em cada município ao Governador

vpm=vp2018 %>% filter(DS_CARGO=="Governador") %>% group_by(CD_MUNICIPIO,NM_MUNICIPIO,NM_PARTIDO) %>%
  summarise(QT_VOTOS_NOMINAIS=sum(QT_VOTOS_NOMINAIS)) %>%
  filter(QT_VOTOS_NOMINAIS==max(QT_VOTOS_NOMINAIS)) %>%
  arrange(NM_MUNICIPIO)

# Obtendo o total de municípios em que os partidos tiveram mais votos a governador
vpm %>% group_by(NM_PARTIDO) %>% summarise(n=n())

####  plotando no mapa este resultado
library(geobr)

muni <- read_municipality(code_muni= "BA", year=2017)

#### Problema: Codigos diferentes dos municípios. Pode usar gerar outros problemas
## Problemas 1 Uma esta minusculo e outro em maísculo
## Solução tornar todas maiúsculas os nomes no shapefile

# comando do pacote stringr para todas letras em maísculas
muni$name_muni=muni$name_muni %>% str_to_upper()

muni2 <-left_join(muni,vpm,by=c("name_muni"="NM_MUNICIPIO"))



### Erro um é fator e outro é caractere
#### Solução possivel tornar a variavel nome em vpm como caractere

vpm$NM_MUNICIPIO= as.character(vpm$NM_MUNICIPIO)

muni2 <-left_join(muni,vpm,by=c("name_muni"="NM_MUNICIPIO"))
### Agora vai....

ggplot() +
  geom_sf(data=muni2, aes(fill=NM_PARTIDO), size=.15) +
  labs(subtitle="Partidos com mais votos para Governador na Bahia em 2018", size=8) +
    theme_minimal()

### Porque NA??? Provavelmente não houve reconhecimento de nomes das cidades
### entre os dois conjuntos de dados

### Verificando a igualdade entre estas variável usando all_equal
all_equal(vpm$NM_MUNICIPIO,muni$name_muni)

### Ordenando os dois por nome para comparação em pares

muni=muni %>% arrange(name_muni)
vpm=vpm %>% arrange(NM_MUNICIPIO)

### Conferindo indices com diferença usando o comando which
which(muni$name_muni!=vpm$NM_MUNICIPIO)

### Olhando estes elementos

vpm$NM_MUNICIPIO[63];muni$name_muni[63] # Problema de acentuaçao
### O correto é o do shapefile
vpm$NM_MUNICIPIO[63]=muni$name_muni[63]

vpm$NM_MUNICIPIO[63];muni$name_muni[69] # Acentuação ou Letra N
### EScreve-se o dois optar pelo do shapefile
vpm$NM_MUNICIPIO[69]=muni$name_muni[69]

vpm$NM_MUNICIPIO[119];muni$name_muni[119] # problema no simbolo (')
## Do shapefile esta correto
vpm$NM_MUNICIPIO[119]=muni$name_muni[119]

vpm$NM_MUNICIPIO[318];muni$name_muni[318] #  QUI ou QUIN
## Do shapefile esta correto
vpm$NM_MUNICIPIO[318]=muni$name_muni[318]

vpm$NM_MUNICIPIO[351];muni$name_muni[351] # problema de acentuação
## Do shapefile esta correto
vpm$NM_MUNICIPIO[351]=muni$name_muni[351]


# Naturalmente estão iguais agora
all_equal(vpm$NM_MUNICIPIO,muni$name_muni)


# chaveando os dados novamente
muni2 <-left_join(muni,vpm,by=c("name_muni"="NM_MUNICIPIO"))

########### Refazendo o mapa

ggplot() +
  geom_sf(data=muni2, aes(fill=NM_PARTIDO), size=.15) +
  labs(subtitle="Partidos com mais votos para Governador na Bahia em 2018", size=8) +
  theme_minimal()


