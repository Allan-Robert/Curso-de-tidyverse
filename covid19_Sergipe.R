##### DAdos de Covid-19 do Estado de Sergipe #######


library(tidyverse)

# usando pacote do github para obtenção de dados
# usando função do pacote remotes
remotes::install_github("liibre/coronabr")
library(coronabr)

# Exemplo baixando dados do Ministério da Saúde especificamente de Sergipe
# baindo dados do Ministério da Saúde : https://covid.saude.gov.br/
covid_sergipe <- get_corona_minsaude(uf="SE") # ou se

#outros banco de dados são disponibilizados pelo pacote
# por exemplo os dados das secretarias estaduais https://brasil.io/
## mais detalhes no link : https://liibre.github.io/coronabr/articles/coronabr.html


## Usando dados de Sergipe até 16/10/20
library(readxl)
covid_sergipe <- read_excel("Covid_SE_16_10_ms.xlsx")


### Note que o formato da data não é bem compatível
class(covid_sergipe$data)

### Ajustando a variável data usando o pacote lubridate
library(lubridate)
## Indicar a ordem da data em inglês Y-year , m- month d-day
covid_sergipe$data <- as.Date(parse_date_time(covid_sergipe$data, "%y/%m/%d"))

## Filtrando dados somente da capital Aracaju
cov19Aju=covid_sergipe %>% filter(municipio=="Aracaju")



# gráfico de novos casos
g1=ggplot(cov19Aju,aes(x=data,y=casosAcumulado))+
  geom_line()+
  labs(x="",y="Casos Acumulados")
g1

# renomeando as variáveis:
#obitosNovos para Óbitos e obitosAcumulado para Óbitos Acumulados 
cov19Aju=cov19Aju %>% rename(Óbitos='obitosNovos')
cov19Aju=cov19Aju %>% rename('Óbitos Acumulados'='obitosAcumulado')


# gráfico de novos casos
gm=ggplot(cov19Aju,aes(x=data,y=`Óbitos`))+
  geom_line()+
  labs(x="")
gm


#### Valores negativos? 
## Foram digitados erronamente? ou um tipo de correção?
## Solução ideal é entrar em contato com órgão oficial
## Aqui vamos supor que foi digitado incorretamente o sinal negativo

## Danto um zoom nos dados entre 20 e 28 de setembro nos dados
cov19Aju %>% select(data,Óbitos) %>%
  filter(data>=as.Date("2020-09-20") & data<=as.Date("2020-09-28"))


cov19Aju$Óbitos[cov19Aju$data==as.Date("2020-09-21")]=3
cov19Aju$Óbitos[cov19Aju$data==as.Date("2020-09-22")]=2
# Refazendo o gráfico 

gm # desatualizado

gm=ggplot(cov19Aju,aes(x=data,y=`Óbitos`))+
  geom_line()+
  labs(x="")
gm # agora sim


library(zoo) # pacote com função para médias móveis

# Criando a variável Média Movel
cov19Aju=cov19Aju %>%
  mutate('Média Móvel'=rollapply(Óbitos,7,mean,align='right',fill=NA))


## Agrupando para termos categorias de Médias Móvel e Óbitos no mesmo gráfico
### Deixando em outro conjunto cov19Aju2
cov19Aju2=cov19Aju %>%
  gather(c('Óbitos','Média Móvel'),key="Séries", value="Valor")


gm=ggplot(cov19Aju2,aes(x=data,y=Valor,fill=Séries, colour=Séries))+
  geom_line(size=1.1)+
  labs(x="",y="Número de obitos")
gm

### Alterando o tema do seu jeito
gm2=gm +
theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
 # theme(legend.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.key = element_rect(fill = "white", colour = "white")) +
    theme(legend.position = c(0.2, 0.8))

gm2

### Outro formato comum barras e colunas 

ggplot(cov19Aju) + 
  geom_col(aes(x = data, y = Óbitos), color = "yellow", fill = "yellow") +
  geom_line(aes(x = data, y = `Média Móvel`), size = 1.1, color="red", group = 1)


# Customizando gráfico de casos acumulados com temas básicos prontos
# usando pacote gridExtra para multiplos gráficos na mesma saída
library(gridExtra)

g2= gm + theme_bw()
g3= gm + theme_classic()
g4= gm + theme_dark()
g5= gm + theme_gray()

grid.arrange(g2, g3, g4, g5)

# usando temas personalizados com o pacotes ggthemes
library(ggthemes)
g2= gm + theme_excel()
g3= gm + theme_stata()
g4= gm + theme_pander()
g5= gm + theme_wsj()

grid.arrange(g2, g3, g4, g5)


## aternativo ao grid temos o pacote patchwork
### Básico: lateral | e abaixo / com organização por parenteses
library(patchwork)
(g2|g3)/(g4|g5)
## outra forma de visualizar
(g2|g3)/(g4)
## outra forma de visualizar
(g2/g3)|(g4)
# alternativamente
g2/(g3|g4)/g5

# outras opções ver link abaixo
# https://www.datanovia.com/en/blog/ggplot-multiple-plots-made-ridiculuous-simple-using-patchwork-r-package/

# Usando função date_format do pacote scale

library(scales)

# rodar novamente g1 para atualizá-lo em formatos de data
g1+ scale_x_date(labels=date_format("%b-%y")) # mostrar mês (nome abreviado) - ano
gm+ scale_x_date(labels=date_format("%d/%m")) # mostrar dia / mês númerico
gm+ scale_x_date(date_breaks = "1 week", date_labels = "%W") # Mostrar semana do ano
gm2 + scale_x_date(labels=date_format("%b-%y"))


### Trabalhando agora com todos municípios sergipanos
### Obtendo shapefiles diretamento com o pacote geobr
library(geobr)

muni <- read_municipality(code_muni= "SE", year=2017)
#meso=read_meso_region(code_meso= "SE", year=2017) 
#micro=read_micro_region(code_micro= "SE", year=2017)
plot(muni)

# Malha municipal de Sergipe
ggplot() +
  geom_sf(data=muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipios de Sergipe", size=8) +
  theme_minimal()

#  filtrando o dia 14/10/20 
cvu=covid_sergipe %>% filter(data=="2020-10-14")
cvu=na.omit(cvu) # retirar linhas referentes a todo estado


##### Próximo passo juntar informações junto a malha.
#### Geralmente usando codigo

##### obs cod de municipio diferente dos dados de Covid (ultimo número a menos)
#### Solução estrair o último número do  code_muni do shapefile irá funcionar
#### como fazer isso? usando substr do pacote stringr
muni$code_muni=substr(muni$code_muni,1,6)
# retornando a ser uma variável do tipo número (precisa ser do mesmo tipo dos dados)
muni$code_muni=as.numeric(muni$code_muni)
cvu$codmun=as.numeric(cvu$codmun)

# criando nova malha juntando os dados.
muni2 <-left_join(muni,cvu,by=c("code_muni"="codmun"))

library(RColorBrewer) # paletas de cores

ggplot() +
  geom_sf(data=muni2, aes(fill=casosAcumulado), size=.15) +
  labs(subtitle="Casos Acumulados de Covid-19 em Sergipe até 16/08/2020", size=8) +
  scale_fill_distiller( palette="Blues",name="Casos Ac",  trans="reverse" ) +
  theme_minimal()


library(RColorBrewer) # paletas de cores
display.brewer.all() # para ver opções
ggplot() +
  geom_sf(data=muni2,aes(fill=casosAcumulado), size=.15) +
  labs(title="Casos Acumulados de Covid-19 em Sergipe",
       subtitle=" Entre 27/03 a 16/08",size=8) +
  scale_fill_distiller(palette="Spectral",name="Casos Ac") +
  theme_minimal()


### Adicionando nomes dos municípios (caso particular usando pacote geobr)
##OBS acesso complicado as latitudes e longitudes deste pacote

muni.labs <- muni2 %>%
  group_by(name_muni) %>%
  summarise(long = mean(geom[[1]][[1]][[1]][,1]),
            lat = mean(geom[[1]][[1]][[1]][,2]))


ggplot() +
  geom_sf(data=muni2,aes(fill=casosAcumulado), size=.15) +
  labs(title="Casos Acumulados de Covid-19 em Sergipe",
       subtitle=" Entre 27/03 a 09/09",size=8) +
  scale_fill_distiller(palette="Spectral",name="Casos Ac") +
  theme_minimal() +
  geom_text(data=muni.labs, aes(long, lat, label = name_muni), size=2.5)

## Problema : Muito texto em cima do outro. Solução mostrar somente as de mais casos

### Vetor somente com municípios de mais casos
mc=c("Aracaju", "Nossa Senhora Do Socorro", "Itabaiana", "Lagarto",
     "Estância", "São Cristóvão")
# Filtrando por estes municípios
muni.labs2=muni.labs %>% filter( name_muni %in% mc)
# forçando nome Aracaju para mais baixo para não coincidir com nome de São Cristóvão
muni.labs2[1,3]=-11.02

### Acrescentar escala e simbolo de direção norte usando ggspatial

ggplot() +
  geom_sf(data=muni2,aes(fill=casosAcumulado), size=.15) +
  labs(title="Casos Acumulados de Covid-19 em Sergipe",
       subtitle=" Entre 27/03 a 09/09",size=8) +
  scale_fill_distiller(palette="Spectral",name="Casos Ac") +
  theme_minimal() +
  geom_text(data=muni.labs2, aes(long, lat, label = name_muni), size=3)+
  ggspatial::annotation_scale(location="tr")+
  ggspatial::annotation_north_arrow(which_north = "true",location="br")
# tr abreviação top right (combinar primeira letra de left, top, right e  bottom)




########## Animação de acumulados ao longo do tempo
library(gganimate)

cvs=covid_sergipe
cvs=na.omit(cvs) # retirando linhas de todo estado

cvs$codmun=as.numeric(cvs$codmun)
# juntando dados (precisam ser do mesmo tipo)

muni3 <-inner_join(muni,cvs,by=c("code_muni"="codmun"))
dim(muni3)


a=ggplot() +
  geom_sf(data=muni3, aes(fill=casosAcumulado), size=.15) +
  labs(title="Óbitos - Covid-19 em Sergipe", subtitle = 'Data: {current_frame}') +
  scale_fill_distiller(palette="Spectral",name="Casos Ac") +
  theme_minimal() +
  transition_manual(data)

a


############## Usando informações das Secretarias de saude dos estados via Brasil.io ####

# read_csv é uma função do readr que reconhece formato data para o R. 
covid_SE_16_10_io <- read_csv("covid_SE_16_10_io.csv")

serg= covid_SE_16_10_io %>% filter(city_ibge_code!=28) # 28 refere a todo estado


# note que aqui o código do município está correto com o IBGE e o pacote geobr
# Chamar novamento a malha municipal de sergipe para retornar ao original
muni <- read_municipality(code_muni= "SE", year=2017)

muni4 <-inner_join(muni,serg,by=c("code_muni"="city_ibge_code"))

muni4 %>% filter(date=="2020-10-14") %>%
  ggplot() +
  geom_sf(aes(fill=confirmed_per_100k_inhabitants), size=.15) +
  labs(title="Casos confirmados Covid-19 em Sergipe/ 1000.000 habitantes",
       subtitle=" Em  15/10",size=8) +
  scale_fill_distiller(palette="Spectral",name="Casos/100k") +
  theme_minimal() +
  ggspatial::annotation_scale(location="tr")+
  ggspatial::annotation_north_arrow(which_north = "true",location="br")


#### Animação da Evolução de casos por 100.000

a2=muni4 %>% ggplot() +
  geom_sf(aes(fill=confirmed_per_100k_inhabitants), size=.15) +
  labs(title="Casos confirmados Covid-19 em Sergipe/ 1000.000 habitantes",
       subtitle = 'Data: {current_frame}') +
  scale_fill_distiller(palette="Spectral",name="Casos/100k") +
  theme_minimal()+
  transition_manual(date) # frames

# controlando velocidade (parece rápido demais)
animate(a2, fps=3) # frames por segundo


############# Dados do Governo de Sergipe que apresentam dados de todo estado
######## com informações outras informações disponibilizado via site:
#### https://todoscontraocorona.net.br/

### Informações inicialmente constam em 01/04 com nomes inciando na linha a20 até q182



cgov <- read_excel("covid_SE_16_10_gov.xlsx",
                   range = "A20:q219", col_types = c("date",
                  "numeric", "numeric", "numeric", "numeric",
                  "numeric", "numeric","numeric", "numeric",
                  "numeric", "numeric", "numeric", "numeric",
                  "numeric", "numeric", "numeric", "numeric"))

#### Construindo um painel com informações interessantes

#### Juntando comandos de manipulação e comandos gráficos.

### Evolução dos casos pelo sexo usando geom area

a1=cgov %>% select(DATA,MASCULINO,FEMININO) %>%
  gather(key="Sexo",value="Infectados",-DATA) %>%
ggplot(aes(x=DATA, y=Infectados, fill=Sexo)) +
  geom_area() +labs(x="")+theme_bw()

a1
# problema entre 18 e 26 de julho algum valor esta errado na base de dados.
# Identificar

cgov %>% select(DATA,'TESTES CONFIRMADOS',MASCULINO,FEMININO) %>%
  filter(DATA>=as.Date("2020-07-17") & DATA<=as.Date("2020-07-28"))

### Problema no dia 24/07
cgov$FEMININO[cgov$DATA==as.Date("2020-07-24")] = 51132-22794
## Agora pode refazer o gráfico

### Novo gráfico com número de leitos disponíveis para covid
cores=c("#7CAE00","#F8766D") # alterando padrão de cores


a2=cgov %>% select(DATA,`Nº DE LEITOS UTI`,`Nº DE LEITOS  ENFERMARIA`) %>%
  rename(Enfermaria=`Nº DE LEITOS  ENFERMARIA`,UTI=`Nº DE LEITOS UTI`) %>%
  gather(key="Tipo",value="Leitos",-DATA) %>%
  ggplot(aes(x=DATA, y=Leitos, colour=Tipo)) +
  scale_colour_manual(values=cores) +
  geom_line() +labs(x="",y="Nº de Leitos")+theme_bw()


### Novo gráfico com taxa de ocupação nos leitos

# Taxa de ocupação em UTI e emfermarias
# Para UTI ((UTI SUS + UTI PRIVADO) /Nº DE LEITOS UTI))*100
# Para Enf ((ENFERMARIA SUS + ENFERMARIA PRIVADO) /Nº DE LEITOS  ENFERMARIA))*100

## Criando as variáveis permanentementes de Taxas de Ocupação
cgov=cgov %>% mutate(Taxa_UTI=((`UTI SUS` + `UTI PRIVADO`)/`Nº DE LEITOS UTI`)*100,
 Taxa_ENF=((`ENFERMARIA SUS` + `ENFERMARIA PRIVADO`)/`Nº DE LEITOS  ENFERMARIA`)*100)

# plotando as taxas no mesmo gráfico
cores=c("#7CAE00","#F8766D") # padrao de cores

a3=cgov %>% select(DATA,Taxa_ENF,Taxa_UTI) %>%
  rename(Enfermaria=Taxa_ENF,UTI=Taxa_UTI) %>% # nenomeando pro causa do gráfico
  gather(key="Tipo",value="Taxa",-DATA) %>%
    ggplot(aes(x=DATA, y=Taxa, colour=Tipo)) +
  scale_colour_manual(values=cores) +
  geom_line() +labs(x="",y="Taxa de Ocupação")+theme_bw()


library(patchwork)
(a2/a3)|a1

################
###############
### Sobre chaveamento

tiba= tibble(x1 = 1:3, y = rnorm(3)) 
tibb= tibble(x1 = c(1,2,4), z = rnorm(3)) 

tiba %>% inner_join(tibb, by = "x1")
tiba %>% left_join(tibb, by = "x1")
tiba %>% right_join(tibb, by = "x1")
tiba %>% full_join(tibb, by = "x1")
