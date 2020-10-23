
#### Histórico de fontes de energia do Brasil pela ANEEL (2000 a 2019)
### Disponível em https://www.aneel.gov.br/dados/relatorios

library(tidyverse)

## Dados disponíveis até junho de 2019
gfb= read_csv("GeracaoFonte.csv", 
              col_types = cols(mdaEnergiaDespachadaGWh = col_number()))

### Renomiando mda

gfb=gfb %>% rename(Total=mdaEnergiaDespachadaGWh) 


# Transformando NA em zeros 
gfb= gfb %>% replace_na(list(Total = 0)) 


## Criando uma variável em formato data com lubridate
library(lubridate)

data=paste(gfb$anoReferencia,gfb$mesReferencia,sep="-")
gfb$data = as.Date(parse_date_time( data, "%y/%m"))


### Retirando o ano de 2019 por ser incompleto

gfb=gfb %>% filter(anoReferencia!=2019) 

## Obter o total anual por fonte
taf=gfb %>% group_by(anoReferencia,nomFonteGeracao) %>% 
    summarise(Total=sum(Total))

### Calculando a proporção ao ano
#### Agrupando o totalAnual dos dados de "ta" ao gfb

### Primeiro a soma anual
sa=taf %>% group_by(anoReferencia) %>% summarise(Total_Anual=sum(Total))

taf=left_join(taf,sa,by=("anoReferencia"))

taf=taf %>% mutate(Prop=(Total/Total_Anual) %>% round(3))

### Observar o primeiro (2000) e o ultimo (2018) como evoluiu

anos=c(2000,2018)
taf %>% filter(anoReferencia %in% anos) %>% 
  ggplot(aes(x=reorder(nomFonteGeracao,Prop),y=Prop)) +
  geom_col(aes(fill=nomFonteGeracao))+
  coord_flip()+
  theme(legend.position="none")+
  facet_wrap(~anoReferencia)
  


### Evolução mostrada por Área - Total

ggplot(taf,aes(x=anoReferencia, y=Total, fill=nomFonteGeracao)) +
  geom_area() +labs(x="")+theme_bw()

### Evolução mostrada por Área - Proporção

ggplot(taf,aes(x=anoReferencia, y=Prop, fill=nomFonteGeracao)) +
  geom_area() +labs(x="")+theme_bw()

# Nota-se aumento de energia eólica e de gas natural a partir de 2010


### Animação de evolução

library(gganimate)

## Transformando o ano em Data para fazer animação
library(lubridate)
taf$anoReferencia = as.Date(parse_date_time(taf$anoReferencia, "%y"))

A1=ggplot(taf,aes(x=reorder(nomFonteGeracao,Prop),y=Prop)) +
  labs(title="Evolução de Fontes de Energia do Brasil", 
  subtitle = 'Data: {current_frame}') +  
  geom_col(aes(fill=nomFonteGeracao))+
  coord_flip()+
  theme(legend.position="none") +
  transition_manual(anoReferencia)
  
  ### Alterando velocidade 

  animate(A1, fps=3)


  