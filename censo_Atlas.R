# Dados do Censo

# Obtidos via Atlas do Desenvolvimento Humano
# link http://atlasbrasil.org.br/2013/pt/consulta/

### Trabalhar com faces ou facetas e agregate
library(tidyverse)
library(readxl)
censo <- read_excel("AtlasBrasil_Estados.xlsx")
View(censo)

# Extrair os dados gerais do Brasil (Focar só nos estados)
censo=censo %>% filter(Espacialidades!="Brasil")

#### Problema: Reorganizar dados por Ano em linhas.
## Possiveis soluções: Primeira ideia gather
### Ideia da função gather

c2= censo[,1:4] %>% gather(starts_with('Taxa de des'),key="Ano",
                           value="Taxa de Desocupação - 18 ou mais")

#### Problema: não é pratico para mais de uma variável.
#### Para multiplos ideal usar funçao melt pacote data.table

library(data.table)

censo=melt(setDT(censo), id=1:2,
            measure=patterns("^Taxa de d", "^% da popu",
                            "^Esper","^Pop","^Mor",
                            "^ID","^Taxa de a","^Rend"),
     value.name=names(censo)[seq(3,18,2)], variable.name="Ano")



# voltar a ser uma tibble
censo2=as_tibble(censo)

# Identificar corretamente a variável ano
censo2$Ano=factor(censo2$Ano,labels=c(2000,2010))

# Renomear as variavéis combinando funções str_sub e srt_length

names(censo2)[4:11]=str_sub(names(censo2)[4:11],start=1,end=(str_length(names(censo2)[4:11])-5))

##### Pronto agora filtrar

  ggplot(censo2,
         aes(x=IDHM,
             y=`Renda per capita`))+
  geom_point()+facet_grid(.~Ano)

## livre escalas e nomeando as UFs

  ggplot(censo2,
         aes(x=IDHM,
             y=`Renda per capita`))+
    geom_text(aes(label=Espacialidades),hjust=1, vjust=1)+
    geom_point()+facet_grid(.~Ano,scales="free")

### Criando variável com abreviação

censo2$ab_uf=factor(censo2$Espacialidades,labels=c("AC","AL","AP",
                "AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
                "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC",
                "SP","SE","TO"))
### Alterando escala numérica para ajustar a formatação brasileira
gx=ggplot(censo2,
       aes(x=IDHM,
           y=`Renda per capita`))+
  geom_text(aes(label=ab_uf),hjust=1, vjust=0)+
  geom_point()+
  scale_x_continuous( labels = scales::number_format(accuracy = 0.06,big.mark = '.',
                    decimal.mark = ',')) + # alterando para formato brasileiro
  scale_y_continuous( labels = scales::number_format(big.mark = '.',
                                                     decimal.mark = ',')) +
  facet_grid(.~Ano,scales="free")



# criar Regiões e associando aos estados com mutate
reg=c("Norte","Sul","Sudeste","Centro Oeste", "Nordeste")
## Uma categórica que dá origem a outra categórica

censo2=censo2 %>%
  mutate(Regiões= recode_factor(ab_uf,
          "AC"=reg[1],"AL"=reg[5],"AP"=reg[2],"AM"=reg[5],"BA"=reg[5],
          "CE"=reg[5],"ES"=reg[3],"GO"=reg[4],"MA"=reg[5],"MT"=reg[4],
          "MS"=reg[4],"MG"=reg[3],"PA"=reg[1],"PB"=reg[5],"PR"=reg[2],
          "PE"=reg[5],"PI"=reg[5],"RJ"=reg[3],"RN"=reg[5],"RS"=reg[2],
          "RO"=reg[1],"RR"=reg[1],"SC"=reg[2],"SP"=reg[3],"SE"=reg[5],
          "TO"=reg[1],"DF"=reg[4]))

#### Classificação do IDH por ano??
## http://www.atlasbrasil.org.br/2013/pt/o_atlas/idhm/
## 0,0 ate 0,499 Muito Baixo
## 0,5 até 0,599 Baixo
## 0,6 até 0,699 Médio
## 0,7 até 0,799 Alto
## 0,8 até 1 Muito Alto

censo2=censo2 %>% mutate(CIDHM=cut(IDHM, breaks=c(0, 0.5, 0.6, 0.7, 0.8, 1),
                           labels=c("Muito Baixo","Baixo","Médio",
                                    "Alto","Muito Alto")))

### Exercício plotar classe de IDH por estado no mapa do Brasil
## Por Ano

library(geobr)

states <- read_state(year=2014)

states2 <-left_join(states,censo2,by=c("code_state"="Código"))

library(RColorBrewer) # paletas de cores
display.brewer.all() # para ver opções

### Dica de cores para copia (Muito legal) ver link
# https://colorbrewer2.org/

cores=c('#e41a1c','#ff7f00','#4daf4a','#377eb8','#984ea3')
cores2=c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0')

ggplot() +
  geom_sf(data=states2, aes(fill=CIDHM), size=.15) +
  scale_fill_manual(values=cores2) +
  labs(title="Evolução do IDHM no Brasil de 2000 à 2010", size=8) +
  theme_minimal()+
   facet_grid(.~Ano)

### Escala livre de cores do baixo como vermelho (branco no meio) e alto com azul

ggplot() +
  geom_sf(data=states2, aes(fill=CIDHM), size=.15) +
  scale_fill_gradiente2(midpoint=mid, low="red", mid="white",high="azul") +
  labs(title="Evolução do IDHM no Brasil de 2000 à 2010", size=8) +
  theme_minimal()+
  facet_grid(.~Ano)

#### Ajustando Modelos de Regressão e plotando no gráfico de maneira prática
library(ggpmisc)
### Gerando polinômio de 1 grau (reta)
formula <- censo2$`Renda per capita`~ poly(censo2$IDHM, 1, raw = TRUE)

g1=ggplot(censo2,
       aes(x=IDHM,
           y=`Renda per capita`))+ geom_point()+
           stat_smooth(method = "lm",se=FALSE) +
  stat_poly_eq(aes(label = paste(..eq.label..,..adj.rr.label.., sep = "~~~~")),
               formula = formula, parse = TRUE)


### Polinômio de grau 2.


formula2 <- censo2$`Renda per capita`~ poly(censo2$IDHM, 2, raw = TRUE)

g2=ggplot(censo2,
       aes(x=IDHM,
           y=`Renda per capita`))+ geom_point()+
  stat_smooth(method="lm",formula= y~poly(x,2)) +
  stat_poly_eq(aes(label = paste(..eq.label..,..adj.rr.label.., sep = "~~~~")),
               formula = formula2, parse = TRUE)

## Método de Regressão Local loess


g3=ggplot(censo2,
          aes(x=IDHM,
              y=`Renda per capita`))+ geom_point()+
  stat_smooth(method="loess")


library(patchwork)
g1/g2/g3




