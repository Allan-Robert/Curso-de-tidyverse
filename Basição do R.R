########### Comandos básicos no R ###############

# Versões do R (Rgui e RStudio)
# Pacotes básicoas e adicionais

# O R um software livre que requer usar comandos e inicalmente foi criado para 
# melhoria do aprendizado em estatística e agora é usado para trabalho 
# profissional, que contem diversos pacotes para diferentes áreas de atuação.


# Em todos softwares que envolvem comandos um dos elementos essenciais é como 
# fazer comentários

# Tudo do lado direito do simbolo do hashtag ou "jogo da velha" é comentário
# Logo O R enteder que não é um comando


# Associando objtos 

A=2 #  Cria um objeto chamado "A" contendo o número  2
B=3 #  Cria um objeto chamado "B" contendo o número 3
A=4 #  Cria um objeto chamado "A" contendo o número 4 
# Agora A foi atualizado 
a=5 #  Cria um objeto chamado "a" contendo o número 5 
# Distinção entre maiúscula e minúscula "a" é diferente de "A" 


# testar qual destes elementos o r aceita 
2r=2; r2=2 ;r*=2   # OBS: O ponto e virgula (;) permite que mais de um comando 
# seja feito em mesma linha 

ls() # lista todos objetos criados 
rm(A,B) # Apaga os objetos A e B que criamos
rm(list=ls()) # apaga todos objetos que foram criados  

# SOBRE PACOTEs

# A versão básica do R vem com um grupo limitado de pacotes e caso necessite 
# pode instalar mais

install.packages("qcc") # Instala o pacote denominado "qcc" usado em controle 
# de qualidade
# OBS: Diferença entre instalar e usar 

library(qcc) # Comando que carrega pacotes (neste caso o pacote  qcc)
library() # mostra todos os pacotes instalados 
library(help=qcc) # Mostra todas funções e dados de um pacote (neste caso o qcc)  
data(package="qcc") # Mostra somente os conjuntos de dados de um pacote (neste caso o qcc)  
data(boiler) # torna acessivel o conjunto de dados de nome boiler (por ser do pacote qcc este comando s? acessa o conjunto de dados se j? tiver sido carregado o pacote)
### OBS não é regra, por exemplo tidyverse não necessita disso.

### Duvidas (help) sobre funções ou pacotes usar ? antes 
?boiler
?var

# Procurar funções envolvendo o termo "Multivariate". 
help.search("multivariate")  


# Operações matemáticas básicas 
7+2 # soma
7-2 # diferença
7*2 # multiplicação
7/2 # divisão

# criando duas variaveis (ou objetos)
x1=7 # recebendo o número 7
x2=2 # recebendo o número 2

# Operações matemáticas básicas usando variáveis
x1+x2 # soma
x1-x2 # diferença
x1*x2 # multiplicação
x1/x2 # divisão

# outras funções matemáticas
n<-15 # outra forma de atribuir além do sinal de igual
cos(n)  # cosseno
sin(n)  # seno
exp(n)  # exponencial
log(n)  # logaritmo natural
n^4     # ^ sinal de potenciação  
sqrt(n) # raiz quadrada
n^(2/3) # o que deve resultar disto? 
factorial(n) # fatorial  
trunc(n/2) # Retorna o inteiro de uma fração (neste 15/2=7.5 e retorna 7)
round(sqrt(n),digits=3) # Arredonda um número par, neste caso, 3 dígitos.
n%/%2 # Quociente da divisão
n%%2 # Resto da divisão
n/0 # Retorna Inf que significa infinito se fosse -n/0 retornaria -Inf
0/0 # Retorna NaN que significa que não é um m número 


# Principais Sinais lógicos
# <- e = indica que uma variável irá receber um dado 
# == igualdade 
# != diferença
# < > menor e maior 
# &  "e" lógico
# |  "ou" lógico

# Exemplos 

15421 %% 7 == 0 # Estamos perguntando se resto da divisão de 15421 por 7 é zero? 
40%/%5 != 8 # Estamos perguntado se o quociente da divisão de 40 por 5 é 8 ?  
exp(-Inf) == 0 # Aqui a resposta ?

## Principais objetos do R

### Vetores inicia em 1

# o comando c()  concatena elementos separados por vírgulas

xx=c(1,2,3,5,7,NA)  # OBS: NA representa valores faltantes
xx[5]  # Refere-se ao 5 elemento
xx[c(1,6)]# Refere-se aos elementos 1 e 6 
xx[1:3] # Refere-se a sequencia do elemento 1 ao 3 

class(xx) # classe associada ao elemento

xt=c("a","bc","x") # Vetor de caracteres (Observe a necessidade de usar aspas "")
class(xt) 
mix=c("a",2i,4) # Vetor com caracteres, número complexo e um número comum.
mix # observe que todos foram convertidos em caracteres


# Matrizes

xx=matrix(c(1,2,3,4),ncol=2) # 1 coluna feita pelos 2 primeiros elementos
xx[2,1] # Retorna o elemento da linha 2 coluna 1 
yy=matrix(c(1,2,3,4),ncol=2, byrow=T)  # 1 linha feita pelos 2 primeiros elementos
yy[1:2,1] # Retonra elementos da 1? a 2? linha que s?o  da 1? coluna
yy[2,]    # Retorna elementos da linha 2 de todas colunas

# Operações possiveis com matrizes ou vetores 
# (OBS: válidas obviamente se as regras matemáticas permitirem)

0.5*yy      # multiplicaçao de uma constante por uma matriz
0.5+yy      # soma de uma constante por uma matriz
somax=xx+yy # soma de matrizes
xx%*%yy     # multiplicaçao de matrizes 
t(xx)       # transposta de uma matriz 
solve(yy)   # inversa ( para matrizes quadradas )
det(xx)     # determinante de uma matriz
diag(yy)    # Pega a diagonal da matriz
eigen(xx)   # auto valores e auto vetores de uma matriz
rbind(xx,yy)# concatena duas matrizes (ou vetores) por linha
cbind(xx,yy)# concatena duas matrizes (ou vetores) por coluna


##### Fatores

# Fatores tem uma função de variáveis categóricas

A=rep(c(1,2),times=8) # Repete o vetor (1,2) oito vezes
class(A)
fa=as.factor(A) # P?e em "fa" a convers?o do vetor "A" em fator
A;fa # saidas diferem
class(fa)

# É possível renomar as variáveis que são fatores
nfa=factor(fa,labels=c("I","II"))
nfa

# Tabelas de Contigência no R - 
# Usadas especialmente em Estatística Não Paramétrica

# Simulando conjunto de dados com sexo, origem e notas de 16 alunos

sexo=rep(c("Masculino","Feminino"),8)
notas=c(10,8,8,7,2,3,5,4,4,5,8,9,9,8,8,10)
origem1=rep(c("Pub","Par"),each=7) ;origem2=c("Fed","Fed")
origem=c(origem1,origem2)

table(sexo) # contagem por sexo
table(origem) # contagem por Origem
tab=table(sexo,origem) # Contagem por sexo x origem guradado em "tab"
tab
class(tab)

margin.table(tab,1) # marginal da vari?vel de linha
margin.table(tab,2)  # marginal da vari?vel de coluna

prop.table(tab) # percentual geral
prop.table(tab,1) # percentual por linha
prop.table(tab,2) # percentual por coluna

# Listas objetos mais amplos, pois permitem múltiplas class
# Tipo vetores, matrizes ( com números e/ou caracteres) e até outras listas

## Exemplo
R<-list(versao=4, origem='Áustria', notas=c(9,10,8))
names(R) #ver objetos guardados dentro de R
R$versao # Fazendo referência a um objeto da lista
R$notas

# Data frames São semelhantes a matrizes por terem duas dimensões 
# podem ter diferentes tipos de elementos em cada coluna
# Usados em banco de dados


# Usando os dados gerados anteriormente

dado=data.frame(Sexo=sexo,Proeminencia=origem)
dado # observe que as variáveis foram renomeadas 
class(dado)
str(dado) # observando características das variáveis

# OBS agora temos sexo externo ao dataframe "dado" e Sexo dentro do "dado"

sexo;dado$Sexo


# pode-se ter muita confusão
# Exemplo: Alterar um elemento do sexo 

# O que está fora de "dado"

sexo[1]="Feminino" # alterando de masculino para feminino
## Note que isso não altera o que está dento de "dado"
dado$Sexo

## para fazer o mesmo em dado temos duas opções
# Rodar novamente o comando dado=data.frame(Sexo=sexo,Proeminencia=origem)
# Assim eu atualizo o dado ... ou diretamente no banco de dados:

dado$sexo[1]="Feminino"


### Medidas de Resumo e família apply

#### Medidas de Resumo

# Medidas de posiçao e variabilidade mais usadas 
# Para exemplicifar usaremos alguns bancos de dados InsectSprays e HairEyeColor

# InsectSprays - variaveis: "count" conta o número de insetos mortos e 
# "spray" que é o tipo de spray usado


attach(InsectSprays) # acessa diretamente suas variáveis
### Obs alterações da variável permanente deverá conter nome dos dados + $ + variável

sum(count)# Total 
mean(count) # Média
median(count) # Mediana
var(count) # Variância
sd(count) # desvio padrão
summary(count)# Apresenta mínimo, máximo, média mediana e 1º e 3º  quartil


# Medidas especificas para um determinado tipo de spray, 
# por exemplo o tipo A

mean(count[spray=="A"]) 
median(count[spray=="A"]) 
var(count[spray=="A"]) 
sd(count[spray=="A"]) 
summary(count[spray=="A"])

####### Comandos da família apply
####### Outra forma de resumir dados


# 1) Apply : Aplicada uma funçao em margens de um array, 
# matriz ou dataframe (obviamente deve ter números).  

# exemplo dados de HairEyeColor

data(HairEyeColor) 

# Observe que este conjunto de dados é um array
is.array(HairEyeColor)
# dimensões 1 linha (Cor dos cabelos)  2ª coluna cor dos olhos 
# 3ª(Gera duas matrizes) refere-se ao Sexo  

# (a) Qual a proporçao de homens e mulheres na amostra?

apply(HairEyeColor,3,sum) # Total por sexo
apply(HairEyeColor,3,sum)/sum(HairEyeColor) # Proporçao por sexo 

# (b) Quantos são os homens de cabelos pretos?

apply(HairEyeColor,c(1,3),sum) # Total por Cabelo e Sexo. Logo a resposta ? 56

# Para ser mais e usar um comando
cx=apply(HairEyeColor,c(1,3),sum) ; cx[1,1] # homens de cabelo preto




#2) tapply : Tem objetivo de aplicar funçoes em grupos (Ou categorias) diferentes.  

## voltando ao exemplo InsectSprays

tapply(count,spray,mean) # número médio de insetos por tipo de spray

tapply(count,spray,var) # Variância de insetos por tipo de spray


#3)  lapply : Funçao para listas

lista = list(x1=c(1,2,NA,4,5), x2=seq(1,100,1), x3=rnorm(100))
# OBS: O comando "rnorm" gera elementos da distribuição normal 

lapply(lista, mean)

# observe que o meam não faz o cálculo para X1 por ter dados faltantes (NA)
# Uma soluçao é incluir o argumento na funçao lapply conforme mostra abaixo

lapply(lista, mean, na.rm=T)

#4) sapply : Similar ao lapply, com saída diferente. 

sapply(lista, mean,na.rm=T)


#5) mapply: Versão multivariada do sapply. (Mais complexa, mas pode ter saidas 
# computacionais mais rápidas em alguns casos)


#Exemplo fazendo uma lista

list(rep(1,9),rep(2,8),rep(3,7),rep(4,6))

# Mesma lista usando mapply

mapply(rep, x = 4:1, times = 9:6)

## Lista de funções rápidas
## Ver o link: http://www.leg.ufpr.br/~walmes/cursoR/guia_rapido_R.pdf

############# Livros e outros materiais online
## https://bookdown.org/wevsena/curso_r_tce/curso_r_tce.html
## https://cran.r-project.org/doc/contrib/Landeiro-Introducao.pdf
## https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf
