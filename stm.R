require(quanteda)
require(tidyverse)
require(readtext)
require(data.table)
require(openxlsx)
require(tidyverse)
require(stm)
require(tm)

stop <- str_split("é
la
pra
en
el
sobre
ser
y
vai
q
dia
the
pode
diz
los
$
utm_medium
utm_campaign
=
contra
tá
via
hoje
ter
ainda
gente
del
agora
todos
to
vc
aqui
con
un
fazer
tudo
todo
bem
anos
las
bom
vamos
quer
faz
porque
sempre
ver
vou
es
of
nunca
in
após
novo
nada
una
aí
vida
cara
acho
and
deve
pq
al
vez
onde
tão
menos
desde
cada
dar
dá
boa
coisa
maior
tempo
sabe
disse
parte
caso
falar
apenas
pro
assim
nova
fala
vem
segundo
ano
dias
fez
lá
toda
antes
dois
precisa
vão
sendo
todas
lo
su
nao
que
uma
mas
um
isso
para
com
i
qualquer
alguém
sei
meio
primeiro
le
dizer
pois
conta
semana
fim
is
outros
nesta
ficar
então
muita
sim
ninguém
outro
falta
desse
nome
estar
saber
nesse
deu
dessa
parece
pouco
hora
vivo
momento
obrigado
más
primeira
neste
tô
on
lado
pessoa
além
quase
olha
tipo
tanto
querem
tem
né
x
d
ha
por
uol
como
quem
não
dos
por
muito
kkk
kkkk
kkkkk
si
ta
igshid

that
with
tb
final
duas
vi
quanto
né
demais
outra
coisa
coisas
ir
l
amp
c
d
mesma
ha
vcs
alguns
r
ñ
nenhum
si
saiba
mil
ser
vamos
nova
bo
outras
desta
with
et
tb
you
uns
uso
tal
ai
this
n
at
ta
ato
p", pattern = "\n")

stop <- unlist(stop, use.names=FALSE)

tweets <- readtext("tweets_presidenciaveis.csv")
tweets <- tweets %>% filter(person == "Haddad")
tweets <- tweets %>% rename(documents = text.1)
processed <- textProcessor(tweets$documents, stem= F, lowercase = F, 
                           metadata = tweets, removestopwords = T, 
                           customstopwords = stop, striphtml = T)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

findingk <- searchK(out$documents, out$vocab, K = c(16, 25, 36, 49, 64),
                    prevalence =~ periodo_eleitoral, data = meta, verbose=FALSE)
plot(findingk)

F_STM <- stm(documents = out$documents, vocab = out$vocab,
             K = 25, prevalence =~ periodo_eleitoral,
             max.em.its = 250, data = out$meta,
             init.type = "Spectral", verbose = T,
             seed = pi)
plot(F_STM)
plot(F_STM, type="labels", topics=c(4, 7,9))
predict_topics<-estimateEffect(formula = 1:25 ~ periodo_eleitoral, stmobj = F_STM, 
                                                             metadata = out$meta, uncertainty = "Global")
plot(predict_topics, covariate = "periodo_eleitoral", topics = c(4, 7, 9),
     model = F_STM, method = "difference",
     cov.value1 = "Sim", cov.value2 = "Não",
     xlab = "Não ... Sim",
     main = "Efeito do Período Eleitoral nos Tópicos - Haddad",
     xlim = c(-.1, .1),
     labeltype = "custom",
     custom.labels = c('Educação', "Segurança", 'Lula'))

# BOLSONARO

tweets <- readtext("tweets_presidenciaveis.csv")
tweets <- tweets %>% filter(person == "Bolsonaro")
tweets <- tweets %>% rename(documents = text.1)
processed <- textProcessor(tweets$documents, stem= F, lowercase = F, 
                           metadata = tweets, removestopwords = T, 
                           customstopwords = stop, striphtml = T)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

F_STM <- stm(documents = out$documents, vocab = out$vocab,
             K = 25, prevalence =~ periodo_eleitoral,
             max.em.its = 250, data = out$meta,
             init.type = "Spectral", verbose = T,
             seed = pi)
plot(F_STM)

predict_topics<-estimateEffect(formula = 1:25 ~ periodo_eleitoral, stmobj = F_STM, 
                               metadata = out$meta, uncertainty = "Global")
plot(predict_topics, covariate = "periodo_eleitoral", topics = c(1, 6, 8, 14, 15),
     model = F_STM, method = "difference",
     cov.value1 = "Sim", cov.value2 = "Não",
     xlab = "Não ... Sim",
     main = "Efeito do Período Eleitoral nos Tópicos - Bolsonaro",
     xlim = c(-.1, .1),
     labeltype = "custom",
     custom.labels = c("Israel", "Segurança", 'Imprensa & Fake News', "Economia", "Esquerda"))


