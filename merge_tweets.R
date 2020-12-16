require(tidyverse)
library(readr)

`%notin%` <- Negate(`%in%`)

Bolsonaro <- read_csv("jairbolsonaro.csv")
Marina <- read_csv("MarinaSilva.csv")
Alckmin <- read_csv("geraldoalckmin.csv")
Haddad <- read_csv("Haddad_Fernando.csv")
Lula <- read_csv("LulaOficial.csv")
Boulos <- read_csv("GuilhermeBoulos.csv")
Ciro <- read_csv("cirogomes.csv")


combinado <- bind_rows(
  Bolsonaro %>%
    mutate(person = "Bolsonaro"),
  Marina %>%
    mutate(person = "Marina"),
  Alckmin %>%
    mutate(person = "Alckmin"),
  Boulos %>%
    mutate(person = "Boulos"),
  Haddad %>%
    mutate(person = "Haddad"),
  Lula %>%
    mutate(person = "Lula"),
  Ciro %>%
    mutate(person = "Ciro"),
)

combinado <- combinado %>% 
  mutate(periodo_eleitoral = case_when(
    (combinado$date < as.Date('2018-10-29'))&(combinado$date > as.Date('2018-07-07')) ~ "Sim",
                                                                         T ~ "Não"))

ggplot(data = combinado, aes(x = date)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Tempo") + ylab("Número de tweets") + 
  scale_fill_gradient(low = "darkblue", high = "darkorange")

write.csv(combinado, 'tweets_presidenciaveis.csv', row.names = F)
