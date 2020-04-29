library(tidyverse)
library(reshape)
library(tidyr)
library(lubridate)
setwd("/Users/isiscosta/RScript/COVID_Manaus")
anos <- c(2015,2016,2017,2018,2019,2020)
mortesma <- c(148,166,410,507,402,905)
barcolor <- "#b5403e"

df <- data.frame(anos = anos, Manaus = mortesma)
obitos_registros <- gather(df, cidade, mortes, Manaus, factor_key=TRUE)
casos = read.csv("caso.csv")
simam = read.csv("SIMAM2018.csv")

mean_deaths <- obitos_registros %>% 
   filter(cidade=="Manaus",
          anos %in% c(2015,2016,2017,2018,2019)) %>%
   select(mortes)
p1 <- obitos_registros %>% 
   filter(cidade=="Manaus") %>%
   ggplot(aes(x = anos, y = mortes)) +
   geom_text(aes(x = anos, y = mortes, label = mortes, vjust = 0)) +
   geom_bar(stat="identity", fill = barcolor) + 
   labs(title = "Óbitos no Mês de Março", 
        subtitle = "Dados de registro civil")+
   theme_bw()+
   geom_hline(yintercept=as.numeric(mean(mean_deaths$mortes)), linetype="dashed", color = "blue")+
   scale_y_continuous(breaks = c(pretty(obitos_registros$mortes), as.numeric(mean(mean_deaths$mortes))), 
                      labels = c(pretty(obitos_registros$mortes), as.numeric(mean(mean_deaths$mortes))))

png("mortes_em_manaus.png",width=3200,height=1800,res=300)
print(p1)
dev.off()



max_deaths <- casos %>% 
   filter(city == "Manaus") %>%
   filter(is_last == "True") %>%
   select(deaths)
march_deaths <- casos %>% 
   filter(city == "Manaus",
          date == "2020-03-31") %>%
   select(deaths)
min_date <- casos %>% 
   filter(city == "Manaus") %>%
   filter(ymd(date) == min(ymd(date))) %>%
   select(date)
p2 <- casos %>% filter(city == "Manaus") %>%
   mutate(date = ymd(date)) %>%
   ggplot(aes(x = date, y = deaths)) +
   geom_line(size=1.2) + 
   theme_bw()+
   labs(title = "Acumulado de óbitos por covid19 confirmados",
        subtitle = "Manaus",
        x = "Data",
        y = "# de Mortes")+
   geom_hline(yintercept=c(as.numeric(max_deaths$deaths)), linetype="dashed", color = "blue")+
   scale_y_continuous(breaks = c(pretty(casos$deaths), as.numeric(max_deaths)), 
                      labels = c(pretty(casos$deaths), as.numeric(max_deaths)))
   
png("mortes_confirmadas_covid19_manaus.png",width=3200,height=1800,res=300)
print(p2)
dev.off()

italy_max_death100k = 10.5

p3 <- casos %>% filter(city == "Manaus") %>% 
   mutate(pop = 2182763) %>% 
   mutate(deaths_per_100k_inhabitants = deaths*100000/pop) %>%
   mutate(date = ymd(date)) %>%
   ggplot(aes(x = date, y = deaths_per_100k_inhabitants)) +
   geom_line(size=1.2) + 
   theme_bw()+
   labs(title = "Taxa de letalidade por 100 Mil habitantes",
        subtitle = "Manaus",
        x = "Data",
        y = "Mortes/100k habitantes")+
   geom_hline(yintercept=c(11.27), linetype="dashed", color = "red")+
   geom_hline(yintercept=c(as.numeric(italy_max_death100k)), linetype="dashed", color = "blue")+
   scale_y_continuous(breaks = c(pretty(casos$deaths_per_100k_inhabitants), as.numeric(italy_max_death100k), 11.27), 
                      labels = c(pretty(casos$deaths_per_100k_inhabitants), as.numeric(italy_max_death100k), 11.27))


png("mortes_confirmadas_covid19_100k_hab_manaus.png",width=3200,height=1800,res=300)
print(p3)
dev.off()

### Abril 2020

anos <- c(2015,2016,2017,2018,2019,2020)
mortesma <- c(104 ,417,157,266,591,1676 )
barcolor <- "#b5403e"
df <- data.frame(anos = anos, Manaus = mortesma)
obitos_registros <- gather(df, cidade, mortes, Manaus, factor_key=TRUE)

mean_deaths <- obitos_registros %>% 
  filter(cidade=="Manaus",
         anos %in% c(2015,2016,2017,2018,2019)) %>%
  select(mortes)
p1 <- obitos_registros %>% 
  filter(cidade=="Manaus") %>%
  ggplot(aes(x = anos, y = mortes)) +
  geom_text(aes(x = anos, y = mortes, label = mortes, vjust = 0)) +
  geom_bar(stat="identity", fill = barcolor) + 
  labs(title = "Óbitos no Mês de Abril", 
       subtitle = "Dados de registro civil")+
  theme_bw()+
  geom_hline(yintercept=as.numeric(mean(mean_deaths$mortes)), linetype="dashed", color = "blue")+
  scale_y_continuous(breaks = c(pretty(obitos_registros$mortes), as.numeric(mean(mean_deaths$mortes))), 
                     labels = c(pretty(obitos_registros$mortes), as.numeric(mean(mean_deaths$mortes))))

png("mortes_em_manaus_abril.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

