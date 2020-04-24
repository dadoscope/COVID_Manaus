library(tidyverse)
library(reshape)
library(tidyr)
library(lubridate)
anos <- c(2015,2016,2017,2018,2019,2020)
mortesma <- c(148,166,410,507,402,905)

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
   ggplot(aes(x = anos, y = mortes, fill=cidade)) +
   geom_text(aes(x = anos, y = mortes, label = mortes, vjust = 0)) +
   geom_bar(stat="identity") + 
   labs(title = "Óbitos no Mês de Março", 
        subtitle = "Dados de registro civil")+
   theme_bw()+
   geom_hline(yintercept=as.numeric(mean(mean_deaths$mortes)), linetype="dashed", color = "red")+
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
   geom_line() + 
   theme_bw()+
   labs(title = "Acumulado de óbitos por covid19 confirmados",
        subtitle = "Manaus")+
   geom_hline(yintercept=c(as.numeric(march_deaths$deaths),as.numeric(max_deaths$deaths)), linetype="dashed", color = "red")+
   scale_y_continuous(breaks = c(pretty(casos$deaths), as.numeric(max_deaths)), 
                      labels = c(pretty(casos$deaths), as.numeric(max_deaths)))
   
png("mortes_confirmadas_covid19_manaus.png",width=3200,height=1800,res=300)
print(p2)
dev.off()

italy_max_death100k = 10.5
p3 <- casos %>% filter(city == "Manaus") %>% 
   mutate(pop = confirmed*100000/confirmed_per_100k_inhabitants) %>% 
   mutate(deaths_per_100k_inhabitants = deaths*100000/pop) %>%
   mutate(date = ymd(date)) %>%
   ggplot(aes(x = date, y = deaths_per_100k_inhabitants)) +
   geom_line() + 
   theme_bw()+
   labs(title = "Taxa de letalidade por 100 Mil habitantes",
        subtitle = "Manaus")+
   geom_hline(yintercept=c(as.numeric(italy_max_death100k)), linetype="dashed", color = "red")+
   scale_y_continuous(breaks = c(pretty(casos$deaths_per_100k_inhabitants), as.numeric(italy_max_death100k)), 
                      labels = c(pretty(casos$deaths_per_100k_inhabitants), as.numeric(italy_max_death100k)))


png("mortes_confirmadas_covid19_100k_hab_manaus.png",width=3200,height=1800,res=300)
print(p3)
dev.off()
