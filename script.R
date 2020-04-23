library(tidyverse)
library(reshape)
library(tidyr)
library(lubridate)
anos <- c(2018,2019,2020)
mortessp <- c(6769,6734,7755)
mortesma <- c(507,402,905)

df <- data.frame(anos = anos, `São Paulo` = mortessp, Manaus = mortesma)
obitos_registros <- gather(df, cidade, mortes, `São.Paulo`:Manaus, factor_key=TRUE)

casos = read.csv("caso.csv")
mean_deaths <- obitos_registros %>% 
   filter(cidade=="Manaus",
          anos %in% c(2018,2019)) %>%
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
   geom_hline(yintercept=as.numeric(max_deaths), linetype="dashed", color = "red")+
   scale_y_continuous(breaks = c(pretty(casos$deaths), as.numeric(max_deaths)), 
                      labels = c(pretty(casos$deaths), as.numeric(max_deaths)))
   
png("mortes_confirmadas_covid19_manaus.png",width=3200,height=1800,res=300)
print(p2)
dev.off()

