library(PNADcIBGE)
library(dplyr)

variaveis=c('Ano','Trimestre','UF','V2010','V4012','V20082','V2005','V2007','V3009A','V2010',
            'V4040','V4039','V403312','V4019',"V403312","V403322","V403412","V403422","V405012",
            "V405022","V405112","V405122","V405812","V405822","V405912","V405922",'V4016',
            'V40161','V40162','V40163','V40171','V4040',
            'V40401','V40402','V40403',"V401711")
empreendedores=c("Empregador","Conta própria")
clean=function(data){
  empreendedores1=subset(data,V4012 %in% empreendedores)
  x=data.frame(empreendedores1$pweights)
  x1=data.frame(empreendedores1$variables)
  x1=x1%>%select(all_of(variaveis))
  x1$weight=x$empreendedores1.pweights
  x1
}
PNAD2018T1 <- get_pnadc(year=2018, quarter=1, vars=variaveis)
PNAD2018T1=clean(PNAD2018T1)
PNAD2018T2 <- get_pnadc(year=2018, quarter=2, vars=variaveis)
PNAD2018T2=clean(PNAD2018T2)
PNAD2018T3 <- get_pnadc(year=2018, quarter=3, vars=variaveis)
PNAD2018T3=clean(PNAD2018T3)
PNAD2018T4 <- get_pnadc(year=2018, quarter=4, vars=variaveis)
PNAD2018T4=clean(PNAD2018T4)

PNAD2019T1 <- get_pnadc(year=2019, quarter=1, vars=variaveis)
PNAD2019T1=clean(PNAD2019T1)
PNAD2019T2 <- get_pnadc(year=2019, quarter=2, vars=variaveis)
PNAD2019T2=clean(PNAD2019T2)
PNAD2019T3 <- get_pnadc(year=2019, quarter=3, vars=variaveis)
PNAD2019T3=clean(PNAD2019T3)
PNAD2019T4 <- get_pnadc(year=2019, quarter=4, vars=variaveis)
PNAD2019T4=clean(PNAD2019T4)

PNAD2020T1 <- get_pnadc(year=2020, quarter=1, vars=variaveis)
PNAD2020T1=clean(PNAD2020T1)
PNAD2020T2 <- get_pnadc(year=2020, quarter=2, vars=variaveis)
PNAD2020T2=clean(PNAD2020T2)
PNAD2020T3 <- get_pnadc(year=2020, quarter=3, vars=variaveis)
PNAD2020T3=clean(PNAD2020T3)
PNAD2020T4 <- get_pnadc(year=2020, quarter=4, vars=variaveis)
PNAD2020T4=clean(PNAD2020T4)

PNAD2021T1 <- get_pnadc(year=2021, quarter=1, vars=variaveis)
PNAD2021T1=clean(PNAD2021T1)
PNAD2021T2 <- get_pnadc(year=2021, quarter=2, vars=variaveis)
PNAD2021T2=clean(PNAD2021T2)
PNAD2021T3 <- get_pnadc(year=2021, quarter=3, vars=variaveis)
PNAD2021T3=clean(PNAD2021T3)
PNAD2021T4 <- get_pnadc(year=2021, quarter=4, vars=variaveis)
PNAD2021T4=clean(PNAD2021T4)

PNAD2022T1 <- get_pnadc(year=2022, quarter=1, vars=variaveis)
PNAD2022T1=clean(PNAD2022T1)
PNAD2022T2 <- get_pnadc(year=2022, quarter=2, vars=variaveis)
PNAD2022T2=clean(PNAD2022T2)
PNAD2022T3 <- get_pnadc(year=2022, quarter=3, vars=variaveis)
PNAD2022T3=clean(PNAD2022T3)
PNAD2022T4 <- get_pnadc(year=2022, quarter=4, vars=variaveis)
PNAD2022T4=clean(PNAD2022T4)

PNAD2023T1 <- get_pnadc(year=2023, quarter=1, vars=variaveis)
PNAD2023T1=clean(PNAD2023T1)
PNAD2023T2 <- get_pnadc(year=2023, quarter=2, vars=variaveis)
PNAD2023T2=clean(PNAD2023T2)
PNAD2023T3 <- get_pnadc(year=2023, quarter=3, vars=variaveis)
PNAD2023T3=clean(PNAD2023T3)
PNAD2023T4 <- get_pnadc(year=2023, quarter=4, vars=variaveis)
PNAD2023T4=clean(PNAD2023T4)

PNAD20182023=rbind(PNAD2018T1,PNAD2018T2,PNAD2018T3,PNAD2018T4,
                   PNAD2019T1,PNAD2019T2,PNAD2019T3,PNAD2019T4,
                   PNAD2020T1,PNAD2020T2,PNAD2020T3,PNAD2020T4,
                   PNAD2021T1,PNAD2021T2,PNAD2021T3,PNAD2021T4,
                   PNAD2022T1,PNAD2022T2,PNAD2022T3,PNAD2022T4,
                   PNAD2023T1,PNAD2023T2,PNAD2023T3,PNAD2023T4)
saveRDS(PNAD20182023,"PNAD20182023.rds")
PNAD=readRDS("PNAD20182023.rds")
