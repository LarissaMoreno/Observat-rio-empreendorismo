library(PNADcIBGE)
library(dplyr)

variaveis=c('Ano','Trimestre','UF','V2010','V4012','V20082','V2005','V2007','V3009A','V2010',
            'V4040','V4039','V403312','V4019',"V403312","V403322","V403412","V403422","V405012",
            "V405022","V405112","V405122","V405812","V405822","V405912","V405922",'V4016',
            'V40161','V40162','V40163','V40171','V4040',
            'V40401','V40402','V40403',"V401711")
empreendedores=c("Empregador","Conta pr?pria")
clean=function(data){
  empreendedores1=subset(data,V4012 %in% empreendedores)
  x=data.frame(empreendedores1$pweights)
  x1=data.frame(empreendedores1$variables)
  x1=x1%>%select(all_of(variaveis))
  x1$weight=x$empreendedores1.pweights
  x1
}

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


PNAD=rbind(PNAD2023T1,PNAD2023T2,PNAD2023T3,PNAD2023T4,
           PNAD2022T1,PNAD2022T2,PNAD2022T3,PNAD2022T4)
saveRDS(PNAD,"c.rds")
qs::qsave(PNAD, file = "PNAD1.qs")
library(qs)
PNAD=qread("PNAD.qs")
