#setwd("C:/Users/User/Desktop/IBICT/observatorio empreendorismo/app/observatorio")
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(data.table)
library(formattable)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(ggtext)
library(qs)
#############################################################################
data4=readRDS("BrazilIndividual.rds")
x1=qread("PNAD1.qs")

x1$Idade=as.numeric(x1$Ano)-
  as.numeric(x1$V20082)

x1$Idade1 = with(x1, 
                 ifelse( Idade < 15,"<14",
                         ifelse(Idade >=15 & Idade < 20,"15-20",
                                ifelse(Idade >=20 & Idade < 25,"20-25",
                                       ifelse(Idade >=25 & Idade < 30,"25-30",
                                              ifelse(Idade >=30 & Idade < 35,"30-35",
                                                     ifelse(Idade >=35 & Idade < 40,"35-40",
                                                            ifelse(Idade >=40 & Idade < 45,"40-45",
                                                                   ifelse(Idade >=45 & Idade < 50,"45-50",
                                                                          ifelse(Idade >=50 & Idade < 55,"50-55",
                                                                                 ifelse(Idade >=55 & Idade < 60,"55-60",
                                                                                        ifelse(Idade >=60 & Idade < 65,"60-65",
                                                                                               ifelse(Idade >=65 & Idade < 85,"65+","Ignorado")
                                                                                        ))))))))))))

x1$Região=with(x1,
               ifelse(UF %in% c("Amazonas", 'Pará', 'Roraima', 'Amapá', 'Rondônia', 'Acre', 'Tocantins'),
                      "Norte",
                      ifelse(UF %in% c('Piauí', 'Maranhão', 'Pernambuco', 'Rio Grande do Norte', 'Paraíba', 
                                       'Ceará', 'Bahia', 'Alagoas', 'Sergipe'),"Nordeste",
                             ifelse(UF %in% c('Mato Grosso', 'Mato Grosso do Sul', 'Goiás'), "Centro-Oeste",
                                    ifelse(UF %in% c('São Paulo', 'Rio de Janeiro', 'Espírito Santo', 'Minas Gerais'),
                                           "Sudeste","Sul"
                                    )))))
x1$Chefe = with(x1, ifelse(
  V2005=="Pessoa responsável pelo domicílio","Chefe de condomínio","Outros"
))

x1$V403412<-ifelse(is.na(x1$V403412),0,x1$V403412)
x1$V403422<-ifelse(is.na(x1$V403422),0,x1$V403422)
x1$V405112<-ifelse(is.na(x1$V405112),0,x1$V405112)
x1$V405122<-ifelse(is.na(x1$V405122),0,x1$V405122)
x1$V405912<-ifelse(is.na(x1$V405912),0,x1$V405912)
x1$V405922<-ifelse(is.na(x1$V405922),0,x1$V405922)
x1$rendimentos<-x1$V403412+x1$V403422+x1$V405112+x1$V405122+x1$V405912+x1$V405922

x1$socio=with(x1,
              ifelse(V40171=="6 ou mais sócios","6+",
                     V401711))
x1$socio=with(x1,
              ifelse(socio %in% c("3","4","5","6+"),"3 ou mais",socio))


###########################################################################################


ui=navbarPage(shinyWidgets::useShinydashboard(),title="OEMFE", header = includeCSS('style.css'),
              #########################TABPNAEL###############################
              tabPanel("Sociodemográfico",
                       inputPanel(
                         selectInput("ano1","Ano",
                                     choices =sort(unique(x1$Ano)),
                                     selected = 2017),
                         pickerInput("sexo1","sexo",
                                     choices =sort(unique(x1$V2007)),
                                     selected = "Mulher"),
                         pickerInput("idade1","Idade",
                                     choices =sort(unique(x1$Idade1)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$Idade1)),
                         pickerInput("uf1","UF",
                                     choices =sort(unique(x1$UF)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$UF)),
                         pickerInput("raca1","Raça",
                                     choices =sort(unique(x1$V2010)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$V2010)),
                         
                       ),
                       mainPanel(
                         valueBoxOutput(outputId = "box_1",
                                        width = 6),
                         valueBoxOutput(outputId = "box_2",
                                        width = 6)),
                       
                       mainPanel(
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot1"))),style="position: relative;left: 250px;bottom:-35px;")),
                         
                         fluidRow(div(column(6,div(style="width:600px;",DTOutput("plot2"))),style="position: relative;left: -200px;bottom:-50px;"),
                                  div(column(6,div(style="width:600px;position: relative;bottom:-50px;",plotOutput("plot5")))),style="position: relative;left: 250px;bottom:-50px;"),
                         
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot3"))),style="position: relative;left: 15px;bottom:-150px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot4"))),style="position: relative;left: 250px;bottom:-150px;"))
                         
                       )
                       
              ),
              ############################### TABPANEL 2##########################################         
              tabPanel("Socioeconomico",
                       inputPanel(
                         selectInput("ano2","Ano",
                                     choices =sort(unique(x1$Ano)),
                                     selected = 2017),
                         pickerInput("sexo2","sexo",
                                     choices =sort(unique(x1$V2007)),
                                     selected = "Mulher"),
                         pickerInput("idade2","Idade",
                                     choices =sort(unique(x1$Idade1)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$Idade1)),
                         pickerInput("uf2","UF",
                                     choices =sort(unique(x1$UF)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$UF)),
                         pickerInput("raca2","Raça",
                                     choices =sort(unique(x1$V2010)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$V2010)),
                         
                       ),
                       mainPanel(
                         valueBoxOutput(outputId = "box_3",
                                        width = 6)),
                       mainPanel(
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot6"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot7"))),style="position: relative;left: 250px;bottom:-35px;"))
                       )
              ),
              #############################TABPANEL 3
              tabPanel("Empresariais",
                       inputPanel(
                         selectInput("ano3","Ano",
                                     choices =sort(unique(x1$Ano)),
                                     selected = 2017),
                         pickerInput("sexo3","sexo",
                                     choices =sort(unique(x1$V2007)),
                                     selected = "Mulher"),
                         pickerInput("idade3","Idade",
                                     choices =sort(unique(x1$Idade1)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$Idade1)),
                         pickerInput("uf3","UF",
                                     choices =sort(unique(x1$UF)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$UF)),
                         pickerInput("raca3","Raça",
                                     choices =sort(unique(x1$V2010)),
                                     options = list(`actions-box` = TRUE),multiple = T,
                                     selected = unique(x1$V2010)),
                         
                       ),
                       mainPanel(
                         fluidRow(div(column(6,div(style="width:600px;",plotOutput("plot8"))),style="position: relative;left: 15px;bottom:-35px;"),
                                  div(column(6,div(style="width:600px;",plotOutput("plot9"))),style="position: relative;left: 250px;bottom:-35px;"))
                         
                       )
              ),
              
              ############################TABPANEL 4
              tabPanel("Motivacionais",
                       inputPanel(
                         selectInput("ano4","Ano",
                                     choices =sort(unique(data4$yrsurv)),
                                     selected = 2017),
                         pickerInput("sexo4","sexo",
                                     choices =sort(unique(data4$gender)),
                                     selected = "Mulher")),
                       mainPanel(
                         fluidRow(div(column(6,div(style="width:600px;",DTOutput("table1"))),style="position: relative;left: -255px;bottom:0px;"),
                                  div(column(6,div(style="width:600px;",DTOutput("table5")))),style="position: relative;left: 255px;bottom:-50px;"),
                         
                         fluidRow(div(column(6,div(style="width:600px;position: relative;bottom:50px;",DTOutput("table2"))),style="position: relative;left: -255px;bottom:-50px;"),
                                  div(column(6,div(style="width:600px;",DTOutput("table4")))),style="position: relative;left: 255px;bottom:-100px;")
                       ))
)
server=function(input, output, session) {
  ######################## TABPANEL 1
  output$box_1 <- shinydashboard::renderValueBox({
    d=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                  Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      summarise(media=paste0(round(weighted.mean(V4039,weight),2)," horas"))
    valueBox(d, "Média de horas trabalhadas na semana", color = "green")
  })
  
  output$box_2 <- shinydashboard::renderValueBox({
    d=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                  Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      summarise(media=paste0(round(weighted.mean(V403312,weight,na.rm=T),2)," R$"))
    valueBox(d, "Média de rendimento bruto", color = "green")
    
  })
  
  output$plot <- renderPlot({
    sexo=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                     Ano %in% input$ano1)%>%
      group_by(V2007)%>%summarise(total=sum(weight))%>%
      mutate(prop=total/sum(total))
    ggplot(data=sexo, aes(x=V2007, y=prop,fill=V2007)) +
      geom_bar(stat="identity")+
      geom_text(aes(label=paste0(round(100*prop,2),"%")), vjust=-0.3, size=3.5)+
      scale_fill_manual(values=c("blue",'#E69F00'))+
      ggtitle("Estimação de Empreendedores segundo sexo")+
      theme(legend.text=element_text(size=12))
  })
  output$plot1=renderPlot({
    chefe=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                      Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      group_by(Chefe)%>%summarise(total=sum(weight))%>%mutate(prop=total/sum(total))
    ggplot(chefe,aes(x="",y=prop,fill=Chefe))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=prop,label=paste0(round(100*prop,2),"%")), position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00'))+
      ggtitle("Estimação de Empreendedores segundo condição no condominio")+
      theme_void()+theme(legend.text=element_text(size=12))
  })
  output$plot2=renderDataTable({
    escolaridade=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                             Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      group_by(V3009A)%>%summarise(total=sum(weight))%>%
      mutate(prop=paste0(100*round(total/sum(total),3),"%"))%>%arrange(desc(prop))
    escolaridade$V3009A=ifelse(is.na(escolaridade$V3009A),"Ignorado",as.character(escolaridade$V3009A))
    escolaridade=rbind(escolaridade[escolaridade$V3009A!="Ignorado",],
                       escolaridade[escolaridade$V3009A=="Ignorado",])
    names(escolaridade)=c("Escolaridade","total","Proporção")
    datatable(escolaridade[,c(1,3)],
              rownames = FALSE, 
              options = list(pageLength = 10,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")
                             
              ))%>%
      
      formatStyle(columns = "Escolaridade",target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "Proporção", target = "cell", backgroundColor = "#B0C4DE")
    
  })
  output$plot3=renderPlot({
    idade=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1)%>%
      group_by(Idade1,V2007)%>%summarise(total=sum(weight))
    
    idade=idade%>%group_by(V2007)%>%mutate(prop=paste0(100*round(total/sum(total),3),"%"))%>%
      arrange(desc(prop))
    
    ggplot(data=idade, aes(x=Idade1, y=prop, fill=V2007)) +
      geom_bar(stat="identity", position=position_dodge())+
      xlab("")+ylab("Porcentagem")+
      geom_text(aes(label=prop), vjust=1.6,
                position = position_dodge(0.9), size=3)+
      scale_fill_manual(values=c("blue",'#E69F00'),name="")+
      theme_minimal()+
      ggtitle("Percentual de Empreendedores por sexo e idade")+
      theme(legend.text=element_text(size=12))
  })
  output$plot4=renderPlot({
    anos.empre=x1%>%filter(V2010 %in% input$raca1,UF %in% input$uf1, Idade1 %in% input$idade1,
                           Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      group_by(V4040)%>%summarise(n=n())%>%
      mutate(prop=paste0(round(100*n/sum(n),2),"%"))
    
    ggplot(data=anos.empre, aes(x=V4040, y=prop,fill=V4040)) +
      geom_bar(stat="identity")+
      xlab("")+
      geom_text(aes(label=prop), vjust=-0.3, size=3.5)+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE','#f6de4b'),
                        name = "")+
      ggtitle("Tempo que estava nesse trabalho")+
      theme(legend.text=element_text(size=12))
  })
  output$plot5=renderPlot({
    emp.uf=x1%>%filter(V2010 %in% input$raca1, Idade1 %in% input$idade1,
                       Ano %in% input$ano1, V2007 %in% input$sexo1)%>%
      group_by(Região)%>%summarise(n=n())%>%
      mutate(prop=paste0(round(100*n/sum(n),2),"%"))
    
    ggplot(emp.uf,aes(x="",y=prop,fill=Região))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=prop,label=prop), position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE','#f6de4b',"darkblue"))+
      ggtitle("Estimação de Empreendedores segundo Região")+
      theme_void()+
      theme(legend.text=element_text(size=12))
  })
  
  ############################ TABPANEL 2
  output$plot6=renderPlot({
    cnpj=x1%>%group_by(V4019)%>%
      filter(V2010 %in% input$raca2, Idade1 %in% input$idade2,
             Ano %in% input$ano2, V2007 %in% input$sexo2)%>%
      summarise(n=sum(weight))%>%
      mutate(prop=paste0(round(100*n/sum(n),2),"%"))
    
    ggplot(cnpj,aes(x="",y=prop,fill=V4019))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=prop,label=prop), position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE'),name = "Possui CNPJ")+
      ggtitle("Estimação de Empreendedores com CNPJ")+
      theme_void()+
      theme(legend.text=element_text(size=12))
  })
  output$plot7=renderPlot({
    cnpj1=x1%>%
      filter(V2010 %in% input$raca2, Idade1 %in% input$idade2,
             Ano %in% input$ano2, V2007 %in% input$sexo2)%>%
      group_by(V4019)%>%
      summarise(media=weighted.mean(V403312,weight,na.rm=T))
    ggplot(data=cnpj1, aes(x=V4019, y=media,fill=V4019)) +
      geom_bar(stat="identity")+
      xlab("")+
      geom_text(aes(label=paste0(round(media,2)," R$")), vjust=-0.3, size=3.5)+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE','#f6de4b'),
                        name = "")+
      ggtitle("Rendimento bruto mensal que recebia normalmente nesse trabalho por CNPJ")+
      theme(plot.title = element_textbox_simple(),legend.text=element_text(size=12))
    
  })
  output$box_3 <- shinydashboard::renderValueBox({
    d=x1%>%filter(V2010 %in% input$raca2,UF %in% input$uf2, Idade1 %in% input$idade2,
                  Ano %in% input$ano2, V2007 %in% input$sexo2)%>%
      summarise(media=paste0(round(weighted.mean(rendimentos,weight),2)," R$"))
    valueBox(d, "Renda per Capita bruta", color = "green")
  })
  
  
  ############################ TABPANEL 3
  output$plot8=renderPlot({
    z=x1%>%group_by(V4016)%>%
      filter(!is.na(V4016),V2010 %in% input$raca3,UF %in% input$uf3, Idade1 %in% input$idade3,
             Ano %in% input$ano3, V2007 %in% input$sexo3)%>%
      summarise(n=sum(weight))%>%#daqueles que contavam com empregados
      mutate(prop=n/sum(n))%>%arrange(desc(prop))
    
    ggplot(data=z, aes(x=V4016, y=prop,fill=V4016)) +
      geom_bar(stat="identity")+
      xlab("")+
      geom_text(aes(label=paste0(round(prop,2)," %")), vjust=-0.3, size=3.5)+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE','#f6de4b'),
                        name = "")+
      ggtitle("Número de empregados")+
      theme(plot.title = element_textbox_simple(),legend.text=element_text(size=12))
  })
  output$plot9=renderPlot({
    z=x1%>%group_by(socio)%>%
      filter(!is.na(socio),V2010 %in% input$raca3,UF %in% input$uf3, Idade1 %in% input$idade3,
             Ano %in% input$ano3, V2007 %in% input$sexo3)%>%
      summarise(n=sum(weight))%>%#daqueles que contavam com socios
      mutate(prop=n/sum(n))%>%arrange(desc(prop))  
    
    ggplot(z,aes(x="",y=prop,fill=socio))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y", start=0)+
      geom_text(aes(y=round(prop,2),label=paste0(round(100*prop,2)," %")), 
                position = position_stack(vjust = .5))+
      scale_fill_manual(values=c("blue",'#E69F00','#B0C4DE'))+
      ggtitle("Número de sócios")+
      theme_void()+
      theme(legend.text=element_text(size=12))
  })
  
  ############################ TABPANEL 4
  output$table1=renderDT({
    x=data.frame(motivação=c("Fazer a Diferença no Mundo",
                             "Para construir uma grande riqueza ou uma renda muito alta",
                             "Para continuar uma tradição familiar",
                             "Para ganhar a vida porque os empregos são escassos"))
    z=data4%>%filter(gender %in% input$sexo4,yrsurv %in% input$ano4)
    
    x1=as.data.frame(rbind(prop.table(table(z$SU_yyMOT1yes)),
                           prop.table(table(z$SU_yyMOT2yes)),
                           prop.table(table(z$SU_yyMOT3yes)),
                           prop.table(table(z$SU_yyMOT4yes))))
    #Empreendedores novos (ENO)
    x2=as.data.frame(rbind(prop.table(table(z$BB_yyMOT1yes)),
                           prop.table(table(z$BB_yyMOT2yes)),
                           prop.table(table(z$BB_yyMOT3yes)),
                           prop.table(table(z$BB_yyMOT4yes))))
    #Empreendedores estabelecidos (EBO)
    x3=as.data.frame(rbind(prop.table(table(z$TEAyyMOT1yes)),
                           prop.table(table(z$TEAyyMOT2yes)),
                           prop.table(table(z$TEAyyMOT3yes)),
                           prop.table(table(z$TEAyyMOT4yes))))
    x4=cbind(x,`% ENA`=x1[,2],`% ENO`=x2[,2],`% EBO`=x3[,2])
    x4$`% ENA`=round(x4$`% ENA`*100,2)
    x4$`% ENA`=paste0(x4$`% ENA`,"%")
    x4$`% ENO`=round(x4$`% ENO`*100,2)
    x4$`% ENO`=paste0(x4$`% ENO`,"%")
    x4$`% EBO`=round(x4$`% EBO`*100,2)
    x4$`% EBO`=paste0(x4$`% EBO`,"%")
    
    datatable(x4,
              rownames = FALSE, 
              options = list(pageLength = 10, dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")))%>%
      
      formatStyle(columns = "% ENA",target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "% EBO", target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "% ENO", target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "motivação", target = "cell", backgroundColor = "#B0C4DE")
  })
  output$table2=renderDT({
    z=data4%>%filter(gender %in% input$sexo4,yrsurv %in% input$ano4)
    x=data.frame(`Atitudes e Percepções`=c("Conhece alguém que iniciou um novo negócio",
                                           "Boas oportunidades para iniciar um negócio na minha área",
                                           "É fácil começar um negócio",
                                           "Pessoalmente, tem as habilidades e conhecimentos",
                                           "Medo do fracasso",
                                           "Espera iniciar um negócio nos próximos 3 anos?"))
    
    x1=data.frame(`% Adultos`=c(prop.table(table(z$KNOWENyy))[2],#Know someone who has started a new business
                                prop.table(table(z$OPPORTyy))[2],#Good opportunities to start a business in my area
                                prop.table(table(z$EASYSTyy))[2],#It is easy to start a business
                                prop.table(table(z$SUSKILyy))[2],#Personally have the skills and knowledge
                                prop.table(table(z$FRFAILOP))[2],#Fear of failure (opportunity)
                                prop.table(table(z$FUTSUPyy))[2]))#EXPECTS TO START BUSINESS IN THE NEXT THREE YEARS
    x2=as.data.frame(cbind(x,x1))
    x2$X..Adultos=round(x2$X..Adultos*100,2)
    names(x2)=c("Atitudes e Percepções","% Adultos")
    x2$`% Adultos`=paste0(x2$`% Adultos`,"%")
    datatable(x2,
              rownames = FALSE, 
              options = list(pageLength = 10, dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")))%>%
      
      formatStyle(columns = "Atitudes e Percepções",target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "% Adultos", target = "cell", backgroundColor = "#B0C4DE")
    
    
  })
  
  output$table4=renderDT({
    z=data4%>%filter(gender %in% input$sexo4,yrsurv %in% input$ano4)
    p=data.frame(prop.table(table(z$EXIT_RS)))
    p=p%>%mutate_if(is.numeric,~  round(. * 100,2))%>%
      arrange(desc(Freq))%>%
      mutate_if(is.numeric,~paste0(. ,"%"))
    names(p)=c("Razões da saída do negócio","Porcentagem")
    datatable(p,
              rownames = FALSE, 
              options = list(pageLength = 10, dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")))%>%
      
      formatStyle(columns = "Razões da saída do negócio",target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "Porcentagem", target = "cell", backgroundColor = "#B0C4DE")
    
  })
  output$table5=renderDT({
    z=data4%>%filter(gender %in% input$sexo4,yrsurv %in% input$ano4)
    x=data.frame("No meu País"=
                   c("Há muita cobertura de mídia para novos negócios",
                     "As pessoas que crescem um novo negócio de sucesso recebem status alto",
                     "Começar um negócio é considerado uma boa escolha de carreira",
                     "As pessoas preferem o padrão de vida uniforme"))
    x1=data.frame(rbind(prop.table(table(data4$NBMEDIyy)),
                        prop.table(table(data4$NBSTATyy)),
                        prop.table(table(data4$NBGOODyy)),
                        prop.table(table(data4$EQUALIyy))))
    x2=as.data.frame(cbind(x,x1))
    x2=x2%>%mutate_if(is.numeric,~ round(. * 100,2))
    x2=x2%>%mutate_if(is.numeric,~paste0(. ,"%"))
    names(x2)=c("No meu País","Discordo","Concordo")
    datatable(x2,
              rownames = FALSE, 
              options = list(pageLength = 10, dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'darkblue', 'color':'#E69F00'});",
                               "}")))%>%
      
      formatStyle(columns = "No meu País",target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "Discordo", target = "cell", backgroundColor = "#B0C4DE")%>%
      formatStyle(columns = "Concordo", target = "cell", backgroundColor = "#B0C4DE")
  })
}
shinyApp(ui,server)

