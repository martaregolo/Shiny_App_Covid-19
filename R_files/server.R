library(shiny)
library(ggpubr)
library(scales)
library(xts)
library(dygraphs)
library(RColorBrewer)

#setwd("C:/Users/secon/Documents/R/TESI/tesi")

function(session,input, output) {
        #TAB Italia 
        output$map <- renderLeaflet({
                day<-input$day
                request<-filter(region_dataset,region_dataset$data==day)
                
                Italia<-merge(Italia,request,by="denominazione_regione")
                Nord<-merge(Nord, request,by="denominazione_regione")
                Centro<-merge(Centro, request,by="denominazione_regione")
                Sud_e_Isole<-merge(Sud_e_Isole, request,by="denominazione_regione")
                
                bins <- unique(c(0,(as.vector(quantile(request[[gsub(" ","_",input$variabile)]],probs=c(0,0.4,0.6,0.75,0.85,0.98,1))))))
                # bins<-unique(c(as.numeric(scientific(bins, digits=1)),ceiling(as.numeric(scientific(max(request[[gsub(" ","_",input$variabile)]],digits=2)))+5),floor(as.numeric(scientific(min(request[[gsub(" ","_",input$variabile)]],digits=2))))))
                bins<-unique(c(as.numeric(scientific(bins, digits=1)),max(ceiling(as.numeric(scientific(bins,digits=2))),max(bins)),min(floor(as.numeric(scientific(bins,digits=2)))),min(bins)))
                bins<-sort(bins)
                pal <- colorBin("YlOrRd", domain = request[[gsub(" ","_",input$variabile)]], bins = bins)
                
                dat<-get(gsub(" ","_",input$scope))
                
                leaflet()%>%
                        addTiles()%>%
                        addPolygons(data=dat,fillColor=~pal(dat@data[[gsub(" ","_",input$variabile)]]),label=paste(dat@data$denominazione_regione," - ", input$variabile,": ",dat@data[[gsub(" ","_",input$variabile)]]),weight=0.5,opacity=1,color="grey",fillOpacity = 0.5,highlightOptions = highlightOptions(color = "white",weight = 2,bringToFront = TRUE))%>%
                        leaflet::addLegend(pal=pal, values=dat@data[[gsub(" ","_",input$variabile)]],title=paste(input$scope,"-", input$variabile))
        })
        
        output$plot1<-renderDygraph({
                request<-area
                if(input$variabile != "casi testati" && input$variabile!= "variazione casi testati"&& input$variabile!= "p casi testati"&& input$variabile!= "p nuovi positivi" && input$variabile!= "p casi testati"&& input$variabile!= "n casi testati normalizzato"){
                        if(input$variabile=="p terapia intensiva"){
                                ita<-aggregate(request[["terapia_intensiva"]], by=list(Data=request$data), FUN= sum)
                                ita<-mutate(ita, x=x/tot_beds)
                        }
                        else if(input$variabile == "n positivi giornalieri normalizzato"){           
                                ita<-aggregate(request[["nuovi_positivi"]], by=list(Data=request$data), FUN= sum)
                                ita<-mutate(ita, x=x*10^5/tot_popolazione) 
                        }
                        else if(input$variabile == "p isolamento domiciliare"){           
                                ita<-aggregate(request[["isolamento_domiciliare"]], by=list(Data=request$data), FUN= sum)
                                ita2<-aggregate(request[["totale_positivi"]], by=list(Data=request$data), FUN= sum)
                                it<-cbind(ita,ita2$x)
                                names(it)[3]<-"x2"
                                ita<-(mutate(it, x=x/x2))[,1:2]
                        }
                        else if(input$variabile == "p positivi"){           
                                ita<-aggregate(request[["nuovi_positivi"]], by=list(Data=request$data), FUN= sum)
                                ita2<-aggregate(request[["variazione_tamponi"]], by=list(Data=request$data), FUN= sum)
                                it<-cbind(ita,ita2$x)
                                names(it)[3]<-"x2"
                                ita<-(mutate(it, x=x/x2))[,1:2]
                        }
                        else{
                                ita<-aggregate(request[[gsub(" ","_",input$variabile)]], by=list(Data=request$data), FUN= sum)
                        }
                }
                else{
                        ita<-filter(request,data > "2020/04/18")
                        if(input$variabile == "p casi testati"){
                                ita<-aggregate(ita[["casi_testati"]], by=list(Data=request$data[-(1:1155)]), FUN= sum)
                                ita<-mutate(ita, x=x/tot_popolazione)
                        }
                        else if(input$variabile == "n casi testati normalizzato"){ 
                                ita<-aggregate(ita[["variazione_casi_testati"]], by=list(Data=request$data[-(1:1155)]), FUN= sum) #per data sommo la variazione totale
                                ita<-mutate(ita, x=x*10^5/tot_popolazione) 
                                ita$x<-round(ita$x,2)
                        }
                        else if(input$variabile == "p nuovi positivi"){ 
                                ita1<-aggregate(ita[["nuovi_positivi"]], by=list(Data=request$data[-(1:1155)]), FUN= sum) 
                                ita2<-aggregate(ita[["variazione_casi_testati"]], by=list(Data=request$data[-(1:1155)]), FUN= sum) #per data sommo la variazione totale
                                it<-cbind(ita1,ita2$x)
                                names(it)[3]<-"x2"
                                ita<-(mutate(it, x=x/x2))[,1:2]
                        }
                        else{
                                ita<-aggregate(ita[[gsub(" ","_",input$variabile)]], by=list(Data=request$data[-(1:1155)]), FUN= sum)
                        }}
                
                ita<-xts(x=ita$x,order.by=ita$Data)
                names(ita)<-paste(input$variabile,"_Italia")
                
                if(input$scope != "Italia"){
                        r<-filter(request,area==input$scope)
                        pop<-sum(unique(r$popolazione))
                        beds<-sum(unique(r$posti_terapia_intensiva))
                        
                        if(input$variabile != "casi testati"&& input$variabile!= "variazione casi testati"&& input$variabile!= "p casi testati"&& input$variabile!= "p nuovi positivi" && input$variabile!= "n casi testati normalizzato"){
                                if(input$variabile=="p terapia intensiva"){
                                        r<-aggregate(r[["terapia_intensiva"]], by=list(Data=r$data), FUN= sum)
                                        r<-mutate(r,x=x/beds)
                                }
                                
                                else if(input$variabile == "n positivi giornalieri normalizzato"){
                                        r<-aggregate(r[["nuovi_positivi"]], by=list(Data=r$data), FUN= sum)
                                        r<-mutate(r,x=x*10^5/pop)
                                }
                                else if(input$variabile == "p isolamento domiciliare"){ 
                                        r1<-aggregate(r[["isolamento_domiciliare"]], by=list(Data=r$data), FUN= sum)
                                        r2<-aggregate(r[["totale_positivi"]], by=list(Data=r$data), FUN= sum)
                                        r2<-cbind(r1,r2$x)
                                        names(r2)[3]<-"x2"
                                        r<-(mutate(r2, x=x/x2))[,1:2]
                                }
                                else if(input$variabile == "p positivi"){  
                                        r<-filter(r,data>"2020/02/25")
                                        r1<-aggregate(r[["nuovi_positivi"]], by=list(Data=r$data), FUN= sum)
                                        r2<-aggregate(r[["variazione_tamponi"]], by=list(Data=r$data), FUN= sum)
                                        r2<-cbind(r1,r2$x)
                                        names(r2)[3]<-"x2"
                                        r<-(mutate(r2, x=x/x2))[,1:2]
                                }
                                else{
                                        r<-aggregate(r[[gsub(" ","_",input$variabile)]], by=list(Data=r$data), FUN= sum)
                                }
                        }
                        else{
                                r<-filter(r,data > "2020/04/18")
                                if(input$variabile == "p casi testati"){
                                        r<-aggregate(r[["casi_testati"]], by=list(Data=r$data), FUN= sum)
                                        r<-mutate(r, x=x/pop)
                                }
                                else if(input$variabile == "n casi testati normalizzato"){
                                        r<-aggregate(r[["variazione_casi_testati"]], by=list(Data=r$data), FUN= sum)
                                        r<-mutate(r,x=x*10^5/pop)
                                        r$x<-round(r$x)
                                }
                                else if(input$variabile == "p nuovi positivi"){ 
                                        r1<-aggregate(r[["nuovi_positivi"]], by=list(Data=r$data), FUN= sum) 
                                        r2<-aggregate(r[["variazione_casi_testati"]], by=list(Data=r$data), FUN= sum) #per data sommo la variazione totale
                                        r2<-cbind(r1,r2$x)
                                        names(r2)[3]<-"x2"
                                        r2[1:2,3]<-0
                                        r<-(mutate(r2, x=x/x2))[,1:2]
                                        
                                }
                                else{
                                        r<-aggregate(r[[gsub(" ","_",input$variabile)]], by=list(Data=r$data), FUN= sum)
                                }
                        }
                        
                        r<-xts(x=r$x,order.by=r$Data)
                        names(r)<-input$variabile
                        r<-merge.xts(r,ita)
                        
                        dygraph(r, main= input$variabile) %>%
                                dyRangeSelector()%>%
                                dySeries(gsub(" ",".",input$variabile),label=input$scope)%>%
                                dySeries(paste(gsub(" ",".",input$variabile),"._Italia",sep=""), label="Italia")%>%
                                #dyOptions(sigFigs = 7)#%>%
                                dyAxis(
                                        "y",
                                        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                                        axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
                                )
                }
                else{
                        dygraph(ita, main= input$variabile) %>%
                                dyRangeSelector()%>%
                                dySeries(paste(input$variabile,"_Italia"), label="Italia", color="navy" ) %>%
                                #dyOptions(sigFigs = 7)#%>%
                                dyAxis(
                                        "y",
                                        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
                                        axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
                                )
                }
        })
        
        output$plot2<-renderPlotly({
                
                request<-area
                day<-input$day
                request<-filter(request,data==day)
                x<-request[request$area=="Nord",]
                x<-arrange(x,x[[gsub(" ","_",input$variabile)]])
                y<-request[request$area=="Centro",]
                y<-arrange(y,y[[gsub(" ","_",input$variabile)]])
                z<-request[request$area=="Sud e Isole",]
                z<-arrange(z,z[[gsub(" ","_",input$variabile)]])
                request<-rbind(x,y,z)
                #condition= "input.variabile == 'casi testati'"
                plot_ly(request,x=request$denominazione_regione,y=request[[gsub(" ","_",input$variabile)]],  
                        type="bar", split = request$area,color= request$area, colors=colorRampPalette(brewer.pal(3,"Dark2"))(3) ) %>%
                        layout( xaxis=list(categoryorder="array",categoryarray=request[[gsub(" ","_",input$variabile)]]), yaxis=list(tickformat=".4r"))
                
        }) 
        
        output$text<-renderText({"ATTENZIONE: Selezionare un'altra data!"})
        
        output$text1<-renderText({"Il dato dei casi testati e' disponibile dal 19 Aprile 2020"})
        
        ###############################################################################################################
        #TAB Monitoring
        
        output$target <-renderUI({
                sliderInput("target","Valore target",min=0,max=as.integer(2*mean((filter(select(filter(region_dataset,denominazione_regione==input$reg),gsub(" ", "_", input$var),data),data>"2020/05/03"))[[gsub(" ","_",input$var)]])),value=mean((filter(select(filter(region_dataset,denominazione_regione==input$reg),gsub(" ", "_", input$var),data),data>"2020/05/03"))[[gsub(" ","_",input$var)]]),step=1)
        })
        #fase2
        observe({
                date<-as.Date(input$date1)
                if(input$var!="n casi testati normalizzato"){
                updateDateInput(session,"date2",min=date+1,max=region_dataset$data[nrow(region_dataset)])}
                else{
                updateDateInput(session,"date2",min=date+8,max=region_dataset$data[nrow(region_dataset)])   
                }
                
         })
        #fase1
        observe({
                date<-as.Date(input$date2)
                if(input$var!="n casi testati normalizzato"){
                updateDateInput(session,"date1",min="2020/05/04",max=date-1)}
                else{
                updateDateInput(session,"date1",min="2020/05/04",max=date-8)   
                }
        })
        d2<-1.128
        D3<-0
        D4<-3.267
        
        #c chart
        output$chart1 <- renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data)
                request<-filter(request, data>"2020/05/03")
                request<-request[request$nuovi_positivi>=0,]
                requestF1<-filter(request,data>=input$date1 & data<input$date2)
                
                if(input$reg=="Lazio"){
                        
                        requestF1<-filter(requestF1,data<"2020/06/06"|data>"2020/06/13")
                }
                r<-xts(x=request[[gsub(" ","_",input$var)]],order.by=request$data)
                names(r)<-input$var
                
                c<-mean(requestF1[[gsub(" ","_",input$var)]])
                UCL<-c+3*sqrt(c)
                LCL<-max(0,c-3*sqrt(c))
               
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                    if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                      control<-filter(control,data!=requestF1[i,2])
                      c<-mean(control[["nuovi_positivi"]])
                      UCL<-c+3*sqrt(c)
                      LCL<-max(0,c-3*sqrt(c))
                    }
                }
                
                dygraph(r, main= paste(input$var,"-",input$reg)) %>%
                        dySeries(input$var)%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2)%>%
                        dyLimit(c,label="Central Line",labelLoc = "left")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(request[[gsub(" ","_",input$var)]])),max(UCL+5,max(request[[gsub(" ","_",input$var)]]))+20))%>%
                        dyShading(from=input$date1,to=(input$date2-1),color="rgba(255,215,0,0.4)")#%>%
                        #dyAnnotation(input$date1,"Fase 1",width=60,height=25,attachAtBottom = TRUE)
                                     
        })
        
        output$chart2<-renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                request<-request[request$n_positivi_giornalieri_normalizzato>=0,]
                requestF1<-filter(request,data>=input$date1 & data<input$date2)
                if(input$reg=="Lazio"){
                        
                        requestF1<-filter(requestF1,data<"2020/06/06"|data>"2020/06/13")
                }
                n<-unique((request$popolazione/10^5))
                u<-request$n_positivi_giornalieri_normalizzato
                r<-xts(x=u,order.by=request$data)
                names(r)<-"Nuovi positivi per 100000 abitanti"
                c<-mean(requestF1$n_positivi_giornalieri_normalizzato) 
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
                control<-requestF1
                
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[["n_positivi_giornalieri_normalizzato"]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                
                dygraph(r, main= paste(input$var,"-",input$reg)) %>%
                        dySeries("Nuovi positivi per 100000 abitanti")%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2,digitsAfterDecimal = 5)%>%
                        dyLimit(c,label="Central Line",labelLoc = "left")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(u)),max(UCL+0.001,max(u)+0.0005)))%>%
                        dyShading(from=input$date1,to=input$date2,color="rgba(255,140,0,0.4)")

        })
        
        output$text7<-renderText({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                request<-request[request$n_positivi_giornalieri_normalizzato>=0,]
                requestF1<-filter(request,data>=input$date1 & data<input$date2)
                g<-0
                if(input$reg=="Lazio"){
                        g<-8
                        requestF1<-filter(requestF1,data<"2020/06/06"|data>"2020/06/13")
                }
                n<-unique((request$popolazione/10^5))
                c<-mean(requestF1[[gsub(" ","_",input$var)]])
                UCL<-c+3*sqrt(c/n)
                LCL<-max(0,c-3*sqrt(c/n))
                f<-0+g
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                                f<-f+1
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[[gsub(" ", "_", input$var)]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                if(f>1 || f==0){
                        str<-"Sono stati eliminati"
                        str2<-"punti per il calcolo dei limiti di controllo."}
                if(f==1){
                        str<-"E' stato eliminato"
                        str2<-"punto per il calcolo dei limiti di controllo."      
                }
                
                paste(str,f,str2)
                
        })
        
        
        
        output$text6<-renderText({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data)
                request<-filter(request, data>"2020/05/03")
                request<-request[request$nuovi_positivi>=0,]
                requestF1<-filter(request,data>=input$date1 & data<input$date2)
                g<-0
                if(input$reg=="Lazio"){
                        g<-8
                        requestF1<-filter(requestF1,data<"2020/06/06"|data>"2020/06/13")
                }
                c<-mean(requestF1[[gsub(" ","_",input$var)]])
                UCL<-c+3*sqrt(c)
                LCL<-max(0,c-3*sqrt(c))
                f<-0+g
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                                f<-f+1
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[[gsub(" ", "_", input$var)]])
                                UCL<-c+3*sqrt(c)
                                LCL<-max(0,c-3*sqrt(c))
                        }
                }
                if(f>1 || f==0){
                str<-"Sono stati eliminati"
                str2<-"punti per il calcolo dei limiti di controllo."}
                if(f==1){
                        str<-"E' stato eliminato"
                        str2<-"punto per il calcolo dei limiti di controllo."      
                }
                
                paste(str,f,str2)
                
        })

        #u control chart - TARGET
        output$chart3<-renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request<-filter(request,data!="2020/05/07")
                }
                requestF1<-filter(request, data>=input$date1 & data<input$date2)
                n<-unique((request$popolazione/10^5))
                t<-input$target #u medio
                u<-request$n_casi_testati_normalizzato
                r<-xts(x=u,order.by=request$data)
                names(r)<-"Average n^ nonconformities"
                
                c<-t #u medio
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
                dygraph(r, main= paste("U Control Chart - target <br>",input$var,"-",input$reg)) %>%
                        dySeries("Average n^ nonconformities")%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2,digitsAfterDecimal = 5)%>%
                        dyLimit(c,label="Central Line",labelLoc = "right")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(u)),max(UCL+0.001,max(u)+0.0005)))%>%
                        dyShading(from=input$date1,to=input$date2,color="rgba(255,127,80,0.4)")

        })
        
        #u chart
        output$chart4<-renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request<-filter(request,data!="2020/05/07")
                }
                requestF1<-filter(request, data>=input$date1 & data<input$date2)
                n<-unique((request$popolazione/10^5))
                u<-request$n_casi_testati_normalizzato
                r<-xts(x=u,order.by=request$data)
                names(r)<-"Casi testati per 100000 abitanti"
                
                c<-mean(requestF1$n_casi_testati_normalizzato) #u medio fase1
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[["n_casi_testati_normalizzato"]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                
                
                dygraph(r, main= paste("U Control Chart <br>",input$var,"-",input$reg)) %>%
                        dySeries("Casi testati per 100000 abitanti")%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2,digitsAfterDecimal = 5)%>%
                        dyLimit(c,label="Central Line",labelLoc = "right")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(u)),max(UCL+0.001,max(u)+0.0005)))%>%
                        dyShading(from=input$date1,to=input$date2,color="rgba(255,127,80,0.4)") 
        })
        
        output$text8<-renderText({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request<-filter(request,data!="2020/05/07")
                }
                requestF1<-filter(request,data>=input$date1 & data<input$date2)
                
                
                n<-unique((request$popolazione/10^5))
                c<-mean(requestF1[[gsub(" ","_",input$var)]])
                UCL<-c+3*sqrt(c/n)
                LCL<-max(0,c-3*sqrt(c/n))
                f<-0
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,1]>1.5*UCL || requestF1[i,1]<0.5*LCL){
                                f<-f+1
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[[gsub(" ", "_", input$var)]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                if(f>1 || f==0){
                        str<-"Sono stati eliminati"
                        str2<-"punti per il calcolo dei limiti di controllo."}
                if(f==1){
                        str<-"E' stato eliminato"
                        str2<-"punto per il calcolo dei limiti di controllo."      
                }
                
                paste(str,f,str2)
                
        })
        
        output$text5<-renderUI({
                str1<-"La fase di Monitoring inizia il"
                str2<-"4 Maggio 2020"
                HTML(paste(str1,paste0("<b>",str2),sep='<br>'))
                })
        
        output$chart5<-renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request$n_casi_testati_normalizzato[4]<-29.5
                }
                casi<-rollapply(request$n_casi_testati_normalizzato,7,mean,by=7)
                data<-as.Date(unique(cut(as.Date(request$data),"week")))
                if(nrow(request)%%7!=0){
                        ok<-tail(request,n=nrow(request)%%7)
                        mean<-mean(ok$n_casi_testati_normalizzato)
                        c<-cbind(mean,as.data.frame(ok[1,2]))
                        casi[length(casi)+1]<-c$mean
                }
                ci<-as.data.frame(data)
                ci[ 2]<-casi
                names(ci)<-c("data","media_n_casi_testati_normalizzato")
                requestF1<-filter(ci, data>=input$date1 & data<input$date2)
                n<-unique((request$popolazione/10^5))
                u<-round(ci$media_n_casi_testati_normalizzato,2)
                r<-xts(x=u,order.by=ci$data)
                names(r)<-"Media casi testati per 100000 abitanti"
                c<-round(mean(requestF1$media_n_casi_testati_normalizzato),2) #u medio fase1
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
                control<-requestF1
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,2]>1.2*UCL || requestF1[i,2]<0.8*LCL){
                                control<-filter(control,data!=requestF1[i,2])
                                c<-mean(control[["media_n_casi_testati_normalizzato"]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                dygraph(r, main= paste("U Control Chart","-",input$reg)) %>%
                        dySeries("Media casi testati per 100000 abitanti")%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2,digitsAfterDecimal = 5)%>%
                        dyLimit(c,label="Central Line",labelLoc = "right")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(u)),max(UCL+0.001,max(u)+0.0005)))%>%
                        dyShading(from=input$date1,to=input$date2,color="rgba(255,127,80,0.4)") 
        })
        
        output$chart6<-renderDygraph({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request$n_casi_testati_normalizzato[4]<-29.5
                }
                casi<-rollapply(request$n_casi_testati_normalizzato,7,mean,by=7)
                data<-as.Date(unique(cut(as.Date(request$data),"week")))
                if(nrow(request)%%7!=0){
                        ok<-tail(request,n=nrow(request)%%7)
                        mean<-mean(ok$n_casi_testati_normalizzato)
                        c<-cbind(mean,as.data.frame(ok[1,2]))
                        casi[length(casi)+1]<-c$mean
                }
                ci<-as.data.frame(data)
                ci[ 2]<-casi
                names(ci)<-c("data","media_n_casi_testati_normalizzato")
                requestF1<-filter(ci, data>=input$date1 & data<input$date2)
                n<-unique((request$popolazione/10^5))
                u<-round(ci$media_n_casi_testati_normalizzato,2)
                r<-xts(x=u,order.by=ci$data)
                names(r)<-"Media casi testati per 100000 abitanti"
                c<-input$target #u medio
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
        
                
                dygraph(r, main= paste("U Control Chart","-",input$reg)) %>%
                        dySeries("Media casi testati per 100000 abitanti")%>%
                        dyOptions(drawPoints = TRUE, pointSize = 2,digitsAfterDecimal = 5)%>%
                        dyLimit(c,label="Central Line",labelLoc = "right")%>%
                        dyLimit(UCL,label="Upper Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyLimit(LCL,label="Lower Control Limit",labelLoc = "left",strokePattern = "solid")%>%
                        dyAxis("y",valueRange=c(min(LCL,min(u)),max(UCL+0.001,max(u)+0.0005)))%>%
                        dyShading(from=input$date1,to=input$date2,color="rgba(255,127,80,0.4)") 
                
        })
        
        output$text9<-renderText({
                request<-filter(region_dataset, denominazione_regione==input$reg)
                request<-select(request,gsub(" ", "_", input$var),data,popolazione)
                request<-filter(request, data>"2020/05/03")
                if(input$reg=="Puglia"){
                        request$n_casi_testati_normalizzato[4]<-29.5
                }
                casi<-rollapply(request$n_casi_testati_normalizzato,7,mean,by=7)
                data<-as.Date(unique(cut(as.Date(request$data),"week")))
                if(nrow(request)%%7!=0){
                        ok<-tail(request,n=nrow(request)%%7)
                        mean<-mean(ok$n_casi_testati_normalizzato)
                        c<-cbind(mean,as.data.frame(ok[1,2]))
                        casi[length(casi)+1]<-c$mean
                }
                ci<-as.data.frame(data)
                ci[ 2]<-casi
                names(ci)<-c("data","media_n_casi_testati_normalizzato")
                requestF1<-filter(ci, data>=input$date1 & data<input$date2)
                n<-unique((request$popolazione/10^5))
                u<-round(ci$media_n_casi_testati_normalizzato,2)
                r<-xts(x=u,order.by=ci$data)
                names(r)<-"Media casi testati per 100000 abitanti"
                c<-round(mean(requestF1$media_n_casi_testati_normalizzato),2) #u medio fase1
                LCL<-max(0,c-(3*sqrt(c/n)))
                UCL<-c+(3*sqrt(c/n))
                control<-requestF1
                f<-0
                for (i in 1: nrow(requestF1)){
                        if(requestF1[i,2]>1.2*UCL || requestF1[i,2]<0.8*LCL){
                                control<-filter(control,data!=requestF1[i,2])
                                f<-f+1
                                c<-mean(control[["media_n_casi_testati_normalizzato"]])
                                UCL<-c+3*sqrt(c/n)
                                LCL<-max(0,c-3*sqrt(c/n))
                        }
                }
                if(f>1 || f==0){
                        str<-"Sono stati eliminati"
                        str2<-"punti per il calcolo dei limiti di controllo."}
                if(f==1){
                        str<-"E' stato eliminato"
                        str2<-"punto per il calcolo dei limiti di controllo."      
                }
                
                paste(str,f,str2)
                
        })
        
}
