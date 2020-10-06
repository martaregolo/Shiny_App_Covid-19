library(geojsonio)
library(broom)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(leaflet)
library(sp)
library(maptools)
library (readr)
library(stringr)


#read the Italian population at Jan 1st 2019 (ISTAT SOURCE)
popolazioneMF_df=read.csv(url("https://raw.githubusercontent.com/martaregolo/Shiny_App_Covid-19/master/PopolazioneRegioni20190101.csv"), header=TRUE)
popolazione_df=subset(popolazioneMF_df, select = -c(Codice_Regione,Totale_Maschi,Totale_Femmine))
names(popolazione_df)=c("denominazione_regione","popolazione") #change names to fit to region_dataset

tot_popolazione<-sum(popolazione_df[,2])/2

#read the number of IC beds at Apr 29th 2020 (Ministero della Saule SOURCE)
IC_beds_df=read.csv(url("https://raw.githubusercontent.com/martaregolo/Shiny_App_Covid-19/master/LettiTerapiaIntensiva20200429.csv"), header=TRUE)
IC_beds_df=subset(IC_beds_df, select = -c(codice_regione))

tot_beds<-sum(IC_beds_df[,2])

#read from github the entire dataset of observations by region up to the current day (REGIONS)
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
#urlfile="https://raw.githubusercontent.com/martha-ruler/Tesi-COVID-19/master/dpc-covid19-ita-regioni.csv"
region_dataset<-read.csv(url(urlfile))
region_dataset<-select(region_dataset,-21)

#eliminate the hh:mm from the field "date"
region_dataset$data=as.Date(region_dataset$data, "%Y-%m-%d",tz="")

#add regional population and number of IC beds at each record
region_dataset<-merge(region_dataset,popolazione_df,by="denominazione_regione")
region_dataset<-merge(region_dataset,IC_beds_df,by="denominazione_regione")

#add variation for "terapia intensiva" at each record
region_dataset<-arrange(region_dataset,denominazione_regione, data)
region_dataset<-mutate(region_dataset,variazione_terapia_intensiva= region_dataset$terapia_intensiva - lag(region_dataset$terapia_intensiva))
for (i in 1: nrow(region_dataset)){
        if(region_dataset$data[i]=="2020/02/24"){
                region_dataset$variazione_terapia_intensiva[i]<-0
        }
}

#compute the ratio "terapia_intensiva/posti_TI" and add new column
region_dataset<-mutate(region_dataset, p_terapia_intensiva=round(terapia_intensiva/posti_terapia_intensiva,3))

#add variation for "casi testati" at each record
region_dataset<-mutate(region_dataset,variazione_casi_testati= region_dataset$casi_testati - lag(region_dataset$casi_testati))
for (i in 1: nrow(region_dataset)){
        if(region_dataset$data[i]=="2020/02/24"){
                region_dataset$variazione_casi_testati[i]<-0
        }
}

#compute the cumulative ratio "casi_testati/popolazione" and add new column
region_dataset<-mutate(region_dataset, p_casi_testati=round(casi_testati/popolazione,4))

#computes the daily number of newly tested peoplex100000 residents and attachs columns to the dataset
region_dataset<-mutate(region_dataset,n_casi_testati_normalizzato=round(10^5*variazione_casi_testati/popolazione,0))

#computes the ratio "isolamento_domiciliare/totale_positivi" and attachs columns to the dataset
region_dataset<-mutate(region_dataset,p_isolamento_domiciliare=round(isolamento_domiciliare/totale_positivi,3))
for (i in 1: nrow(region_dataset)){
        if((is.infinite(region_dataset$p_isolamento_domiciliare[i])) || (is.nan(region_dataset$p_isolamento_domiciliare[i]))|| (is.na(region_dataset$p_isolamento_domiciliare[i]))){  #questa cosa solo perchè non c'è ancora il conditional panel in regioni
                region_dataset$p_isolamento_domiciliare[i]<-0
        }
}

#computes the daily number of new positivesx100000 residents and attachs columns to the dataset
region_dataset<-mutate(region_dataset,n_positivi_giornalieri_normalizzato=10^5*nuovi_positivi/popolazione)

#compute the ratio "nuovi_positivi/variazione_casi_testati and add new column
region_dataset<-mutate(region_dataset, p_nuovi_positivi=round(nuovi_positivi/variazione_casi_testati,4))
#if we have 0/0 or something/0
for (i in 1: nrow(region_dataset)){
        if((is.infinite(region_dataset$p_nuovi_positivi[i])) || (is.nan(region_dataset$p_nuovi_positivi[i]))|| (is.na(region_dataset$p_nuovi_positivi[i]))){ #il na solo perchè non c'è ancora il conditional panel in regioni
                region_dataset$p_nuovi_positivi[i]<-0
        }
}

#evaluate variation for "Tamponi" and attachs columns to the dataset
region_dataset<-mutate(region_dataset,variazione_tamponi=region_dataset$tamponi-lag(region_dataset$tamponi))
for (i in 1: nrow(region_dataset)){
        if(region_dataset$data[i]=="2020/02/24"){
                region_dataset$variazione_tamponi[i]<-0
        }
}

#computes the ratio "nuovi_positivi/diff_TMP" and attachs columns to the dataset
region_dataset<-mutate(region_dataset,p_positivi=round(nuovi_positivi/variazione_tamponi,4))
#if we have 0/0 or something/0
for (i in 1: nrow(region_dataset)){
        if((is.infinite(region_dataset$p_positivi[i])) || (is.nan(region_dataset$p_positivi[i]))|| (is.na(region_dataset$p_positivi[i]))){
                region_dataset$p_positivi[i]<-0
        }
}

#color for terapia intensiva
region_dataset<-mutate(region_dataset,col=case_when(variazione_terapia_intensiva>0 ~ 'red',variazione_terapia_intensiva<=0 ~ 'green' ))

#import geojson file 
dat_p<-geojson_read("https://gist.githubusercontent.com/datajournalism-it/212e7134625fbee6f9f7/raw/dabd071fe607f5210921f138ad3c7276e3841166/province.geojson", what="sp")
dat_r<-geojson_read("https://gist.githubusercontent.com/datajournalism-it/48e29e7c87dca7eb1d29/raw/2636aeef92ba0770a073424853f37690064eb0ea/regioni.geojson",what="sp")
names(dat_r@data)<-"denominazione_regione"
names(dat_p@data)<-"denominazione_provincia"
dat_r@data[["denominazione_regione"]]<-gsub("Trentino-Alto Adige","P.A. Bolzano",dat_r@data[["denominazione_regione"]])
dat_r@polygons[[4]]@Polygons[[1]]@coords<-dat_p@polygons[[19]]@Polygons[[1]]@coords #replace Trentino coordinates with P.A.Bolzano's ones

#extract geojson P.A. Trento and creation SpatialPolygonsDataFrame
coord<-dat_p@polygons[[23]]@Polygons[[1]]@coords
p=Polygon(coord) #sp with coordinates
p<-Polygons(list(p),1)
p=SpatialPolygons(list(p)) 
p<-SpatialPolygonsDataFrame(p,data.frame(denominazione_regione=as.character("P.A. Trento"))) #assign name
p@polygons[[1]]@ID<-"21" #assign ID
assign(paste0("region_", p@data[["denominazione_regione"]]), p) #create sp named P.A. Trento

#initialize sp "regioni"
Italia<-assign(paste0(p@data[["denominazione_regione"]]), p)

#create sp for each region and an unique sp called "Italia" with all coords and data
for (i in 1:20){
        coord<-dat_r@polygons[[i]]@Polygons[[1]]@coords #coordinate
        p=Polygon(coord) #crea oggetto Polygon su coordinate
        p<-Polygons(list(p),1)
        p=SpatialPolygons(list(p)) #crea oggetto SpatialPolygons
        data<-data.frame(denominazione_regione=dat_r@data[["denominazione_regione"]][i]) #crea dataframe con nome regione
        p<-SpatialPolygonsDataFrame(p,data) #crea oggetto SpatialPolygonsDataFrame
        p@polygons[[1]]@ID<-as.character(i) #ID regione
        assign(paste0(p@data[["denominazione_regione"]]), p) #rinomina colonna data
        Italia <- rbind(Italia, assign(paste0(p@data[["denominazione_regione"]]), p))
}

#create sp for the Italy's zones
Nord <- rbind(`P.A. Trento`,Piemonte,`Valle d'Aosta`, Lombardia, `P.A. Bolzano`, Veneto, `Friuli Venezia Giulia`, Liguria, `Emilia-Romagna`)
Nord<-cbind(Nord)
Centro<-rbind(Toscana, Marche, Abruzzo, Umbria, Lazio, Molise)
Centro<-cbind(Centro)
Sud_e_Isole<-rbind(Campania, Basilicata, Puglia, Calabria, Sicilia, Sardegna)
Sud_e_Isole<-cbind(Sud_e_Isole)

#complete dataframe with added column "area" - specified zone
area<-arrange(region_dataset, region_dataset$codice_regione )
area<-rbind(area[area$denominazione_regione=="P.A. Trento",],area[area$denominazione_regione=="P.A. Bolzano",],area)
area<-area[1:(nrow(area)-2*nrow(area[area$denominazione_regione=="Piemonte",])),] #reorder dataframe
area["area"]<-c(rep("Nord",(9*nrow(area[area$denominazione_regione=="Piemonte",]))),rep("Centro",6*nrow(area[area$denominazione_regione=="Piemonte",])),rep("Sud e Isole",6*nrow(area[area$denominazione_regione=="Piemonte",])))
area<-arrange(area,data) #reorder by date










