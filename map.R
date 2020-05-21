library(geojsonio)
library(broom)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(sp)
library(maptools)
library (readr)
library(plyr)
library(stringr)

#reads the Italian population at Jan 1st 2019 (ISTAT SOURCE)
popolazioneMF_df=read.csv("PopolazioneRegioni20190101.csv", header=TRUE)
popolazione_df=subset(popolazioneMF_df, select = -c(Codice_Regione,Totale_Maschi,Totale_Femmine))
names(popolazione_df)=c("denominazione_regione","popolazione") #changes names to fit to region_dataset

#reads the number of IC beds at Apr 29th 2020 (Ministero della Saule SOURCE)
IC_beds_df=read.csv("LettiTerapiaIntensiva20200429.csv", header=TRUE)
IC_beds_df=subset(IC_beds_df, select = -c(codice_regione))

#reading from github the entire dataset of observations up to the current day (NATIONAL)
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
national_dataset<-read.csv(url(urlfile))

#eliminating the hh:mm from the field "date"
national_dataset$data=as.Date(national_dataset$data, "%Y-%m-%d",tz="")
#eliminating useless columns from region_dataset
#national_dataset=subset(national_dataset,select=-c(note_it,note_en))

#reading from github the latest dataset of observations - current day (REGIONS)
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv"
region_latest_dataset<-read.csv(url(urlfile))

#eliminating the hh:mm from the field "date"
region_latest_dataset$data=as.Date(region_latest_dataset$data, "%Y-%m-%d",tz="")
#eliminating useless columns from region_latest_dataset
#region_latest_dataset=subset(region_latest_dataset,select=-c(note_it,note_en))


#reading from github the entire dataset of observations by region up to the current day (REGIONS)
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
region_dataset<-read.csv(url(urlfile))

#eliminating the hh:mm from the field "date"
region_dataset$data=as.Date(region_dataset$data, "%Y-%m-%d",tz="")

#adds regional population and number of IC beds at each record
region_dataset=left_join(region_dataset,popolazione_df,by="denominazione_regione") %>%
        left_join(IC_beds_df,by="denominazione_regione")
region_latest_dataset=left_join(region_latest_dataset,popolazione_df,by="denominazione_regione") %>% 
        left_join(IC_beds_df,by="denominazione_regione")

#reading from github the entire dataset of observations up to the current day (PROVINCES)
urlfile="https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"
province_latest_dataset<-read.csv(url(urlfile))

#eliminating the hh:mm from the field "date"
province_latest_dataset$data=as.Date(province_latest_dataset$data, "%Y-%m-%d",tz="")

province_latest_dataset$denominazione_provincia<-gsub("ForlÃ¬-Cesena","Forlì-Cesena",province_latest_dataset$denominazione_provincia)
province_latest_dataset<-filter(province_latest_dataset,denominazione_provincia!="In fase di definizione/aggiornamento")

#importare file gejson
dat_p<-geojson_read("https://gist.githubusercontent.com/datajournalism-it/212e7134625fbee6f9f7/raw/dabd071fe607f5210921f138ad3c7276e3841166/province.geojson", what="sp")
dat_r<-geojson_read("https://gist.githubusercontent.com/datajournalism-it/48e29e7c87dca7eb1d29/raw/2636aeef92ba0770a073424853f37690064eb0ea/regioni.geojson",what="sp")
names(dat_r@data)<-"denominazione_regione"
names(dat_p@data)<-"denominazione_provincia"
dat_r@data[["denominazione_regione"]]<-gsub("Trentino-Alto Adige","P.A. Bolzano",dat_r@data[["denominazione_regione"]])
dat_r@polygons[[4]]@Polygons[[1]]@coords<-dat_p@polygons[[19]]@Polygons[[1]]@coords

#estrarre P.A. Trento
coord<-dat_p@polygons[[23]]@Polygons[[1]]@coords
p=Polygon(coord)
ps<-Polygons(list(p),1)
sps=SpatialPolygons(list(ps))
data<-data.frame(denominazione_regione=as.character("P.A. Trento"))
spdf<-SpatialPolygonsDataFrame(sps,data)
spdf@polygons[[1]]@ID<-"21"
assign(paste0("region_", spdf@data[["denominazione_regione"]]), spdf)

#estrarre tutti i confini di regione
regioni<-assign(paste0(spdf@data[["denominazione_regione"]]), spdf)
#intera Italia
for (i in 1:20){
        coord<-dat_r@polygons[[i]]@Polygons[[1]]@coords
        p=Polygon(coord)
        ps<-Polygons(list(p),1)
        sps=SpatialPolygons(list(ps))
        data<-data.frame(denominazione_regione=dat_r@data[["denominazione_regione"]][i])
        spdf<-SpatialPolygonsDataFrame(sps,data)
        spdf@polygons[[1]]@ID<-as.character(i)
        assign(paste0(spdf@data[["denominazione_regione"]]), spdf)
        if (rgeosStatus()) {
                regioni <- rbind(regioni, assign(paste0(spdf@data[["denominazione_regione"]]), spdf))
                FIPS <- row.names(regioni)
                str(FIPS)
                length(slot(regioni, "polygons"))
        }
}

#unico sp italia
Italia<-merge(regioni,region_latest_dataset,by="denominazione_regione")
Nord <- rbind(`P.A. Trento`,Piemonte,`Valle d'Aosta`, Lombardia, `P.A. Bolzano`, Veneto, `Friuli Venezia Giulia`, Liguria)
Nord<-merge(Nord, region_latest_dataset,by="denominazione_regione")
Centro<-rbind(Toscana, Marche, Abruzzo, Umbria, Lazio, Molise)
Centro<-merge(Centro, region_latest_dataset,by="denominazione_regione")
Sud_e_Isole<-rbind(Campania, Basilicata, Puglia, Calabria, Sicilia, Sardegna)
Sud_e_Isole<-merge(Sud_e_Isole, region_latest_dataset,by="denominazione_regione")











       