# Dashboard COVID-19 in R Shiny

For my master project, I designed and implemented a dashboard in R Shiny for the data visualization about the COVID-19 epidemic. 
I used three Exploratory Data Analysis tools: a leaflet map, a bar plot, and a time series. Besides, I used control charts, that are statistical process control tools, to monitor the path of some statistics.

The app is available here:

https://regolomarta.shinyapps.io/tesi/

## Data collection and preparation

The data were available from the Civil Protection Department [repository](https://github.com/pcm-dpc/COVID-19) on Github.
The interesting document for the study was that with all the data collected on a regional basis.
This file has 18 fields with information about state, region, regional code, date, latitude, longitude and the relevant parameters such as:
* Hospitalized with symptoms;
* Intensive care;
* Total hospitalized;
* Home isolation;
* Total positives;
* New positives;
* Discharged healed;
* Total positive change;
* Deceaseds;
* Total cases;
* Swabs;
* Tested cases.



## First Tab

![Test](https://github.com/martaregolo/Shiny_App_Covid-19/blob/master/overall.PNG)


