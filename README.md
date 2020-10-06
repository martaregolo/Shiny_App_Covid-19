# Dashboard COVID-19 in R Shiny

For my master project, I designed and implemented a dashboard in R Shiny for the data visualization about the COVID-19 epidemic. 
I used three Exploratory Data Analysis tools: a leaflet map, a bar plot, and a time series. Besides, I used control charts, that are statistical process control tools, to monitor the path of some statistics.

The app is available here:

https://regolomarta.shinyapps.io/tesi/

## Data collection and preparation

First of all, data were collected. The database was available from the Civil Protection Department [repository](https://github.com/pcm-dpc/COVID-19) on Github.
The interesting document for the study was that with all the data collected on a regional basis.
This file had 18 fields with information about region, regional code, date, latitude, longitude and the relevant parameters such as:
* Hospitalized with symptoms;
* Intensive care;
* Total hospitalized;
* Home isolation;
* Total positives;
* New positives;
* Discharged healed;
* Total positive change;
* Deceased;
* Total cases;
* Swabs;
* Tested cases.

Second, to conduct the study, data were additionally manipulated and new statistics have been defined, such as: *percentage of tested cases*,*percentage of intensive care* and *normalised number of tested cases*.

The Dashboard is composed by two tabs. 

## First Tab

The first tab allows the user to visualise the evolution of each parameter for zones (Italy, Nord, Center, South and Islands).

![Test](https://github.com/martaregolo/Shiny_App_Covid-19/blob/master/overall.PNG)

## Second Tab
The second tab illustrates the evolution of three main parameters, that are *new positives*, *normalised number of daily positives* and *normalised number of tested cases*. 

![Test](https://github.com/martaregolo/Shiny_App_Covid-19/blob/master/tab2.PNG)
