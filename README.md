# Air Pollution and COVID-19 Case Fatality Rate - A Global Analysis  
 The coronavirus disease 2019 (COVID-19) pandemic is still rapidly spreading globally. To detect high-risk cities and the impacts of air pollution on public health, this study explores the relationship between the long-term average concentration of air pollution and the city-level case fatality rate (CFR) of COVID-19 globally. Then, geographically weighted regression (GWR) is applied to illustrate the spatial variability of the relationships. Six air pollution factors, including nitrogen dioxide (NO2), sulfur dioxide (SO2), ozone (O3), PM2.5 (particles with diameter  ≤2.5 μm), PM10 (particles with diameter  ≤10 μm), and air quality index (AQI), are positively associated with the city-level COVID-19 CFR. Our results indicate that a 1-unit increase in NO2, SO2, O3, PM2.5, PM10, AQI, is related to a 1.450%, 1.005%, 0.992%, 0.860%, 0.568%, and 0.776% increase in the city-level COVID-19 CFR, respectively. Additionally, the effects of NO2, O3, PM2.5, AQI, and probability with poor AQI on COVID-19 have spatial variability worldwide. The adverse impacts of air pollution are different among the cities. This study illustrates that long-term exposure to air pollution is related to the COVID-19 CFR, i.e., it leads to more deaths after being infected by COVID-19.  
## Results: Coefficients, CFRR, 95% Confidence Intervals, and t-Value for Air Pollution Factors on COVID-19 CFR  
|	        |Variable                   	|Coefficient|CFRR(%)|95% CI	            |t-value|  
| :----:    | :----:                        | :----:    | :----:| :----:            |:----:|
|Model 1	|Average Concentration of NO2	|0.023**	|1.450	|(0.002 - 2.870)	|1.968|  
|Model 2	|Average Concentration of SO2	|0.016**	|1.005	|(0.064 - 1.952)	|2.064|  
|Model 3	|Average Concentration of O3	|0.015**	|0.992	|(0.072 - 1.905)	|2.100|  
|Model 4	|Average Concentration of PM2.5	|0.014***	|0.860	|(0.439 - 1.270)	|4.031|  
|Model 5	|Average Concentration of PM10	|0.009*	    |0.568	|(-0.028 – 1.156)	|1.863|  
|Model 6	|Average AQI                	|0.012***	|0.776	|(0.229 – 1.312)	|2.788|  
|Model 7	|Probability with Poor AQI  	|0.486	    |30.445	|(-21.213 – 80.735)	|1.165|  
| Note:      |                               |           |       |                   | \*p<0.1, \*\*p<0.05, \*\*\*p<0.01 |  
  
  
## Code  
[Preprocessing.R](\Code\Preprocessing.R) includes the code to extract all necessary data except CFR data.  
[CFR.R](\Code\CFR.R) includes the code to extract CFR data from R package "COVID-19" and to merge the data from Preprocessing.R.  
[Add_control_variables.R](\Code\Add_control_variables.R) includes the code to extract control variables from dataset.  
[Analyses.R](\Code\Analyses.R) includes the code to implement the Mixed Effects Models (MEM) and geographical weighted model (GWR).  
[Figure.R](\Code\Figure.R) includes the code to generate figures in Main Text and Supplementary Materials.  
  
## Data
The dependent variable is the city-level CFR, which is the ratio between deaths and confirmed cases in the cities. The data source is the R package “COVID19”, provided by Oxford COVID-19 Government Response Tracker. The numbers of deaths and confirmed cases of COVID-19 are provided by the R package “COVID19”. The last day of the data is 30th June 2021.  
The air pollution data are the atmospheric concentrations of several air pollutants, including PM2.5, PM10, SO2, NO2, and O3, Air Quality Index (AQI) and probability with poor AQI in more than 600 major cities of roughly 90 countries from 2015 to 2020. Air Quality Open Data Platform: <https://aqicn.org/data-platform/covid19/verify/57c94d81-c396-481a-a12c-93a0065c705f>  
World Pop 2020 Population Data: <https://www.worldpop.org/geodata/listing?id=65>  
GDP per Capita (PPP): Scientific Data: 10.1038/sdata.2018.4   
   
We thank all abovementioned data providers for making their data public and for enabling this research to be possible.  

## Cite our Article  
Li, C., Managi, S., 2022. Impacts of air pollution on COVID-19 case fatality rate: a global analysis. Environmental Science and Pollution Research.  
Note：The [manuscript](\Manuscript\Li-2022-Impacts-of-air-pollution-on-covid--.pdf) is in the Manuscript Folder.

## Contact Us:  
- Email: Prof. Shunsuke Managi <managi@doc.kyushu-u.ac.jp>  
- Email: Chao Li <chaoli0394@gmail.com>  
  
## Term of Use:  
Authors/funders retain copyright (where applicable) of code on this Github repo. This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Air Pollution and COVID-19 Case Fatality Rate - A Global Analysis" The analyses rely upon publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.  
