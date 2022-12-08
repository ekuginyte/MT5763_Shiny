### Covid-19 Tracker
##### By **Moray**

#### Introduction

This project scrapes time series data on Covid-19, including the total number of confirmed, death, and recovered cases since the virus outbreak began (supposedly) on January 1, 2021. The data is scraped from a GitHub repository where the information is updated daily. The most recent data available is from two days ago. The data is operated by Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Population data extracted from the worldometers.info.

To note, the dates are displayed in an American month/day/year format. 

#### Data Map

The goal of this app is to display the relative to population numbers (per million) of cases over time, and the map makes it easy to compare how each country is dealing with the outbreak. Mouse hovering over a country will display the selected statistic. Countries with no data recorded by the JHU are filled with grey.

Download buttons allow to download both the currently displayed map and the data set.

#### Data

Data section allows for downloading data for chosen (up to 10 at a time) countries of the date frame that has been selected. The data has been manipulated so the cases are relative to respective population per million.

Download button allow to download the selected data.

#### Data Statistics

Data Statistics section allows to visually compare the confirmed, Covid-19 death associated and recovered numbers across selected (up to 10 at a time) countries of the date frame that has been selected. The numbers of cases are relative to respective country population per million.

Download buttons allow to download both the selected data and the plots that come with it.
