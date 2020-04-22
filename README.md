# SYS2202-Final
crime mapping 

By:
Emma, Jackson, Darren, Niels, and Hunter (UVa Second Years)

For Systems Engineering Final Project

Professor: Afsaneh Doryab


This project intends to analyze different aspects and patterns of crime data within Charlottesville, Virginia. To begin, we first
researched and procured a data set that included several rows and thousands of columns of data on criminal activity and arrest data 
in Charlottesville, Virginia. The data set we used can be found at: https://opendata.charlottesville.org/datasets/d1877e350fad45d192d233d2b2600156_6 .

 The data from the data set includes crime and arrest data from 2018 to present and consistently updated by Charlottesville Open Data. 
 For simplicity's sake, we narrowed down the range of data from 01/01/2019 to 04/01/2020 (project was undertaken in March-April 2020) and
 this allowed for quicker cleaning and organization of data, while still allowing us to analyze important aspects of the data pertinent 
 to the present day. 
 
 With the dataset from Charlottesville Open Data and with Google API, we were able to geocode the Street Address of the dataset to make
 location points with latitude and longitude. Using the leaflet package we were able to get a background street map of the World which
 could be narrowed down to Charlottesville, and then we added markers based on the longitude and latitude points of the crime data. 
 This resulted in a map of Charlottesville with each arrest and its location plotted over it. 
 
 Continuing our analysis, we wanted to make our map interactive, so we began using Rshiny in order to add an interactive element to our 
 map. The interactivity of the map was important because it allowed us to narrow down certain factors pertaining to each crime in order
 to gain a better understanding of what factors contribute to different crimes in different locations throughout charlottesville. We 
 created a variable of violence level that was based on which crime was committed (e.g. murder = 5, probation = 1). We created sliders 
 and checkboxes to control which values would be plotted including time of day, gender, race, and repeat offenses. 
