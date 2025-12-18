library(tidyverse)
library(readr)
library(dplyr)

#read in files. remember to get them in a working directory first. notes on what each are in email
plots<-read_csv("plot_key.csv")
nests<-read_csv("nest_summary.csv")
visits23<-read_csv("visits_23.csv")
visits24<-read_csv("visits_24.csv")

allnests<-left_join(nests, plots)%>% #joins the plot and nest dataframes by common variable (nestid). This copies over plot level data to the nest observations.
  filter(is.na(field), #remove nests not in GP
         is.na(in_study), #remove nests in GP but not in a plot
         is.na(confirmed), #remove 'presumed' but not found nests
         species=="Bobolink") #remove savanna sparrow nests

