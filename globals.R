#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(RColorBrewer)

library(DT)
library(tidyverse)

library(imager)
library(magick)
library(quanteda)
library(inflection)
library(dashboardthemes)
library(ggforce)
library(imager)
library(magick)

options(DT.options = list(
  pageLength = 12,
  searching = FALSE,
  lengthMenu = c(5, 10, 20, 50, 100, 200, 500,1000,2000),
  dom = "Blfrtip",
  buttons = list(list(extend = "csv", filename = "download"))
))

# Functions

data_table_fun <- function(df,table_title,colours,font_perc,dp){

df %>%
  DT::datatable(
    caption = table_title,
    extensions = c("Buttons"),
    options = list(scrollX = TRUE)
  ) %>%
  DT::formatStyle(columns = base::seq(from = 1,to = dim(df)[2],by = 1),
                  background = DT::styleInterval(cuts = base::seq(from = min(df),to = max(df),length.out = 8),
                                                 values = RColorBrewer::brewer.pal(n = 9,name = colours)),
                  fontSize = font_perc) %>%
  DT::formatRound(columns = base::seq(from = 1,to = dim(df)[2],by = 1),
                  digits = dp)

}


source("tab3.R")
source("tab1.R")
source("tab2.R")

