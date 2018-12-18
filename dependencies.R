if(!require(shiny)){install.packages("shiny")}
if(!require(shinydashboard)){install.packages("shinydashboard")}
if(!require(shinythemes)){install.packages("shinythemes")}
if(!require(udpipe)){install.packages("udpipe")}
if(!require(textrank)){install.packages("textrank")}
if(!require(lattice)){install.packages("lattice")}
if(!require(igraph)){install.packages("igraph")}
if(!require(ggraph)){install.packages("ggraph")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(stringr)){install.packages("stringr")}
if (!require(extrafont)){install.packages("extrafont")} 
if (!require(extrafontdb)){install.packages("extrafontdb")}
library(extrafont)
library(extrafontdb)
loadfonts(device = "win")
#font_import()
library(shiny)
library(shinydashboard)
library(shinythemes)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)