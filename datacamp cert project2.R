# Analysis for R certification

# Loading packages
library(tidyverse)
library(ggplot2)
library(moments)
library(ggthemes)
library(colorspace)
library(caret)
library(broom)
library(ggrepel)
library(xgboost)

# Importing data from csv
moped <- read_csv("School/datacamp/Datacamp-Cert-Project-2/moped.csv")

# cleaning
#####

# Inspecting data for variables to ensure we're not missing anything
head(moped)

