## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries----------------------------------------------------------------
library(ranger)
library(igraph)
library(correlation)
library(data.table)
library(fastDummies)
library(ggplot2)

## ----Data Loading-------------------------------------------------------------
data <- TangledFeatures::Housing_Prices_dataset

## ----Function call------------------------------------------------------------
Results <- TangledFeatures::TangledFeatures(Data = TangledFeatures::Housing_Prices_dataset, Y_var = 'SalePrice', Focus_variables = list(), corr_cutoff = 0.7, RF_coverage = 0.95,  plot = TRUE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'polychoric', cor3 = 'spearman')

## ----Correlation Heatmap------------------------------------------------------
Heatmap_plot <- Results$Correlation_heatmap
plot(Heatmap_plot)

## ----Correlation Graph--------------------------------------------------------
Igraph_plot <- Results$Graph_plot
plot(Igraph_plot)

