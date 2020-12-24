library(flexdashboard)
library(data.table)
library(tidyverse)
library(lubridate)

setwd("D:/Users/Veasna/Documents/Finanzen/Investitionen/R_Dashboard")

finData = fread("financial_overview.csv")
finData[, ":=" (out = -1*out, invest = as.numeric(-1*invest))]

inOutData = melt(finData, id.vars = c("year", "date"), measure.vars = c("in", "out", "invest"))
inOutData[, ":=" (year = as.factor(year))]

inOutData[, ":=" (date = factor(date, levels = c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")), 
                  variable = factor(variable, levels = c("in", "invest", "out"))
                  )]

liqData = finData[, .(year, date, 
                        value = finData$`in` + finData$out + finData$invest)]
liqData[, cumvalue := cumsum(value), by = year]

  ggplot(inOutData, aes(date, value)) +
    geom_col(aes(fill = variable)) +
    geom_hline(yintercept = 0, alpha = .2) +
    geom_line(data = liqData, aes(date, cumvalue), alpha = .5) +
    facet_grid(.~year) +
    scale_fill_manual(values = c("#dff0d8", "#d9edf7", "#f2dede")) +
    theme_classic()
  
depotInData = melt(finData, id.vars = c("year"), measure.vars = c("Eurogov Germany 10+", "MSCI World SRI", "MSCI EM SRI", "Cash"))
depotInData[, ":=" (year = as.factor(year))]

depotInData = depotInData[, .(value = sum(value, na.rm = T)), by = variable]


depotInData

ggplot(depotInData, aes(x = reorder(variable, value), y = value)) +
  geom_segment(aes(xend=reorder(variable, value), y=0, yend=value), color ="grey") +
  geom_point(color = "orange", size = 4) +
  coord_flip() +
  theme_classic()
