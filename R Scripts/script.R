library(textreadr)
library(data.table)
library(stringr)
library(tidytext)
library(stringi)
library(plyr)
library(ggplot2)
library(ggthemes)
library(haven)
library(pwt9)
library(CBRT)
library(Rmisc)
library(plotly)

# Reading Files by R

load(file = "Data/SBB_Files.RData")

# Cleaning Data

PDF_1996 <- PDF_1996 %>%
  unnest_tokens(word, text)

PDF_1997 <- PDF_1997 %>%
  unnest_tokens(word, text)

PDF_1998 <- PDF_1998 %>%
  unnest_tokens(word, text)

PDF_1999 <- PDF_1999 %>%
  unnest_tokens(word, text)

PDF_2000 <- PDF_2000 %>%
  unnest_tokens(word, text)

PDF_2001 <- PDF_2001 %>%
  unnest_tokens(word, text)

PDF_2002 <- PDF_2002 %>%
  unnest_tokens(word, text)

PDF_2003 <- PDF_2003 %>%
  unnest_tokens(word, text)

PDF_2004 <- PDF_2004 %>%
  unnest_tokens(word, text)

PDF_2005 <- PDF_2005 %>%
  unnest_tokens(word, text)

PDF_2006 <- PDF_2006 %>%
  unnest_tokens(word, text)

PDF_2007 <- PDF_2007 %>%
  unnest_tokens(word, text)

PDF_2008 <- PDF_2008 %>%
  unnest_tokens(word, text)

PDF_2009 <- PDF_2009 %>%
  unnest_tokens(word, text)

PDF_2010 <- PDF_2010 %>%
  unnest_tokens(word, text)

PDF_2011 <- PDF_2011 %>%
  unnest_tokens(word, text)

PDF_2012 <- PDF_2012 %>%
  unnest_tokens(word, text)

DOC_2013[, V1 := gsub("?", "ı", V1, fixed = TRUE)]

DOC_2013 <- DOC_2013 %>%
  unnest_tokens(word, V1)

DOCX_2014 <- DOCX_2014 %>%
  unnest_tokens(word, V1)

PDF_2015 <- PDF_2015 %>%
  unnest_tokens(word, text)

PDF_2016 <- PDF_2016 %>%
  unnest_tokens(word, text)

names(PDF_2017)[names(PDF_2017) == "word"] <- "text"

PDF_2017 <- PDF_2017 %>%
  unnest_tokens(word, text)

PDF_2018 <- PDF_2018 %>%
  unnest_tokens(word, text)

PDF_2019 <- PDF_2019 %>%
  unnest_tokens(word, text)

PDF_2020 <- PDF_2020 %>%
  unnest_tokens(word, text)

PDF_1996[, word := gsub("õ", "ı", word, fixed = TRUE)]
PDF_1997[, word := gsub("õ", "ı", word, fixed = TRUE)]
PDF_1999[, word := gsub("õ", "ı", word, fixed = TRUE)]
PDF_2001[, word := gsub("õ", "ı", word, fixed = TRUE)]

# Text Analysis

C_List <- list(PDF_1996[,word], PDF_1997[,word], PDF_1998[,word], PDF_1999[,word], 
               PDF_2000[,word], PDF_2001[,word], PDF_2002[,word], PDF_2003[,word],
               PDF_2004[,word], PDF_2005[,word], PDF_2006[,word], PDF_2007[,word],
               PDF_2008[,word], PDF_2009[,word], PDF_2010[,word], PDF_2011[,word], 
               PDF_2012[,word], DOC_2013[,word], DOCX_2014[,word], PDF_2015[,word], 
               PDF_2016[,word], PDF_2017[,word], PDF_2018[,word], PDF_2019[,word],
               PDF_2020[,word])

theme_project <- function() {
  theme_economist() + theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
                            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
                            axis.title.y = element_text(margin = margin(r = 15), size = 20),
                            axis.title.x = element_text(margin = margin(t = 15), size = 20),
                            plot.caption = element_text(size = 15))
}

find_text_in_file <- function(x,y) {
  a <- data.table(matrix(0, ncol = length(x), nrow = 1))
  for (i in 1:length(x)){
    a[,i] <- sum(as.numeric(ldply(x[i], function(z) sum(z == y))))
  }
  return(a)
}

plot_words <- function(c,d) {
  a <- data.table(matrix(0, nrow = length(c), ncol = 1))
  colnames(a) <- "Number"
  for (i in 1:length(c)){
    a[i, ] <- sum(as.numeric(ldply(c[i], function(z) sum(z == d))))
  }
  a[, Year := seq(1996,2020)]
  g <- ggplot(a, aes(y = Number, x = Year)) + geom_line(size = 1.5) + 
    theme_project() + ggtitle(d) + labs(caption = "Source: SBB")
  return(ggplotly(g) %>%
           layout(annotations = 
                    list(x = 1, y = -0.15, text = "Source: SBB", 
                         showarrow = F, xref = 'paper', yref = 'paper', 
                         xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                         font = list(size = 12, color = "#636363")), 
                  title = list(text = paste0(d,
                                             '<br>',
                                             '<sup>',
                                             '</sup>'))))
}

find_text_grepl <- function(x,y) {
  a <- data.table(matrix(0, ncol = length(x), nrow = 1))
  for (i in 1:length(x)){
    a[,i] <- sum((grepl(y, x[[i]])))
  }
  return(a)
}

plot_words_grepl <- function(c,d) {
  a <- data.table(matrix(0, nrow = length(c), ncol = 1))
  colnames(a) <- "Number"
  for (i in 1:length(c)){
    a[i, ] <- sum((grepl(d, c[[i]])))
  }
  a[, Year := seq(1996,2020)]
  g <-ggplot(a, aes(y = Number, x = Year)) + geom_line(size = 1.5) + 
           theme_project()+ ggtitle(d) + labs(caption = "Source: SBB")
  return(ggplotly(g) %>%
           layout(annotations = 
                    list(x = 1, y = -0.15, text = "Source: SBB", 
                         showarrow = F, xref = 'paper', yref = 'paper', 
                         xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                         font = list(size = 12, color = "#636363")), 
                  title = list(text = paste0(d,
                                             '<br>',
                                             '<sup>',
                                             '</sup>'))))

}

find_freq <- function(x,y) {
  z <- as.data.table(table(x$word))
  setorder(z, -N)
  setnames(z, "V1", "word")
  return(z)
}

# Downloading Data

myCBRTKey <- c("Ql0gFEv8km")
pwt <- read_stata("http://www.rug.nl/ggdc/docs/pwt90.dta")
pwt <- copy(pwt9.1)
setDT(pwt)

data <- pwt[, list(isocode, year, rtfpna, hc, csh_x)]
data <- data[isocode == c("TUR") & year >= 1995]
names(data) <- c("isocode", "year", "tfp", "hc", "csh_x")

world_bank_data <- read.csv("Data/world_bank_data.csv")
world_bank_data
names(world_bank_data) <- c("year","inflation_rate", "gini","benins","bensol","bensa","benun","adeqin","adeqsocl","adeqsocsaf","adequn","grosssave","popgrowth","gdp","gdppc","export")

green_data <- read.csv("Data/green_growth_OECD.csv")
names(green_data) <- c("year", "co2_pbprod", "co2_dbprod","nonenprod","epgrowth","ncapt", "waterstress","expostopm25","foreststock","envtech","energprod","renrd","renrdgov","natgdp")

data <- data[world_bank_data, on = "year"]
data <- data[green_data, on = "year"]
data$econgrowth <- 1
for(i in c(2:25)){
  data$econgrowth[i] <- ((data$gdp[i] - data$gdp[i-1])/data$gdp[i-1])*100
  
}

data$isocode[23] <- "TUR"
data$isocode[24] <- "TUR"
data$isocode[25] <- "TUR"
data$renrd[15] <- mean(data$renrd[14], data$renrd[19])
data$renrd[16] <- mean(data$renrd[14], data$renrd[19])
data$renrd[17] <- mean(data$renrd[14], data$renrd[19])
data$renrd[18] <- mean(data$renrd[14], data$renrd[19])

tfp_graph <- ggplot(data, aes(x = year, y = tfp)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "TFP at constant national prices (2011 = 1)", x = "Year", title = "Total Factor Productivity", caption = "Source: Penn World Tables 9.1") +
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

hc_graph <- ggplot(data, aes(x = year, y = hc)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Index of human capital per person", x = "Year", caption = "Source: Penn World Tables 9.1") + 
  ggtitle("Human Capital", subtitle = "(based on years of schooling and returns to education)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

csh_x_graph <- ggplot(data, aes(x = year, y = csh_x)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Share of merchandise exports at current PPPs", x = "Year", title = "Share of merchandise exports at current PPPs", caption = "Source: Penn World Tables 9.1")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

inflation_rate_graph <- ggplot(data, aes(x = year, y = inflation_rate)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Inflation, consumer prices (annual %)", x = "Year", title = "Inflation Rate", caption = "Source: World Development Indicators")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 18),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(1996,2018,2))

gini_graph <- ggplot(data, aes(x = year, y = gini)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Gini index (World Bank estimate)", x = "Year", title = "Gini Coefficient", caption = "Source: World Development Indicators")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +
  scale_x_continuous(breaks=seq(2002,2018,2)) + coord_cartesian(xlim =c(2002, 2018), ylim = c(32, 46)) +
  scale_y_continuous(breaks=seq(32,44,2))


benins_graph <- ggplot(data, aes(x = year, y = benins)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social insurance programs to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social insurance programs to poorest quintile",subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(1.7), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 14)) +
  scale_x_continuous(breaks=seq(2004,2018,2)) + coord_cartesian(xlim =c(2004, 2018))


bensol_graph <- ggplot(data, aes(x = year, y = bensol)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social protection and labor programs ", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social protection and labor programs", subtitle = "(to poorest quintile, % of total SPL benefits)")+
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) + 
  coord_cartesian(xlim =c(2004, 2018), ylim = c(0, 10)) + scale_y_continuous(breaks = seq(0,10,2))


bensa_graph <- ggplot(data, aes(x = year, y = bensa)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of social safety net programs to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of social safety net programs to poorest quintile", subtitle = " (% of total safety net benefits)")+
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018))


benun_graph <- ggplot(data, aes(x = year, y = benun)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Benefit incidence of unemployment benefits and ALMP to poorest quintile", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Benefit incidence of unemployment benefits and ALMP to poorest quintile", subtitle = "(% of total U/ALMP benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 13),
        axis.title.x = element_text(margin = margin(t = 15), size = 13),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2008,2018,2)) + 
  coord_cartesian(xlim =c(2008, 2016))


adeqin_graph <- ggplot(data, aes(x = year, y = adeqin)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social insurance programs", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social insurance programs", subtitle = "(% of total welfare of beneficiary households)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018))


adeqsocl_graph <- ggplot(data, aes(x = year, y = adeqsocl)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social protection and labor programs ", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social protection and labor programs", subtitle = "(% of total welfare of beneficiary households)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2004,2018,2))  + 
  coord_cartesian(xlim =c(2004, 2018)) + scale_y_continuous(breaks = seq(28,38,2))

adeqsocsaf_graph <- ggplot(data, aes(x = year, y = adeqsocsaf)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of social safety net programs", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Adequacy of social safety net program", subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2004,2018,2)) +
  coord_cartesian(xlim =c(2004, 2018), ylim = c(0, 8))


adequn_graph <- ggplot(data, aes(x = year, y = adequn)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adequacy of unemployment benefits and ALMP", x = "Year", caption = "Source: World Development Indicators")  + 
  ggtitle("Adequacy of unemployment benefits and ALMP", subtitle = "(% of total social insurance benefits)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2008,2016,2)) +
  coord_cartesian(xlim =c(2008, 2016))

grosssave_graph <- ggplot(data, aes(x = year, y = grosssave)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Adjusted savings: gross savings", x = "Year", title = "", caption = "Source: World Development Indicators")+
  ggtitle("Adjusted savings: gross savings", subtitle = "(% of GNI)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))

popgrowth_graph <- ggplot(data, aes(x = year, y = popgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Population Growth", x = "Year", title = "Population Growth", caption = "Source: World Development Indicators") +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


gdp_graph <- ggplot(data, aes(x = year, y = gdp)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Gross Domestic Product (constant 2010 US$)", x = "Year", caption = "Source: World Development Indicators") + 
  ggtitle("Gross Domestic Product", subtitle = "(constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2016,2))


gdppc_graph <- ggplot(data, aes(x = year, y = gdppc)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "GDP per capita (constant 2010 US$)", x = "Year", caption = "Source: World Development Indicators")+
  ggtitle("GDP per capita", subtitle = " (constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


export_graph <- ggplot(data, aes(x = year, y = export)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Exports of goods and services", x = "Year", caption = "Source: World Development Indicators") +
  ggtitle("Exports of goods and services ", subtitle = "(constant 2010 US$)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


co2_pbprod_graph <- ggplot(data, aes(x = year, y = co2_pbprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Production-based CO2 productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Production-based CO2 productivity", subtitle = "GDP per unit of energy-related CO2 emissions")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2020,2))


co2_dbprod_graph <- ggplot(data, aes(x = year, y = co2_dbprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Demand-based CO2 productivity", x = "Year", caption = "Source: OECD") + 
  ggtitle("Demand-based CO2 productivity" , subtitle = "GDP per unit of energy-related CO2 emissions")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2004,2016,2)) + 
  coord_cartesian(xlim =c(2004, 2016), ylim= c(4,6))



nonenprod_graph <- ggplot(data, aes(x = year, y = nonenprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Non-energy material productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Non-energy material productivity" , subtitle = "GDP per unit of DMC")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2018,2))


epgrowth_graph <- ggplot(data, aes(x = year, y = epgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Environmentally adjusted multifactor productivity growth", x = "Year", caption = "Source: OECD") + 
  ggtitle("Environmentally adjusted multifactor productivity growth" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15))+ scale_x_continuous(breaks=seq(1996,2014,2)) + 
  coord_cartesian(xlim =c(1996, 2014))



ncapt_graph <- ggplot(data, aes(x = year, y = ncapt)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Contribution of natural capital", x = "Year", caption = "Source: OECD") + 
  ggtitle("Contribution of natural capital" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2012,2)) +
  coord_cartesian(xlim =c(1996, 2012))


waterstress_graph <- ggplot(data, aes(x = year, y = waterstress)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Water stress", x = "Year", caption = "Source: OECD") + 
  ggtitle("Water stress" , subtitle = "(Total freshwater abstraction, % total available renewable resources)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(1996,2014,2)) +
  coord_cartesian(xlim =c(2004, 2014))



expostopm25_graph <- ggplot(data, aes(x = year, y = expostopm25)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Mean population exposure to PM2.5", x = "Year", caption = "Source: OECD") +
  ggtitle("Mean population exposure to PM2.5" , subtitle = "")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) + scale_x_continuous(breaks=seq(2010,2020,2)) +
  coord_cartesian(xlim =c(2010, 2020))



envtech_graph <- ggplot(data, aes(x = year, y = envtech)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Development of environment-related technologies", x = "Year", caption = "Source: OECD") + 
  ggtitle("Development of environment-related technologies" , subtitle = "% all technologies")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2016,2)) +
  coord_cartesian(xlim =c(1996, 2016))



energprod_graph <- ggplot(data, aes(x = year, y = energprod)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Energy productivity", x = "Year", caption = "Source: OECD") +
  ggtitle("Energy productivity" , subtitle = "GDP per unit of TPES")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2020,2))



renrd_graph <- ggplot(data, aes(x = year, y = renrd)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Renewable energy public RD&D budget", x = "Year", caption = "Source: OECD") +
  ggtitle("Renewable energy public RD&D budget" , subtitle = "% total energy public RD&D")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2016,2))



renrdgov_graph <- ggplot(data, aes(x = year, y = renrdgov)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Environmentally related government R&D budget", x = "Year", caption = "Source: OECD") +
  ggtitle("Environmentally related government R&D budget" , subtitle = "% total government R&D")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2008,2020,2)) +
  coord_cartesian(xlim =c(2008, 2020))

natgdp_graph <- ggplot(data, aes(x = year, y = natgdp )) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "National expenditure", x = "Year", caption = "Source: OECD") + 
  ggtitle("National expenditure on environmental protection" , subtitle = "% GDP")+
  theme(plot.title = element_text(size = rel(1.6), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(2014,2018,2)) +
  coord_cartesian(xlim =c(2014, 2018)) + scale_x_continuous(breaks = seq(2014,2018,1))


econgrowth_graph <- ggplot(data, aes(x = year, y = econgrowth)) + geom_line(size = 1.5) + theme_economist() + 
  labs(y = "Economic Growth", x = "Year", caption = "Source: World Development Indicators") +
  ggtitle("Economic Growth" , subtitle = "in percentages(%)")+
  theme(plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0)), 
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15), size = 15),legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15), size = 20),
        axis.title.x = element_text(margin = margin(t = 15), size = 20),
        plot.caption = element_text(size = 15)) +  scale_x_continuous(breaks=seq(1996,2020,2)) +
  scale_y_continuous(breaks = seq(-6,10,4))


econgrowth_graph
tfp_graph 
hc_graph
csh_x_graph
inflation_rate_graph
gini_graph 
benins_graph
bensol_graph
bensa_graph
benun_graph
adeqin_graph
adeqsocl_graph
adeqsocsaf_graph
adequn_graph
grosssave_graph 
popgrowth_graph
gdp_graph
gdppc_graph
export_graph

#Green graphs
co2_pbprod_graph
co2_dbprod_graph
nonenprod_graph
epgrowth_graph
ncapt_graph
waterstress_graph 
expostopm25_graph
envtech_graph
energprod_graph
renrd_graph
renrdgov_graph 
natgdp_graph



