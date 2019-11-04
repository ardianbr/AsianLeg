# Legislative Power & National Identity
# Ardian Bakhtiar Rivai -- D056020007
# Institute of Political Science, NSYSU

rm(list=ls())

install.packages("aod")
install.packages("ggplot2")

update.packages("aod")
update.packages("ggplot2")

library(readxl)
Data <- read_excel("Data.xlsx")
View(Data)

length(Data)
nchar(Data)
m <- matrix(nrow = 12, ncol = 21)
m
dim(m)
m <- matrix(c(1:3))
class(m)
typeof(m)
head(Data)
tail(Data)
dim(Data)
str(Data)
names(Data)
colnames(Data)



Data$country <- factor(Data$country)
head(Data)

summary(Data)

xtabs(~country + common , data = Data)



# ==== DISPLAY =====

library(dplyr)
glimpse(Data)

library(skimr)
skim(Data)

library(DataExplorer)
DataExplorer::create_report(Data)

library(stargazer)
library(ggplot2)

# ======== 

library(ggplot2)
lmFit1 <- lm(common~ year, data = Data)
acf(resid(lmFit1))


library(tidyverse) 
# Plot
ggplot(Data, aes(x=year, y=common)) +
  geom_point() + 
  geom_segment( aes(x=year, xend=year, y=0, yend=common))





# ======= REGRESSION TEST =======


# Model 1 Executive Oversught


m1 <- lm(common ~ exov*democracy + factor(country) , data=Data)
m1a <- glm(common ~ exov*democracy + factor(country), data=Data)
stargazer(m1, m1a, type="text")
summary(m1)



# Model 2 Legislative constraint on the executive


m2 <- lm(common ~ constraint*democracy + factor(country) , data=Data)
m2b <- glm(common ~ constraint*democracy + factor(country), data=Data)
stargazer(m2,m2b, type="text")
summary(m2)


# Model 3 Legislature controls resources

m3 <- lm(common ~ resources*democracy + factor(country) , data=Data)
m3b <- glm(common ~ resources*democracy + factor(country), data=Data)
stargazer(m3,m3b, type="text")
summary(m3)


# Model 4 Legislature corrupt activities

m4 <- lm(common ~ corrupt*democracy + factor(country) , data=Data)
m4b <- glm(common ~ corrupt*democracy + factor(country), data=Data)
stargazer(m4,m4b, type="text")
summary(m4)


# Model 5 Legislature investigates in practice
m5 <- lm(common ~ investigates*democracy + factor(country) , data=Data)
m5b <- glm(common ~ investigates*democracy + factor(country), data=Data)
stargazer(m5,m5b, type="text")
summary(m5)

# Moddel 6 Legislature opposition parties
m6 <- lm(common ~ opposition*democracy + factor(country) , data=Data)
m6b <- glm(common ~ opposition*democracy + factor(country), data=Data)
stargazer(m6,m6b, type="text")
summary(m6)

# Model 7 Legislature question officials in practice
m7 <- lm(common ~ question*democracy + factor(country) , data=Data)
m7b <- glm(common ~ question*democracy + factor(country), data=Data)
stargazer(m7,m7b, type="text")
summary(m7)

# Model 8 Representation of disadvantaged social groups
m8 <- lm(common ~ disadvantaged*democracy + factor(country) , data=Data)
m8b <- glm(common ~ disadvantaged*democracy + factor(country), data=Data)
stargazer(m8,m8b, type="text")
summary(m8)

# Model 9 Lower chamber committees
m9 <- lm(common ~ low_committees*democracy + factor(country) , data=Data)
m9b <- glm(common ~ low_committees*democracy + factor(country), data=Data)
stargazer(m9,m9b, type="text")
summary(m9)

# Model 10 Lower chamber legislates in practice
m10 <- lm(common ~ low_practice*democracy + factor(country) , data=Data)
m10b <- glm(common ~ low_practice*democracy + factor(country), data=Data)
stargazer(m10,m10b, type="text")
summary(m10)


# ========== BOX PLOT ==========




# Library
library(ggplot2)

# data
head(Data)


ggplot(Data , aes(x=country, y=common)) + geom_boxplot()
ggplot(Data , aes(x=country, y=common)) + geom_boxplot() 
ggplot(Data , aes(x=country, y=common)) + ylim(0,3) + 
  geom_violin() 


# reorder 
Data$country = with(Data, reorder(country, common, mean))



# plot
ggplot(Data, aes(x=country, y=common, fill=country)) +
  geom_violin(alpha=0.6) +
  theme(legend.position="none")

#plot country common good
boxplot(common~country,data=Data, main="Common Good in Asia 2012-2017", 
        xlab="Country", ylab="Common Good")

ggplot(Data, aes(factor(country), common)) +
  geom_violin(aes(fill = factor(country)))

# libraries
library(ggplot2)
library(gridExtra)

# Make 3 simple graphics:
g1=ggplot(Data, aes(x=constraint)) + geom_density(fill="Black")
g2=ggplot(Data, aes(x=democracy, y=common)) + geom_point() +
  geom_rug(position = "jitter", size = 0.2) + geom_text(aes(label = country), size = 3)
g3=ggplot(Data, aes(x=factor(country), y=common)) + geom_boxplot() 
g4=ggplot(Data, aes(x=factor(question))) +  geom_bar()

# Show the 4 plots on the same page
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)




#New Spatial Model Indonesia and Taiwan
A1=ggplot(Data, aes(x=factor(country), y=common)) + geom_boxplot() +
  ggtitle ("Spatial Model of Indonesia & Taiwan")

A2 = ggplot(Data, aes(x = constraint, y = common)) +
  geom_line(position = position_dodge(0.2)) +
  geom_point(position = position_dodge(0.2), size = 4)
grid.arrange(A1, A2, nrow = 2) 


# ============== Linear regression plot ==========

# Model 2
mp2 <- ggplot(Data, aes(x=constraint, y=common)) 

mp2 +
  geom_point(colour = "black") +
  stat_smooth(method = lm, se = FALSE, colour = "blue", level = 0.99) +
  geom_smooth(method=lm , 
  colour = "blue", se=TRUE) + 
  xlab("Legislative Constraint on The Executive") + 
  ylab("The Hybrid National Identity") 

# Model 5

ggplot(Data, aes(x=investigates, y=common)) +
  geom_point(shape=1) +
  geom_smooth(method=lm , 
color="black", se=TRUE) +
xlab("Legislative Investigates in Practices") + ylab("The Hybrid National Identity")

# Model 7

ggplot(Data, aes(x=question, y=common)) +
  geom_point(shape=1) +
  geom_smooth(method=lm , 
color="black", se=TRUE) +
xlab("Legislature Question in Practices") + ylab("The Hybrid National Identity")

# Model 4

ggplot(Data, aes(x=corrupt, y=common)) +
  geom_point(shape=1) +  geom_smooth(method=lm , 
color="black", se=TRUE) +
xlab("Legislature Corrupt Activities") + ylab("The Hybrid National Identity")


# Model 9

ggplot(Data, aes(x=low_committees, y=common)) +
  geom_point(shape=1) +
  geom_smooth(method=lm , 
color="black", se=TRUE) +
xlab("Lower Chamber Committees") + ylab("The Hybrid National Identity")


# Democracy
ggplot(Data, aes(x=democracy, y=common)) +
  geom_point(shape=1) +  geom_smooth(method=lm , 
color="black", se=TRUE) +
xlab("Electoral Democracy") + ylab("The Hybrid National Identity")



# Legislative power among Asian nation -- Density testing Three metaphors of legislature by Liao, 2005
library(ggplot2)  
country <- c("Afghanistan", "Sri Lanka", "Philippine", "Indonesia" , "South Korea" , "Taiwan")
ds <- subset(Data, country %in% country & ! is.na(constraint))
p  <- ggplot(ds, aes(constraint, colour=country, fill=country))
p  <- p + geom_density(alpha=0.1)
p


# ====================== clasification of legislature by Liao, 2005 ================

# library
library(ggplot2)

head(Data)

# Color and shape depend on factor (categorical variable)
ggplot(Data, aes(x=constraint, y=democracy, color=country, shape=country)) + 
  geom_point(size=20, alpha=0.6)

# Color and shape depend on factor (categorical variable)
ggplot(Data, aes(x=constraint, y=democracy, color=country, size=country)) + 
  geom_point(alpha=0.6)




# ================================== OKE =================================


# Electoral Democracy Index Country

library(plotly)

library(readxl)
Data <- read_excel("Dropbox/Data.xlsx")
View(Data)

# Library
library(ggplot2)

# data
head(Data)

# electoral democracy index
ggplot(Data, aes(factor(country), democracy)) + 
  geom_violin(aes(fill = country))

ggplot(Data, aes(factor(country), common)) +
  geom_violin(aes(fill = factor(country)))


# Regreession Legislative Power and Democracy (control variable)

ggplot(Data, aes(x=constraint, y=democracy) ) +
  geom_bin2d() +
  theme_bw()

# Number of bins in each direction?
ggplot(Data, aes(x=constraint, y=common) ) +
  geom_bin2d(bins = 70) +
  theme_bw()

# No Colour

library(dplyr)
country_lb <- ggplot(Data, aes(x=constraint, y=democracy) ) +
  geom_point()

country_lb +
  geom_text(aes(label = country), size = 4) +
  ggtitle ("Three Metaphors of Legislature by Liao 2015") +
  

country_lb +
  geom_text(aes(label = country), size = 4, vjust = 0)

country_lb +
  geom_text (aes(label = country), size = 3, hjust = 0)

# Revised Two
ggplot(Data, aes(x = factor(country), y = democracy)) +
  geom_col() +
  ggtitle ("Electoral Democracy Index in Asia")

ggplot(Data, aes(x = factor(country), y = constraint)) +
  geom_col() +
  ggtitle ("Legislative Constarints Index in Asia")
  




# Time series
library(ggplot2)
library(dplyr)
times_lb <- ggplot(Data, aes(x=year, y=common) ) +
  geom_point()

c1 = times_lb +
  geom_label(aes(label = country), size = 4) +
  ggtitle ("Time-series Common Good in Asia")

c2 = ggplot (Data, aes(x = democracy, y = common)) +
  geom_line() +
  geom_point(size = 4, shape = 21, fill = "white")
grid.arrange(c1, c2, nrow = 2) 







# Index of Constraint & Democracy
  ggplot(Data, aes(x=democracy,  color=country, fill=country)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_light() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x  = element_text(size = 8)
  ) +
    
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~country)

# ========
  
  Data %>%
    filter(!is.na(common)) %>%
    arrange(common) %>%
    mutate(country=factor (country)) %>%
    ggplot( aes(x=country, y=common) ) +
    geom_segment( aes(x=country ,xend=country, y=0, yend=common), color="grey") +
    geom_point(size=3, color="#69b3a2") +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("")

  
  
  
  

# ==============    FINISH ========================



