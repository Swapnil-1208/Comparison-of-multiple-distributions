####Read Data------------
library(ggplot2)
library(car)
library(plotly)
library(gridExtra)

##Version of R
R.version.string

df <- read.csv("./Height_Data.csv", header = T, sep = ";")

####Descriptive Statistics------------

#Sample size
table(df$Sport)

#Data Type
str(df)

#Data summary
df_s <- data.frame()
for (i in unique(df$Sport)){
  
  df_s <- rbind(df_s,summary(df$Height[df$Sport == i]))
  
}
colnames(df_s) <- c("Min","Q1","Median","Mean","Q3","Max")
df_s


#Boxplot Variability assessment
ggplot(data = df, mapping = aes(x = Sport, y = Height)) +
  stat_boxplot(geom = "errorbar", size = 1) +
  geom_boxplot(fill="darkcyan") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))


#Samples variance
for (i in unique(df$Sport)){
  v <- var(df$Height[df$Sport == i])
  print(paste0("Variance of ",i," team: ",v))
}


#QQPlot for normality assessment
for (i in df$Sport){
  
  new_df <- df[df$Sport == i,]
  
  p <- ggplot(data = new_df, mapping = aes(sample = Height)) + 
    geom_qq(colour = "darkcyan") + 
    geom_qq_line(size = 1) +
    labs(x = "", y = i)
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
  
  assign(paste0("qq_",i,"_plot"),p)
}

grid.arrange(qq_volleyball_plot, qq_handball_plot, qq_soccer_plot, qq_basketball_plot, `qq_ice hockey_plot`, `qq_water polo_plot`, nrow = 2, ncol = 3)



#Anova Test
anova.res <- aov(formula = Height ~ Sport, data = df)
summary(anova.res)
?aov()


####Without Adjustment
pairwise.t.test(x = df$Height, g = df$Sport, p.adjust.method = "none", pool.sd = TRUE)
#?pairwise.t.test()

####Bonferroni Adjustment
pairwise.t.test(x = df$Height, g = df$Sport, p.adjust.method = "bonferroni", pool.sd = TRUE)
#t.test(x = df$Height, g = df$Sport, p.adjust.method = "bonferroni")