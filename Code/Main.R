library(readr)
singapore <- read_csv("Data/M810091.csv", skip = 10)

subset <- (2023-1970+1):1
years <- 1970:2023


library(INLA)
library(ggplot2)

dragon_indicator <- rep(0,length.out=length(years))
dragon_indicator[c(7,7+12,7+12*2,7+12*3)] <- 1

snake_indicator <- rep(0,length.out=length(years))
snake_indicator[c(7,7+12,7+12*2,7+12*3)+1] <- 1


for (j in 1:3){
  
  if (j==1){
    data.focus <- data.frame(fertility=singapore$`Total Fertility Rate -> Chinese`[subset],
                             year=years, dragon_indicator=dragon_indicator,
                             snake_indicator=snake_indicator)
  } else if (j==2){
    data.focus <- data.frame(fertility=singapore$`Total Fertility Rate -> Indians`[subset],
                             year=years, dragon_indicator=dragon_indicator,
                             snake_indicator=snake_indicator)
  } else {
    data.focus <- data.frame(fertility=singapore$`Total Fertility Rate -> Malays`[subset],
                             year=years, dragon_indicator=dragon_indicator,
                             snake_indicator=snake_indicator)
  }

  model <- inla(fertility ~ f(year, model = "rw2") + dragon_indicator + snake_indicator
               , control.compute = list(dic = TRUE), verbose = FALSE,
             family = "gaussian", data = data.focus)
  
  
  fit1 <- model$summary.fitted.values
  # plot(x = data.focus$year, y = data.focus$fertility,
  #      ylab='Fertility Rate', xlab='Year', pch=19, cex=0.5, type='p', main='Chinese')
  # points(data.focus$fertility[which(dragon_indicator==1)], x=data.focus$year[which(dragon_indicator==1)],
  #        col='red')
  # lines(data.focus$year, fit1$mean, col='blue')
  # lines(data.focus$year, fit1$`0.025quant`, lty = 2, col='blue')
  # lines(data.focus$year, fit1$`0.975quant`, lty = 2, col='blue')
  
  round(model$summary.fixed[c(2,3),c(1,3,5)],2)
  
  data.focus$mean <- fit1$mean
  data.focus$lo <- fit1$`0.025quant`
  data.focus$hi <- fit1$`0.975quant`
  
  if (j==1){
    data.focus$Ethnicity <- 'Chinese'
  } else if (j==2){
    data.focus$Ethnicity <- 'Indians'
  } else {
    data.focus$Ethnicity <- 'Malays'
  }
  
  if (j==1){
    data.focus.all <- data.focus
  } else {
    data.focus.all <- rbind(data.focus.all, data.focus)
  }
}
  

pdf(file='singapore.pdf', width=7, height=3)
g <- ggplot(data = data.focus.all)+
    geom_point(aes(x = year, y = fertility, col=Ethnicity), size=0.8, alpha=0.8)+
  geom_line(aes(x = year, y = mean, col=Ethnicity))+
  geom_ribbon(aes(x = year, ymin = lo, ymax = hi, fill=Ethnicity), alpha = 0.2, linewidth=0) +
  theme_bw()+
  geom_vline(xintercept=c(2012,2000,1988,1976), linetype="dotted")+xlab("Year")+
  ylab("Total fertility rate") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
print(g)
dev.off()
