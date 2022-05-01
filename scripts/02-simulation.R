n <- 4032
set.seed(03)
x_wwwhr <- sample(1:168,size = n,replace = TRUE)


n <-  4032
set.seed(03)
x_income <- sample(1:13,size = n,replace = TRUE, prob = c(1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13))

n <-  4032
set.seed(03)
x_emailhr <- sample(0:168,size = n,replace = TRUE)

n <-  4032
set.seed(03)
x_quallife <- sample(1:5,size = n,replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2))

n <-  4032
set.seed(03)
x_hlthphys <- sample(1:5,size = n,replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2))

n <-  4032
set.seed(03)
x_hlthmntl <- sample(1:5,size = n,replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2))

n <-  4032
set.seed(03)
x_age_group <- sample(1:8,size = n,replace = TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8))

n <-  4032
set.seed(03)
x_educ_level <- sample(1:4,size = n,replace = TRUE, prob = c(0.25,0.25,0.25,0.25))

n <-  4032
set.seed(03)
x_sex <- sample(1:2,size = n,replace = TRUE, prob = c(0.5,0.5))

df_simulation <- data.frame(x_wwwhr,x_income,x_emailhr,x_quallife,x_hlthphys,x_hlthmntl,x_age_group,x_educ_level,x_sex)

df_simulation_clean <- df_simulation %>% mutate(`educ_level` = case_when(
  x_educ_level >= 1 & x_educ_level <= 6 ~ 'elementary school',
  x_educ_level >= 7 & x_educ_level <=8 ~ 'middle school',
  x_educ_level >= 9 & x_educ_level <=12 ~ 'high school',
  x_educ_level >= 13 ~ 'college or above')) %>%
  mutate(`age_group` = case_when(
    x_age_group >= 10 & x_age_group <= 19 ~ '10-19',    
    x_age_group >= 20 & x_age_group <=29~ '20-29',      
    x_age_group >= 30 & x_age_group <= 39 ~ '30-39',    
    x_age_group >= 40 & x_age_group <=49~ '40-49',      
    x_age_group >= 50 & x_age_group <=59~ '50-59',      
    x_age_group >= 60 & x_age_group <=69~ '60-69',      
    x_age_group >= 70 & x_age_group <=79~ '70-79',      
    x_age_group >= 80 ~ '80+')) 
levels(df_simulation_clean$x_quallife) <-c("EXCELLENT","VERY GOOD","GOOD","FAIR","POOR")
levels(df_simulation_clean$x_hlthphys) <-c("EXCELLENT","VERY GOOD","GOOD","FAIR","POOR")
levels(df_simulation_clean$x_hlthmntl) <-c("EXCELLENT","VERY GOOD","GOOD","FAIR","POOR")
levels(df_simulation_clean$x_sex) <-c("male","female")

write_csv(df_simulation_clean, "00-simulation.R")


par(mfrow=c(2,2))

plot(table(x_income)/n,lwd=10,ylim=c(0,0.2),xlab="income",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(31/1767,19/1767,8/1767,3/1767,12/1767,72/1767,62/1767,63/1767,1241/1767,221/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin income"),  line = 1, cex.main = 0.5)

plot(table(x_quallife)/n,lwd=10,ylim=c(0,1),xlab="qualllife",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(259/1767,792/1767,553/1767,146/1767,17/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin quality of life"),  line = 1, cex.main = 0.5)

plot(table(x_hlthphys)/n,lwd=10,ylim=c(0,1),xlab="hlthphys",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(249/1767,526/1767,674/1767,257/1767,61/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin physical health"),  line = 1, cex.main = 0.5)

plot(table(x_hlthmntl)/n,lwd=10,ylim=c(0,1),xlab="hlthmntl",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(288/1767,668/1767,535/1767,227/1767,49/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin menatl health"),  line = 1, cex.main = 0.5)

par(mfrow=c(2,2))

plot(table(x_age_group)/n,lwd=10,ylim=c(0,0.2),xlab="age_group",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(2/1767,177/1767,324/1767,306/1767,331/1767,339/1767,216/1767,72/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin age group"),  line = 1, cex.main = 0.5)

plot(table(x_educ_level)/n,lwd=10,ylim=c(0,1),xlab="educ_level",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(1375/1767,4/1767,374/1767,13/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin eudcation level"),  line = 1, cex.main = 0.5)

plot(table(x_sex)/n,lwd=10,ylim=c(0,1),xlab="sex",ylab="Density", cex.axis = 0.5,cex.lab = 0.5)
lines(c(814/1767,953/1767),lwd=4,type = "h",col="sky blue")
legend("topright",c("Empirical pdf","pdf"),lty = 1,col = c(1,"sky blue"),lwd = c(5,2), cex = 0.5)
title(main = ( "A. Comparison of Simulation vs. Origin sex"),  line = 1, cex.main = 0.5)
