require(foreign)
states <- read.dta("C:/Documents and Settings/Ahlu/My Documents/Brookings/Violence/states.dta")
states$crime_rate <- states$crimes_2010 *100000/ states$population_2011
states$crime_rate_14_18 <- states$crimes_14_18_2010 *100000/ states$popln_15_19
state.var.labels <- attr(states,"var.labels")  
state.var.labels[24] <- "Crime rate per 100,000"
data.key.states <- data.frame(var.name=names(states),state.var.labels)

require(stargazer)
state_gaze <- stargazer(states[,c(3:11,13:14,16,18:21,23:24)],digits=1
          ,out="StateSummary.tex",title="Summary statistics for states data"
          ,covariate.labels = state.var.labels[c(3:11,13:14,16,18:21,23:24)])
state_gaze <- gsub("lccccc","p{4.5cm}ccccc",state_gaze)
write(state_gaze, file="StateSummary.tex")


state_fit<-lm(states$crime_rate~states$nsdp_per_capita_2010 +states$sex_ratio_2011
                                +states$literacy_female +states$literacy_male
                                +states$lfp_female+states$lfp_male
                                +states$health_exp_per_capita 
                                +states$education_exp_per_capita
                                +states$police_exp_per_capita)
state_fit1<-lm(states$crime_rate_14_18~states$nsdp_per_capita_2010 +states$sex_ratio_2011
              +states$literacy_female +states$literacy_male
              +states$lfp_female+states$lfp_male
              +states$health_exp_per_capita 
              +states$education_exp_per_capita
              +states$police_exp_per_capita)
stargazer(state_fit,state_fit1, out="StateFit.tex",title="Indian states regression"
          ,covariate.labels = state.var.labels[c(14,5,7,8,10,11,13,16,18)]
          ,column.labels = c("Crime rate","Crime rate 14-18 year olds")
         , object.names = F)
