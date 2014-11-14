#create vector to loop over

varnames <- colnames(violence[,2:19])

for(i in varnames) {
  # Create string of regression formula
  dependentvariable <- paste(i," ~  life_expectancy + mean_years_schooling + expected_years_schooling"
                        ,"+ gni_per_capita + quintile_income_ratio + income_gini_coefficient  "
                        ,"+ mmr + adolescent_fertility + female_pop_sec_education  "
                        ,"+ male_pop_sec_education + lfp_female + lfp_male + median_age")
  #run regression using assign
  assign(x = paste("m", i, sep = "_")
         , value = lm(as.formula(dependentvariable), data = violence)) 
}

for(i in seq(1, 18, 3)) {
   
   gazer <- stargazer(get((paste("m", varnames[i], sep = "_")))
            ,get(paste("m", varnames[i+1], sep = "_"))
            ,get(paste("m", varnames[i+2], sep = "_"))
            ,digits = 2 #, out =paste("reg",i,".tex", sep = "")
            ,column.labels = c(varnames[i],varnames[i+1],varnames[i+2])
            ,covariate.labels = var.labels[c(21,22,23,24,27,28,30,31,32,33,34,35,36)]
            ,header = F, no.space = T, object.names = T
            )
   gazer <- gsub("lccc","p{6cm}p{3cm}p{3cm}p{3cm}",gazer)
   write(gazer, file=paste("reg",i,".tex", sep = ""))
}


