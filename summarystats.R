library(foreign)
# Read data
{
  violence <- read.dta("C:/Documents and Settings/Ahlu/My Documents/Brookings/Violence/violence_data.dta")
  violence[,2:36]<-abs(violence[,2:36])
  region <- read.dta("C:/Documents and Settings/Ahlu/My Documents/Brookings/Violence/regions.dta")
  violence <- merge(violence,region,by = "country",all.x=T )
  var.labels <- attr(violence,"var.labels")  
  var.labels<-gsub("%","\\\\%",var.labels)
  var.labels<-gsub("\\$","\\\\$",var.labels)
  data.key <- data.frame(var.name=names(violence),var.labels)
  region_list <- unique(violence$region[!is.na(violence$region)])
  region_codes <- as.data.frame(cbind(region_list[order(region_list)]
                                      ,c("CEE-CIS","EAP","ESA","IC","LAC","MENA","SA","WCA")))
  stargazer(region_codes,out="regioncodes.tex",summary = F)
  colnames(region_codes) <- c("region","region_code")
  require(plyr)
  violence <- join(violence,region_codes,by = "region")
  rm(list=ls(pattern="region"))
}


require(stargazer)

stargazer(violence[,2:19],covariate.labels = var.labels[2:19],digits=1
          ,notes="*Data refer to the most recent year available during the period specified"
          ,out="table1.tex",title="Dependent(UNICEF report) variables")
stargazer(violence[,20:36],covariate.labels = var.labels[20:36],digits = 1
          ,out="table2.tex",title="Independent variables")

for (reg in unique(violence$region)[!is.na(unique(violence$region))]){
  
  stargazer(violence[violence$region == reg,2:19]
            , covariate.labels = var.labels[2:19], digits=1
            , title = reg,out = paste(gsub(" ","",x =  reg),".tex", sep = ""))
  
}

for(i in seq(2, 19, 6)){
    test <- aggregate(violence[,i:(i+5)], by = list(violence$region_code), FUN = mean,na.rm=T)
    test[,2:7] <-apply(test[,2:7],2,round,digits=1)
    test <-as.matrix(test)
    test1 <- aggregate(violence[,i:(i+5)], by = list(violence$region_code), FUN = sd,na.rm=T)
    test1[,2:7] <-apply(test1[,2:7],2,round,digits=1)
    test1 <-as.matrix(test1)
    test <- matrix(paste(test,paste("(",test1,")",sep="")),nrow=nrow(test))
    test[,1] <- test1[,1]
    test[,2:7] <- gsub(pattern = "NA",replacement = "-",x = test[,2:7])
    colnames(test) <- c("Region",colnames(violence[,i:(i+5)]))
    test2 <- aggregate(x=violence[,i:(i+5)], by = list(violence$region_code), function(x) sum( !is.na(x) ))
    sums <- colSums(test2[2:7])
    sums <- cbind("Observations",t(sums))
    test <- rbind(test,sums)
    gazer <- stargazer(test, digits=1, summary = F,no.space = T
                       ,column.labels = colnames(violence[,i:(i+5)]))
    gazer <- gsub("ccccccc","p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}",gazer)
    gazer <- append(gazer,"\\hline \\\\[-1.8ex] ",pmatch("Observation",gazer)-1)
    write(gazer, file=paste("test",i,".tex", sep = ""))
}


rm(sums)
rm(test)
rm(test1)
rm(lo)

#for anova comparison, subset regression
#fit<-rlm(male_homicide~median_age+hdi 
# #         ,subset(violence[which(!is.na(income_gini_coefficient)),])) 
# fit<-rlm(male_homicide~median_age+hdi 
#          ,data = violence) 
# fit1<-rlm(male_homicide~median_age+hdi+ income_gini_coefficient 
#           ,data = violence)
# fit2<-rlm(female_homicide~male_atitude_wife_beating+hdi+income_gini_coefficient+median_age)
# stargazer(fit,fit1,fit2,digits = 2, out ="reg.tex")
# 
# by(data = violence,INDICES =  violence$region, FUN =  stargazer)
#                          
