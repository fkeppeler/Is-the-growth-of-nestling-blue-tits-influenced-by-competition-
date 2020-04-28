# Libraries
require (vegan)
require (dplyr)
require (visreg)
require (boot)

#Functions
as.num.fact <- function(x) {
  as.numeric(as.character(x))
}

#Data
blue_tit_data=read.csv ("blue_tit_data_updated_2020-04-18.csv")
head(blue_tit_data)

#Correct variable types
blue_tit_data=within (blue_tit_data,{
  rear_Cs_at_start_of_rearing = as.num.fact (rear_Cs_at_start_of_rearing)
  number_chicks_fledged_from_rear_nest = as.num.fact(number_chicks_fledged_from_rear_nest)
  rear_d0_rear_nest_brood_size = as.num.fact (rear_d0_rear_nest_brood_size)
  d14_rear_nest_brood_size = as.num.fact (d14_rear_nest_brood_size)
  rear_nest_LD = as.num.fact (rear_nest_LD)
  number_chicks_fledged_from_rear_nest = as.num.fact(number_chicks_fledged_from_rear_nest)
  day_14_tarsus_length = as.num.fact(day_14_tarsus_length)
  day_14_weight = as.num.fact(day_14_weight)
  rear_area=factor (rear_area)
  rear_Box=factor (rear_Box)
  })


blue_tit_data=subset(blue_tit_data, rear_nest_trt=='5' | rear_nest_trt=='6')


#### **Analysis at the "population level" (nests were considered sampling units)**
#### *Here, I'm going to test the influence of the number of live chicks in 
#### the nest (proxy for intraspecific competition) on the average and variance of 
#### chick's mass at day 14 (proxy for growth), and on the number of chicks that 
#### survived to leave the nest*

#Remove variables that we are not going to use for this approach
pop_data=blue_tit_data[,c("rear_Cs_at_start_of_rearing",
                          "d14_rear_nest_brood_size",
                          "number_chicks_fledged_from_rear_nest",
                          "day_14_tarsus_length",
                          "day_14_weight",
                          "rear_nest_LD",
                          "rear_area",
                          "rear_Box"
                          )]

#Calculate average and standard deviation for the variables according to each nest box. 
#Note that for some variables, sd will be zero because the variables were measured at 
#the population level (e.g., number_chicks_fledged_from_rear_nest)
pop_data=pop_data %>% 
  group_by(rear_Box, rear_area) %>% 
  summarise_all(.funs = list(Mean= ~ mean(x=.),
                             Sd= ~sd (x=.)))


#Explore the correlation between:
  
# * Number of live chicks in the nest at day 14 after hatching (d14_rear_nest_brood_size) 
#   and the number of chicks in nest of rearing immediately following experimental chick 
#   removals and additions (rear_Cs_at_start_of_rearing). These two variables are proxies 
#   for the number of chicks in the nests. 
# * Tarsus average length (mm) and average mass of
#   chicks (g) at day 14. These two variables are proxies for chicks growth.  
par (mfrow=c(1,2))
cor_test1=cor.test (pop_data$d14_rear_nest_brood_size_Mean,
                    pop_data$rear_Cs_at_start_of_rearing_Mean)

plot(d14_rear_nest_brood_size_Mean~rear_Cs_at_start_of_rearing_Mean,
     data=pop_data,main=paste('Cor=' ,round(cor_test1$estimate,2), 
                              ", p-value", ifelse(cor_test1$p.value<0.001,
                                                  "< 0.001",round(cor_test1$p.value,3))),
     ylab= "Number of chicks in the nest at day 14",
     xlab= "Number of chicks in the nest at day 0",
     cex.axis=1.1, cex.lab=1.3,cex.main=1.3,
     pch=16, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2))

cor_test2=cor.test (pop_data$day_14_tarsus_length_Mean,
                    pop_data$day_14_weight_Mean)

plot(pop_data$day_14_tarsus_length_Mean~pop_data$day_14_weight_Mean,
     data=pop_data,main=paste('Cor=' ,round(cor_test2$estimate,2), 
                              ", p-value", ifelse(cor_test2$p.value<0.001,
                                                  "< 0.001",round(cor_test2$p.value,3))),
     ylab=" Tarsus average length (mm)",
     xlab="Average mass of chicks (g)",
     cex.axis=1.1, cex.lab=1.3,cex.main=1.3,
     pch=16, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2))

# Both pair of variables are strongly correlated. In order to avoid multicolinearity in 
# my analysis, I will use:
# * The number of chicks in nest of rearing immediately following experimental 
#   chick removals and additions (rear_Cs_at_start_of_rearing) as a single proxy for 
#   intraspecific competition.  
# * Mass of chicks at day 14 (day_14_weight_Mean) as a single proxy for chicks growth. 

pop_data= subset(pop_data, select=-c(d14_rear_nest_brood_size_Mean,
                                     day_14_tarsus_length_Mean))
pop_data=na.omit(pop_data)

#Convert some variable into integers and calculate the coefficient of 
#variance of chicks' mass 

pop_data$number_chicks_fledged_from_rear_nest=as.integer(
  pop_data$number_chicks_fledged_from_rear_nest_Mean)
pop_data$rear_Cs_at_start_of_rearing=as.integer(
  pop_data$rear_Cs_at_start_of_rearing_Mean)
pop_data$day_14_weight_CV= (pop_data$day_14_weight_Sd/
                              pop_data$day_14_weight_Mean)

#Now, I'm going to model the influence of the number of chicks in the nests on the 
#response variables: average growth, coefficient variance of average growth, and 
#chicks survivorship. I'm going to conduct univariate models. For each response 
#variable, two models will be conducted: one with a single exploratory variable (the 
#number of chicks in the nests), and another with one exploratory variable (the number
#of chicks in the nests) and two influential co-variables (rear area and the date of 
#the first egg was laid).

# Average growth model - without co-variables
Model_mean_growth=lm (day_14_weight_Mean~rear_Cs_at_start_of_rearing,
                      data=pop_data)
(Summary_MMeangrowth=summary(Model_mean_growth))

# Average growth model - with co-variables
Model_mean_growth_cov=lm (day_14_weight_Mean~rear_Cs_at_start_of_rearing+
                            rear_area+rear_nest_LD_Mean,
                          data=pop_data)
(Summary_MMeangrowth_cov=summary(Model_mean_growth_cov))

#Growth variation model - without co-variables
Model_CV_growth=lm (day_14_weight_CV~rear_Cs_at_start_of_rearing,
                    data=pop_data)
(Summary_MCVgrowth=summary(Model_CV_growth))

#Growth variation model - with co-variables
Model_CV_growth_cov=lm (day_14_weight_CV~rear_Cs_at_start_of_rearing+
                          rear_area+rear_nest_LD_Mean,
                        data=pop_data)
(Summary_MCVgrowth_cov=summary(Model_CV_growth_cov))

#Survival Model - Without co-variables
Model_mean_Surv= glm (cbind(number_chicks_fledged_from_rear_nest,
                            rear_Cs_at_start_of_rearing)~
                        rear_Cs_at_start_of_rearing,
                      data=pop_data,family="binomial")
paste("In-sample R2 =", 
      (1 - (Model_mean_Surv$deviance/Model_mean_Surv$null.deviance))) 
(Summary_MSurv=summary(Model_mean_Surv))

#Survival Model - With co-variables
Model_mean_Surv_cov=glm (cbind(number_chicks_fledged_from_rear_nest,
                               rear_Cs_at_start_of_rearing)~
                           rear_Cs_at_start_of_rearing+
                           rear_area+rear_nest_LD_Mean,
                         data=pop_data,family="binomial")
paste("In-sample R2 =",
      (1 - (Model_mean_Surv_cov$deviance/ Model_mean_Surv_cov$null.deviance))) 
(Summary_MSurv_cov=summary(Model_mean_Surv_cov))

#Visualize the effect of the number of chicks in the nests on our response variables 
#according to the models created. 

par(mfrow=c(1,2))

# Average growth models
visreg(Model_mean_growth, xlab="Number of live chicks in the nest", 
       ylab="Average chick's mass (g) at day 14",
       main=paste("Model without co-variables,",
                  "Slope =", round(Summary_MMeangrowth$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MMeangrowth$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MMeangrowth$coefficients[2,4],3))))
) 

visreg(Model_mean_growth_cov,xvar="rear_Cs_at_start_of_rearing", 
       xlab="Number of live chicks in the nest", 
       ylab="Average chick's mass (g) at day 14",
       main=paste("Model with co-variables,",
                  "Slope =", round(Summary_MMeangrowth_cov$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MMeangrowth_cov$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MMeangrowth_cov$coefficients[2,4],3))))
)

# Growth variation models
visreg(Model_CV_growth, xlab="Number of live chicks in the nest", 
       ylab="CV of chicks' mass (g) at day 14",
       main=paste("Model without co-variables,",
                  "Slope =", round(Summary_MCVgrowth$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MCVgrowth$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MCVgrowth$coefficients[2,4],3))))
)

visreg(Model_CV_growth_cov,xvar="rear_Cs_at_start_of_rearing", 
       xlab="Number of live chicks in the nest", 
       ylab="CV of chick's mass (g) at day 14",
       main=paste("Model with co-variables,",
                  "Slope =", round(Summary_MCVgrowth_cov$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MCVgrowth_cov$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MCVgrowth_cov$coefficients[2,4],3))))
)

# Survival models
visreg(Model_mean_Surv,scale="response",rug=2,ylab="",
       xlab="Number of live chicks in the nest",
       ylab="Pr (number of chicks that survived to leave the nest)",
       main=paste("Model with co-variables,",
                  "Slope =", round(Summary_MSurv$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MSurv$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MSurv$coefficients[2,4],3))))
)

visreg(Model_mean_Surv_cov,scale="response",rug=2,
       xvar="rear_Cs_at_start_of_rearing",
       xlab="Number of live chicks in the nest",
       ylab="Pr (number of chicks that survived to leave the nest)",
       main=paste("Model with co-variables,",
                  "Slope =", round(Summary_MSurv_cov$coefficients[2,1],3),
                  ", p-value", ifelse(Summary_MSurv_cov$coefficients[2,4]<0.001,
                                      "< 0.001",paste("=",round(Summary_MSurv_cov$coefficients[2,4],3))))
)


