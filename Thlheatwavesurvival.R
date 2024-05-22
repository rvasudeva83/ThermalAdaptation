#check what is in r console from last time
ls()
#remove everything
rm(list=ls())

library(car); library(MASS); library (lme4); library (nlme);  library(lmtest); 
library(ggplot2); library(MuMIn); library(emmeans); library(psych);
library(LMERConvenienceFunctions); library(car); library(AER); library(MASS); library(RLRsim)

#check what working directory is set
getwd()
#setting to required one where data is based
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")


#reading in data 
survival <- read.csv("Thlheatwavesurvival.csv", header = TRUE) # 10D data set 
list.files()



#### DATA CHECK, CLEAN, DESCRIPTION AND SUMMARY #########################################################################

survival # produces all whole dataframe - no NAs, data to 2 or 3 d.p., no irregularities/anomalies

### checking for outliers/errors
summary(survival) # produces general (unsplit) range, quantiles, median, count and mean summary stats for each variable

str(survival) # checks the variable types
# 'data.frame':	49 obs. of  7 variables:
# $ Replicate     : int  1 2 3 4 5 6 7 8 9 10 ...                                          # petri dish, 1 per thermal line so no pseudo rep
# $ Regime        : Factor w/ 3 levels "30thl","38thl",..: 3 3 3 3 3 3 3 3 3 3 ......      # thermal evol regime temperature / kss
# $ Rearing       : int  30 30 30 30 30 30 30 30 30 30 ..                                  # developmental temperature
# $ N             : int  20 20 20 20 20 20 20 20 20 20 ...                                 # group size in petri
# $ Survivors     : int  19 18 18 18 19 20 20 19 18 19 ...                                 # # of survivors
# $ Deaths        : int  1 2 2 2 1 0 0 1 2 1 ...                                           # # of deaths
# $ Prop.survivors: num  0.95 0.9 0.9 0.9 0.95 1 1 0.95 0.9 0.95 ...                       # proportion of survivors after a heatwave

survival$Rearing<-as.factor(survival$Rearing)
survival$Regime<-as.factor(survival$Regime)

str(survival)

is.na(survival) # returns TRUE of x is missing
# nothing missing

levels(survival$Regime)
# "30thl" "38thl" "KSS"  

levels(survival$Rearing)
# "30" "38"

# dataset to compare to KKS
survival$Regime <- factor(survival$Regime,levels = c("KSS", "30thl", "38thl"))
levels(survival$Regime)


# within thermals 
survivalnoKSS <- survival[survival$Regime != "KSS",]
survivalnoKSS$Regime <- droplevels(survivalnoKSS$Regime)
str(survivalnoKSS)

# within 30
survival30oC <- survival[survival$Rearing == "30",]
survival30oC$Rearing <- NULL
survival38oC <- survival[survival$Rearing != "30",]
survival38oC$Rearing <- NULL
str(survival30oC)
str(survival38oC)

# within 38
survival30thl <- survival[survival$Regime == "30thl",]
survival30thl$Regime <- NULL
survival38thl <- survival[survival$Regime == "38thl",]
survival38thl$Regime <- NULL
str(survival30thl)
str(survival38thl)




names(survival)
str(survival)


#############   FIGURE 3 PLOT ##################

temp <- expression(paste(bold('Temperature (',degree,'C)'))) #the temperature label with degrees sign

# sex survival boxplot
thlsurvival<-ggplot(survival, aes(x=Rearing, y=Prop.survivors, fill= Regime)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
     geom_boxplot(notch=F,  #change to F if want to get rid of notchs
                  outlier.shape= NA, #shape of the outlier (hashtag out if dont want outliers marked)
                  width=0.5,
                  lwd=0.5,
                  fatten=0.5,
                  color="black",
                  position=position_dodge(0.5)) + #size of the outlier (hashtag out if dont want outliers marked)
     stat_summary(fun.y="mean", geom= "point", size=4, position=position_dodge(0.5), color="black") + 
     scale_fill_manual(values=c("ghostwhite", "cadetblue1","darkorange1"), # changes the colour of the bars
                       name = temp, #adds in temperature label on the legend
                       breaks = c("KSS", "30thl", "38thl"), #the order listed in the legend
                       label = c("KSS", "30thl", "38thl")) + #how things are labeled in the lgend
     scale_colour_manual(values=c("black", "black", "black")) +
     geom_point(position=position_jitterdodge(dodge.width=0.5, jitter.width = 0.15), shape=1, size= 1.5) +
     labs (x= expression(bold("Developmental temperature "( degree*C))),
           y="Proportion surviving heatwave treatment") +  #adding title to the x axis and y axis
     scale_x_discrete(breaks=c("30", "38"), #the order of the variables on the x axis
                      labels=c("30", "38")) + # the names on the x axis
     coord_cartesian(ylim=c(-0.02, 1.065)) + #set axis limits
     scale_y_continuous(breaks=seq(0, 1, 0.2), #ticks from 0 to 16000 and show number every 16000
                        expand = c(0, 0)) + #cuts the axis off at 0
     theme_classic() + #the theme of the whole plot 
     annotate("text", x=c(0.85,1,1.15,1.5,1.9,2.1),
           y= c(1.03,1.03,1.03,1.05,1.03,1.03), 
           label= c("A","B","A","***","B","C"),
           size= 4) +
     theme(
          #legend.position="none", #get rid of the hashtag to get rid of legend
          panel.grid.major=element_blank(), #getting rid of majorgridlines
          panel.border=element_blank(),     #getting rid of minorgridlines  
          panel.grid.minor=element_blank(),
          axis.line.x=element_line(color="black", size = 1),
          axis.line.y=element_line(color="black", size = 1),
          axis.text.x=element_text(color="black", size=12),
          axis.text.y=element_text(color="black", size=12),
          axis.title.x=element_text(face = "bold", size=12, color="black", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y=element_text(face = "bold", size=12, color="black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
          legend.position="none",
          panel.background=element_blank(),
          plot.background=element_rect(fill="transparent", colour = NA))

setwd("/Users/krissales/Desktop")
ggsave("thlsurvival.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")


############  DESCRIPTIVE STATS ###############


describeBy(survival$Prop.survivors, list(survival$Regime,survival$Rearing),mat=TRUE) 
#    item group1 group2 vars  n      mean         sd median   trimmed      mad  min  max range        skew  kurtosis          se
# X11    1    KSS     30    1 17 0.9441176 0.03906066  0.950 0.9433333 0.074130 0.90 1.00  0.10  0.18186707 -1.439701 0.009473602
# X12    2  30thl     30    1  8 0.4750000 0.25354628  0.450 0.4750000 0.259455 0.15 0.85  0.70  0.23294564 -1.545596 0.089642146
# X13    3  38thl     30    1  8 0.9175000 0.09647353  0.950 0.9175000 0.074130 0.75 1.00  0.25 -0.63889157 -1.384457 0.034108545
# X14    4    KSS     38   NA NA        NA         NA     NA        NA       NA   NA   NA    NA          NA        NA          NA
# X15    5  30thl     38    1  8 0.3962500 0.28480256  0.385 0.3962500 0.355824 0.00 0.80  0.80 -0.03911602 -1.579574 0.100692912
# X16    6  38thl     38    1  8 0.6437500 0.26021626  0.665 0.6437500 0.274281 0.20 0.95  0.75 -0.34964586 -1.306520 0.092000340






####################  GLMS WITH NON NORMAL ERROR STRUCTURE #########################


#### Binomial family error structures
# As data is proportion bound at 0 and 1 fitting normal distibution does not give normal and homogenity of variance in residuals 
#! note only proportions without total so I cannot use cbind; raw data gained from Matt, can now use cbind



str(survival)

survival30oC$Cbind.survival <- cbind(survival30oC$Survivors , survival30oC$Deaths)
survivalnoKSS$Cbind.survival <- cbind(survivalnoKSS$Survivors , survivalnoKSS$Deaths)
survival38oC$Cbind.survival <- cbind(survival38oC$Survivors , survival38oC$Deaths)



### THL Comparison MODEL SELECTION #########

# Creating a global model
globalmodbinom<-glm(Cbind.survival ~ Regime * Rearing, binomial(link = "logit"), data=survivalnoKSS, na.action = na.exclude)
nointmodbinom<-glm(Cbind.survival ~ Regime + Rearing, binomial(link = "logit"), data=survivalnoKSS, na.action = na.exclude)
nullmodbinom<-glm(Cbind.survival ~ 1, binomial(link = "logit"), data=survivalnoKSS, na.action = na.exclude)

AIC(globalmodbinom);AIC(nullmodbinom)# 249. 363.1165

par(mfrow=c(2,2)); plot(globalmodbinom);par(mfrow=c(1,1))


pseudoR<-(globalmodbinom$null.deviance-globalmodbinom$deviance) / globalmodbinom$null.deviance # (thomas et al., 2015)
pseudoR# 0.4295218



############ THL Comparison  MODEL SIGNIFICANCE ###############

drop1(globalmodbinom, test= "Chi")
# Df Deviance    AIC    LRT  Pr(>Chi)    
# <none>              158.33 249.91                     
# Regime:Rearing  1   173.76 263.35 15.436 8.535e-05 ***

drop1(nointmodbinom, test= "Chi")
# Df Deviance    AIC    LRT  Pr(>Chi)    
# <none>       173.76 263.35                     
# Regime   1   256.91 344.50 83.149 < 2.2e-16 ***
# Rearing  1   197.38 284.97 23.621 1.173e-06 ***



lrtest(globalmodbinom, nullmodbinom)
#Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   4 -120.95                         
# 2   1 -180.56 -3 119.21  < 2.2e-16 ***
     


############ THL Comparison  MODEL POST HOC ###############
     
## summary
summary(globalmodbinom)
# glm(formula = Cbind.survival ~ Regime * Rearing, family = binomial(link = "logit"), 
#     data = survivalnoKSS, na.action = na.exclude)
# 
# Deviance Residuals: 
#      Min      1Q  Median      3Q     Max  
# -4.511  -1.260   0.141   1.853   3.671  
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -0.1001     0.1583  -0.632 0.527262    
# Regime38thl             2.5119     0.3300   7.612 2.69e-14 ***
# Rearing38              -0.3107     0.2269  -1.369 0.170864    
# Regime38thl:Rearing38  -1.5213     0.4039  -3.767 0.000166 ***
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 277.53  on 31  degrees of freedom
# Residual deviance: 158.33  on 28  degrees of freedom
# AIC: 249.91
# 
# Number of Fisher Scoring iterations: 5

## pairwise comparisons

lsmeans(globalmodbinom, pairwise~Regime*Rearing, adjust="tukey") 
# $contrasts
# contrast              estimate        SE df z.ratio p.value
# 30thl,30 - 38thl,30 -2.5118678 0.3299731 NA  -7.612  <.0001
# 30thl,30 - 30thl,38  0.3106587 0.2268523 NA   1.369  0.5186
# 30thl,30 - 38thl,38 -0.6799020 0.2300430 NA  -2.956  0.0165
# 38thl,30 - 30thl,38  2.8225266 0.3319924 NA   8.502  <.0001
# 38thl,30 - 38thl,38  1.8319659 0.3341807 NA   5.482  <.0001
# 30thl,38 - 38thl,38 -0.9905607 0.2329302 NA  -4.253  0.0001






############ Difference to KSS  MODEL SELECTION ###############


# Creating a global model
globalmodbinom<-glm(Cbind.survival ~ Regime, binomial(link = "logit"), data=survival30oC, na.action = na.exclude)
nullmodbinom<-glm(Cbind.survival ~ 1, binomial(link = "logit"), data=survival30oC, na.action = na.exclude)

AIC(globalmodbinom);AIC(nullmodbinom)# 143.7487 295.1392

par(mfrow=c(2,2)); plot(globalmodbinom);par(mfrow=c(1,1))

pseudoR<-(globalmodbinom$null.deviance-globalmodbinom$deviance) / globalmodbinom$null.deviance # (thomas et al., 2015)
pseudoR# 0.6867175

############ Difference to KSS  MODEL SIGNIFICANCE ###############

drop1(globalmodbinom, test= "Chi")
# Df Deviance    AIC    LRT  Pr(>Chi)    
# <none>       70.89 143.75                     
# Regime  2   226.28 295.14 155.39 < 2.2e-16 ***


lrtest(globalmodbinom, nullmodbinom)
#  #Df   LogLik Df  Chisq Pr(>Chisq)    
# 1   3  -68.874                         
# 2   1 -146.570 -2 155.39  < 2.2e-16 ***


############ Difference to KSS  MODEL POST HOC ###############

## summary
summary(globalmodbinom)
# glm(formula = Cbind.survival ~ Regime, family = binomial(link = "logit"), 
#     data = survival30oC, na.action = na.exclude)
# 
# Deviance Residuals: 
#      Min       1Q   Median       3Q      Max  
# -3.0768  -0.7813   0.1134   1.5143   3.5027  
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#      (Intercept)   2.8301     0.2361   11.99   <2e-16 ***
#      Regime30thl  -2.9302     0.2843  -10.31   <2e-16 ***
#      Regime38thl  -0.4183     0.3736   -1.12    0.263    
# ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 226.28  on 32  degrees of freedom
# Residual deviance:  70.89  on 30  degrees of freedom
# AIC: 143.75


## pairwise comparisons
lsmeans(globalmodbinom, pairwise~Regime, adjust="tukey")
# contrast        estimate        SE df z.ratio p.value
# KSS - 30thl    2.9301960 0.2842531 NA  10.308  <.0001
# KSS - 38thl    0.4183282 0.3735730 NA   1.120  0.5018
# 30thl - 38thl -2.5118678 0.3299731 NA  -7.612  <.0001




# 
# 
# ################## SIMPLE STATS #############################
# 
# 
# ######### within 30oC #################
# # kss
# hist(survival30oC$Prop.survivors[survival30oC$Regime == "KSS"], 
#      col = "black", density = 30, angle = 180, border = "red", 
#      main = list("kss", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5)  # keep nclass = 10, keep scales default
# # 30 thl
# hist(survival30oC$Prop.survivors[survival30oC$Regime == "30thl"], 
#      col = "black", density = 30, angle = 180, border = "red", 
#      main = list("30 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5)  # keep nclass = 10, keep scales default
# # 38 thl
# hist(survival30oC$Prop.survivors[survival30oC$Regime == "38thl"], 
#      col = "red", density = 30, angle = 180, border = "red", 
#      main = list("38 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5)  # keep nclass = 10, keep scales default
# ## groups often platykurtotic,  small samples with averages
# 
# ###### plotting differences
# # base boxplots of data distribution grouped by temperature
# boxplot(survival30oC$Prop.survivors ~ survival30oC$Regime, ylab="survival", xlab="Regime")
# ## 30 thl more variable 
# 
# ### kss not normal
# shapiro.test (survival30oC$Prop.survivors[survival30oC$Regime == "KSS"]) # W = 0.8124, p-value = 0.003016
# ks.test(survival30oC$Prop.survivors[survival30oC$Regime == "KSS"], pnorm)  # D = 0.81594, p-value = 2.954e-10
# ### 30thl ok
# shapiro.test (survival30oC$Prop.survivors[survival30oC$Regime == "30thl"]) # W = W = 0.93412, p-value = 0.5543
# ks.test(survival30oC$Prop.survivors[survival30oC$Regime == "30thl"], pnorm)  # D = 0.55962, p-value = 0.01333
# ### 38thl not normal
# shapiro.test (survival30oC$Prop.survivors[survival30oC$Regime == "38thl"]) # W = 0.83894, p-value = 0.07345
# ks.test(survival30oC$Prop.survivors[survival30oC$Regime == "38thl"], pnorm)  # D = 0.77337, p-value = 0.0001396
# 
# #! need library(car)
# ### different
# bartlett.test(survival30oC$Prop.survivors ~ survival30oC$Regime) # Bartlett's K-squared = 33.415, df = 2, p-value = 5.547e-08
# fligner.test(survival30oC$Prop.survivors ~ survival30oC$Regime) # Fligner-Killeen:med chi-squared = 11.328, df = 2, p-value = 0.003469
# leveneTest(survival30oC$Prop.survivors ~ survival30oC$Regime)   #Df F value Pr(>F) 9.3774 0.0006864 ***
# 
# 
# # ! different and not normal 
# 
# # 1. kurskal wallis
# kruskal.test(survival30oC$Prop.survivors ~ survival30oC$Regime)
# # Chi^2  = 17.123, df = 2, p-value = 0.0001913
# #! library(pgirmess)
# kruskalmc(survival30oC$Prop.survivors ~ survival30oC$Regime, probs = 0.05, cont=NULL)
# # KSS-30thl   16.297794     9.924916       TRUE
# # KSS-38thl    1.172794     9.924916      FALSE
# # 30thl-38thl 15.125000    11.574341       TRUE
# 
# 
# # 2. welches anova with ranks
# oneway.test(survival30oC$Prop.survivors~ survival30oC$Regime, var.equal=FALSE)
# # F 12.877, num df = 2.000, denom df = 10.114, p-value = 0.001659
# # ! library(userfriendlyscience)
# posthocTGH(y=survival30oC$Prop.survivors, x=survival30oC$Regime, method="games-howell")
# # diff ci.lo  ci.hi    t  df    p p.adjusted
# # 30thl-KSS   -0.469 -0.73 -0.205 5.20 7.2 <.01        .01
# # 38thl-KSS   -0.027 -0.13  0.074 0.75 8.1  .74        .74
# # 38thl-30thl  0.442  0.17  0.710 4.61 9.0 <.01        .01
# 
# # 
# # 
# # 
# # ######## within 38oC ##################
# # 
# # # 30 thl
# # hist(survival38oC$Prop.survivors[survival38oC$Regime == "30thl"], 
# #      col = "black", density = 30, angle = 180, border = "red", 
# #      main = list("30 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
# #      nclass = 5)  # keep nclass = 10, keep scales default
# # # 38 thl
# # hist(survival38oC$Prop.survivors[survival38oC$Regime == "38thl"], 
# #      col = "red", density = 30, angle = 180, border = "red", 
# #      main = list("38 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
# #      nclass = 5)  # keep nclass = 10, keep scales default
# # ## groups often platykurtotic,  small samples with averages
# # 
# # ###### plotting differences
# # # base boxplots of data distribution grouped by temperature
# # boxplot(survival38oC$Prop.survivors ~ survival38oC$Regime, ylab="survival", xlab="Regime")
# # ## 30 thl more variable 
# # 
# # 
# # ### 30thl ok
# # shapiro.test (survival38oC$Prop.survivors[survival38oC$Regime == "30thl"]) # W = 0.95762, p-value = 0.7871
# # ks.test(survival38oC$Prop.survivors[survival38oC$Regime == "30thl"], pnorm)  # D = 0.5, p-value = 0.02259
# # ### 38thl not normal
# # shapiro.test (survival38oC$Prop.survivors[survival38oC$Regime == "38thl"]) #W = 0.93358, p-value = 0.5493
# # ks.test(survival38oC$Prop.survivors[survival38oC$Regime == "38thl"], pnorm)  # D = 0.57926, p-value = 0.009321
# # 
# # #! need library(car)
# # ### similar
# # bartlett.test(survival38oC$Prop.survivors ~ survival38oC$Regime) # Bartlett's K-squared =  0.053181, df = 1, p-value = 0.8176
# # fligner.test(survival38oC$Prop.survivors ~ survival38oC$Regime) # Fligner-Killeen:med chi-squared = 0.45455, df = 1, p-value = 0.5002
# # leveneTest(survival38oC$Prop.survivors ~ survival38oC$Regime)   #Df F value Pr(>F) 1    0.16 0.6952
# # 
# # 
# # # ! similar and not normal 
# # 
# # wilcox.test(survival38oC$Prop.survivors ~ survival38oC$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE)
# # # W = 17, p-value = 0.1275
# # 
# # 
# # 
# # 
# # 
# # ######## within 30 thl ################
# # 
# # # 30 thl at 30 oc normal
# # # 30 thl at 38 oc normal
# # 
# # ###### plotting differences
# # # base boxplots of data distribution grouped by temperature
# # boxplot(survival30thl$Prop.survivors ~ survival30thl$Rearing, ylab="survival", xlab="Regime")
# # ## 30 thl more variable 
# # 
# # #! need library(car)
# # ### similar
# # bartlett.test(survival30thl$Prop.survivors ~ survival30thl$Rearing) # Bartlett's K-squared =   0.088093, df = 1, p-value = 0.7666
# # fligner.test(survival30thl$Prop.survivors ~ survival30thl$Rearing) # Fligner-Killeen:med chi-squared = 0.33865, df = 1, p-value = 0.5606
# # leveneTest(survival30thl$Prop.survivors ~ survival30thl$Rearing)   #Df F value Pr(>F) 1  0.1818 0.6763
# # 
# # 
# # # ! similar and  normal 
# # t.test(survival30thl$Prop.survivors ~ survival30thl$Rearing, var.equal = TRUE, paired = FALSE)
# # # t = 0.58414, df = 14, p-value = 0.5684
# # 
# # 
# # 
# # 
# # ####### within 38 thl #################
# # 
# # 
# # # 30 thl at 30 oc not normal
# # # 30 thl at 38 oc not normal
# # 
# # ###### plotting differences
# # # base boxplots of data distribution grouped by temperature
# # boxplot(survival38thl$Prop.survivors ~ survival38thl$Rearing, ylab="survival", xlab="Regime")
# # ## 30 thl more variable 
# # 
# # #! need library(car)
# # ### similar just
# # bartlett.test(survival38thl$Prop.survivors ~ survival38thl$Rearing) # Bartlett's K-squared =   5.5911, df = 1, p-value = 0.01805
# # fligner.test(survival38thl$Prop.survivors ~ survival38thl$Rearing) # Fligner-Killeen:med chi-squared = 3.3163, df = 1, p-value = 0.06859
# # leveneTest(survival38thl$Prop.survivors ~ survival38thl$Rearing)   #Df F value Pr(>F)1   3.479 0.08325 .
# # 
# # 
# # # ! similar and normal
# # t.test(survival38thl$Prop.survivors ~ survival38thl$Rearing, var.equal = TRUE, paired = FALSE)
# # # t = 2.79, df = 14, p-value = 0.01446
# # 
# # 
# # 
