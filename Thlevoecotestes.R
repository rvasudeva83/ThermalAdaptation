ls()
# remove everything
rm(list=ls())


library(car); library(MASS); library (lme4); library (nlme);  library(lmtest); 
library(ggplot2); library(MuMIn); library(emmeans); library(psych);
library(LMERConvenienceFunctions); library(car); library(AER); library(MASS); library(RLRsim)



temp <- expression(paste('Temperature (',degree,'C)',sep='')) #the temperature label with degrees sign

#check what working directory is set
getwd()
#setting to required one where data is based
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")
list.files()


############################ ACCLIMATION CHECK ##########################################

#reading in data 
acclcheckave <- read.csv("Thlevoacclimtestesave.csv", header = TRUE) # full data set 
str(acclcheckave)
acclcheckave$Regime<-as.factor(acclcheckave$Regime) # changing to categorical factor as coding is not proportional to differences
acclcheckave$Rearing<-as.factor(acclcheckave$Rearing) # changing to categorical factor as coding is not proportional to differences

acclchecktotal <- read.csv("Thlevoacclimtestesfull.csv", header = TRUE) # full data set 
str(acclchecktotal)
# $ ID           : chr  "30.3" "30.3" "30.3" "30.3" ...
# $ Line         : num  30.3 30.3 30.3 30.3 30.4 30.4 30.4 30.4 30.6 30.6 ...            thl population code
# $ Replicate    : int  1 2 3 4 1 2 3 4 1 2 ...
# $ Regime       : int  30 30 30 30 30 30 30 30 30 30 ...                                ancestral tempearture evolution
# $ Rearing      : int  38 38 38 38 38 38 38 38 38 38 ...                                developmental temperature
# $ Elytra_length: num  2.32 2.46 2.36 2.39 2.29 2.29 2.44 2.18 2.33 2.16 ...
# $ Testes_size  : num  0.276 0.216 0.324 0.252 0.24 0.24 0.168 0.216 0.288 0.108 ...    testes size



acclchecktotal$ID<-as.factor(acclchecktotal$ID) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Line <-as.factor(acclchecktotal$Line ) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Regime<-as.factor(acclchecktotal$Regime) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Rearing <-as.factor(acclchecktotal$Rearing) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Replicate <-as.factor(acclchecktotal$Replicate) # changing to categorical factor as coding is not proportional to differences





###################  FIGURE 4A PLOT #####################

xlabel <- expression("Focal generation rearing temperautre "(degree*C))



graphtestestotal<-ggplot(acclchecktotal, aes(x=Rearing, y=Testes_size, fill= Regime)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
     geom_boxplot(notch=F,  #change to F if want to get rid of notchs
                  outlier.shape= NA, #shape of the outlier (hashtag out if dont want outliers marked)
                  width=0.5,
                  lwd=0.5,
                  fatten=0.5,
                  color="black",
                  position=position_dodge(0.5)) + #size of the outlier (hashtag out if dont want outliers marked)
     stat_summary(fun.y="mean", geom= "point", size=3, position=position_dodge(0.5), color="black") + 
     scale_fill_manual(values=c("cadetblue1", "darkorange1"), # changes the colour of the bars
                       name = temp, #adds in temperature label on the legend
                       breaks = c("30","38"), #the order listed in the legend
                       label = c("30","38")) + #how things are labeled in the lgend
     scale_colour_manual(values=c("black", "black")) +
     geom_point(position=position_jitterdodge(dodge.width=0.5, jitter.width=0.15), shape=1, size= 1.5) +
     labs (x= expression(bold("Developmental temperature "( degree*C))),
           y= expression(bold(Testes~volume~(mm^{3})))) +  #adding title to the x axis and y axis
     scale_x_discrete(breaks=c("30", "38"), #the order of the variables on the x axis
                      labels=c("30", "38")) + # the names on the x axis
     coord_cartesian(ylim=c(-0.02, 0.52)) + #set axis limits
     scale_y_continuous(breaks=seq(0, 0.5, 0.1), #ticks from 0 to 16000 and show number every 16000
                        expand = c(0, 0)) + #cuts the axis off at 0
     annotate("text", x=c(0.85,1.15,1.5,1.85,2.15),
              y= c(0.48,0.48,0.5,0.48,0.48),  
           label= c("A","B","***","B","A"),
           size= 4) +
     theme_classic() + #the theme of the whole plot 
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
ggsave("graphtestestotal.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")




###################  FIGURE 4B PLOT #####################



### Errorbar plot of total counts at each timepoint
## Making dataframe

acclchecktotalbar <- acclchecktotal


barchartacclimation <- aggregate(acclchecktotalbar$Testes_size,
                                by = list(Line = acclchecktotalbar$Line, Regime = acclchecktotalbar$Regime, Rear = acclchecktotalbar$Rearing),
                                FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                    n = length(x))) # aggregate data frame by 2 factors counts by temp and life stage (tapply can only do 1 factor)) then get matrix of descriptives
barchartacclimation

barchartacclimation <- do.call(data.frame, barchartacclimation) # formatting
barchartacclimation

barchartacclimation$se <- barchartacclimation$x.sd / sqrt(barchartacclimation$x.n) # SE calculation
colnames(barchartacclimation) <- c("Line", "Regime", "Rear", "mean", "sd", "n", "se") #renaming columns

barchartacclimation$upse <- barchartacclimation$mean + barchartacclimation$se # Upper CI
barchartacclimation$lowse  <- barchartacclimation$mean - barchartacclimation$se # Lower CI
#barchartacclimation$ci <- barchartacclimation$se * 1.96 # CI calculation
#barchartacclimation$upci <- barchartacclimation$mean + barchartacclimation$ci # Upper CI
#barchartacclimation$lowci  <- barchartacclimation$mean - barchartacclimation$ci # Lower CI
barchartacclimation


## plotting data

# with solid errorbars


# with solid errorbars
graphtestespattern<-ggplot(barchartacclimation, aes(x= Rear, y=mean, group= Line, colour= Line)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
     #geom_bar(stat="identity", position=position_dodge(1), width=1) +
     geom_errorbar(aes(ymax = upse, ymin = lowse),
                   width=0.1,
                   lwd=1,
                   position=position_dodge(0.3)) +
     geom_line(aes(linetype= Line),
               lwd=1,
               position=position_dodge(0.3)) +
     scale_colour_manual(values=c("cadetblue1","cadetblue1","cadetblue1","cadetblue1","cadetblue1","cadetblue1","cadetblue1","cadetblue1", "darkorange1","darkorange1","darkorange1","darkorange1","darkorange1","darkorange1","darkorange1","darkorange1")) + # colour of the bars
     scale_linetype_manual(values=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) +
     theme_classic() + #the theme of the whole plot
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
          plot.background=element_rect(fill="transparent", colour = NA)) +
  coord_cartesian(ylim=c(-0.02, 0.52)) + #set axis limits
  scale_y_continuous(breaks=seq(0, 0.5, 0.1), #ticks from 0 to 16000 and show number every 16000
                     expand = c(0, 0)) + #cuts the axis off at 0  
     annotate("text", x=c(0.85,1.15,1.5,1.85,2.15),
           y= c(0.48,0.48,0.5,0.48,0.48),  
           label= c("A","B","***","B","A"),
           size= 4) +
     labs (x= expression(bold("Developmental temperature "( degree*C))),
           y= expression(bold(Testes~volume~(mm^{3})))) + # x and y axis titles
     scale_x_discrete(breaks=c("30", "38"), #the order of the variables on the x axis
                      labels=c("30", "38")) 
     
setwd("/Users/krissales/Desktop")
ggsave("graphtestespattern.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")



 

 ###################   DESCRIPTIVE STATS ##################### 
 
 
 #gives you vars  n, mean, sd,  median,  trimmed, mad, min, max, range, skew, kurtosis, se
 describeBy(acclchecktotal$Testes_size, list(acclchecktotal$Regime,acclchecktotal$Rearing),mat=TRUE) 
#    item group1 group2 vars  n      mean        sd median   trimmed      mad min max range        skew    kurtosis       se
# X11    1     30     30    1 32 0.284625 0.07670040  0.282 0.2829231 0.0800604 0.144 0.444 0.300  0.1706908 -0.80658678 0.013558844
# X12    2     38     30    1 32 0.202500 0.05559995  0.210 0.2021538 0.0444780 0.072 0.360 0.288  0.2091222  0.73323399 0.009828776
# X13    3     30     38    1 32 0.224250 0.05447639  0.216 0.2252308 0.0444780 0.096 0.336 0.240 -0.1877437  0.08646041 0.009630157
# X14    4     38     38    1 32 0.284625 0.05237196  0.288 0.2847692 0.0533736 0.180 0.384 0.204 -0.1406248 -0.84285980 0.009258143
#  


 
 
 ##############   MODEL SELECTION  #####################

 
 str(acclchecktotal)
 
 #### Poisson family error structures
 # As data is a discrete and 0 bounded count poisson best
 
 
 # Creating models 
 #! data comes from subpopulation sturcture (lines) within regimes. Lines uniquely coded so no nesting required

 # All data gaussian
 globalmixmod<- glmer(Testes_size ~ Regime*Rearing  + Elytra_length + (1|Line), data=acclchecktotal, family=gaussian(link = "identity"), na.action=na.exclude) 
# boundary (singular) fit: see help('isSingular') line little effect.
 nointmixmod<- glmer(Testes_size ~ Regime+Rearing  + Elytra_length + (1|Line), data=acclchecktotal, family=gaussian(link = "identity"), na.action=na.exclude) 
 
 nullmod<- glmer(Testes_size ~ 1 + (1|Line), data=acclchecktotal, family=gaussian(link = "identity"), na.action=na.exclude) 
 
 
 nullrandmixmod<- glm(Testes_size ~ Regime*Rearing  + Elytra_length, data=acclchecktotal, family=gaussian(link = "identity"), na.action=na.exclude) 
 summary(globalmixmod)
# https://stats.stackexchange.com/questions/511868/how-to-solve-error-in-lme
 # https://stats.stackexchange.com/questions/269224/how-to-obtain-reml-estimates-in-a-glmer-optimizing-random-effects-structure-in
 # https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-significance-of-random-effects
 
 anova(globalmixmod, nullrandmixmod)
 AICc(globalmixmod, nullrandmixmod)
 
 r.squaredGLMM(globalmixmod)
 #     R2m       R2c 
 # 0.2749456 0.2749456
 
 randomtest<- glm(Testes_size ~ Line, data=acclchecktotal, family=gaussian(link = "identity"), na.action=na.exclude) 
 drop1(randomtest, test = "F") # line doesn't seem influential but is being kept as experimentally imporotant 
 
 # Assumptions 
 par(mfrow=c(1,1))
 
 sresid<-resid(globalmixmod, type="pearson"); hist(sresid) # not necassary for normal residuals, but looks it
 fits<-fitted(globalmixmod); plot(sresid~fits) # checking for heteroscedasicity; little wedge /trending, formation/link/error family change not needed

 mcp.fnc(globalmixmod)
 # plotLMER.fnc(globalmixmod)
 
 plot(sresid~acclchecktotal$Line)# no patterns no new x/interactions etc needed
 plot(sresid~acclchecktotal$Rep)
 
 ##############  MODEL SIGNIFICANCE  #####################
# anova(globalmixmod, nullmod)  lrtest(globalmixmod, nullmod) also produce F

 
  # regimes behave differently; interaction significant
 drop1(globalmixmod, test ="Chi")
 # Adult.count20D ~ Regime * Rearing + (1 | Line) 
 #            Df         AIC     LRT       Pr(Chi)
 # <none>              -346.21                     
 # Elytra_length     1 -346.54  1.672     0.196    
 # Regime:Rearing    1 -307.51 40.706 1.769e-10 ***
 # 
 # thl lines across temperatures no different
 # effect of temperatures
 drop1(nointmixmod, test = "Chi")
 # Adult.count.20D ~ Regime + Heatwave.temp + (1 | Line)                                   
 #                 Df    AIC    LRT  Pr(Chi)   
 # <none>             -307.51                
 # Regime           1 -308.80 0.70497  0.4011
 # Rearing          1 -308.71 0.79308  0.3732
 # Elytra_length    1 -309.48 0.02746  0.8684
 
 # overall model significant
 lrtest(globalmixmod, nullmod)
 #   #Df  LogLik Df Chisq Pr(>Chisq)   
 # 1   7 163.65                        
 # 2   3 154.79 -4 17.715   0.001403 **
 
 
 ############## MODEL POST HOC  #####################
  
 summary(globalmixmod)
 # Linear mixed model fit by REML ['lmerMod']
 # Formula: Testes_size ~ Regime * Rearing + Elytra_length + (1 | Line)
 # Data: acclchecktotal
 # 
 # REML criterion at convergence: -327.3
 # 
 # Scaled residuals: 
 #   Min      1Q  Median      3Q     Max 
 # -2.2375 -0.6107  0.0590  0.5967  2.7506 
 # 
 # Random effects:
 #   Groups   Name        Variance Std.Dev.
 # Line     (Intercept) 0.000000 0.00000 
 # Residual             0.003653 0.06044 
 # Number of obs: 128, groups:  Line, 16
 # 
 # Fixed effects:
 #                    Estimate Std. Error t value
 # (Intercept)         0.11043    0.13740   0.804
 # Regime38           -0.08178    0.01511  -5.411
 # Rearing38          -0.05790    0.01523  -3.800
 # Elytra_length       0.07404    0.05822   1.272
 # Regime38:Rearing38  0.14690    0.02165   6.786
 # 
 # Correlation of Fixed Effects:
 #   (Intr) Regm38 Rrng38 Elytr_
 # Regime38    -0.073                     
 # Rearing38   -0.182  0.498              
 # Elytr_lngth -0.997  0.018  0.128       
 # Rgm38:Rrn38 -0.121 -0.695 -0.672  0.160
 # optimizer (nloptwrap) convergence code: 0 (OK)
 # boundary (singular) fit: see help('isSingular')
 

 lsmeans(globalmixmod, pairwise~Regime*Rearing , adjust="tukey") 
 #                        contrast         estimate     SE    df z.ratio p.value
 # Regime30 Rearing30 - Regime38 Rearing30  0.08178 0.0151  48.2   5.411  <.0001 
 # Regime30 Rearing30 - Regime30 Rearing38  0.05790 0.0152 110.2   3.798  0.0013 
 # Regime30 Rearing30 - Regime38 Rearing38 -0.00722 0.0162  51.9  -0.445  0.9702 ==
 # Regime38 Rearing30 - Regime30 Rearing38 -0.02388 0.0152  48.5  -1.570  0.4050 ==
 # Regime38 Rearing30 - Regime38 Rearing38 -0.08900 0.0161 116.0  -5.524  <.0001
 # Regime30 Rearing38 - Regime38 Rearing38 -0.06512 0.0156  49.9  -4.175  0.0007
 
 
 
 # #################################### ACCLIMATION SIMPLE ANALYSIS ########################
 # #! Note that to avoid pseudo replication averages ave being used for this analysis 
 # str(acclcheckave)
 # 
 # summary(acclcheckave) 
 # # Regime  Rearing.Temp      Line       Adult.count.20D
 # # 30:15   30:16        Min.   :30.11   Min.   :  0.0  
 # # 38:16   38:15        1st Qu.:30.35   1st Qu.: 77.2  
 # #                      Median :38.10   Median :116.0  
 # #                      Mean   :34.54   Mean   :113.3  
 # #                      3rd Qu.:38.35   3rd Qu.:138.5  
 # #                      Max.   :38.80   Max.   :262.5  
 # 
 # 
 # 
 # 
 # #### ! library(psych)
 # #gives you vars  n, mean, sd,  median,  trimmed, mad, min, max, range, skew, kurtosis, se
 # describeBy(acclcheckave$Adult.count.20D, list(acclcheckave$Regime,acclcheckave$Rearing.Temp),mat=TRUE) 
 # #     item group1 group2 vars n      mean       sd  median   trimmed      mad     min    max    range        skew  kurtosis       se
 # # X11    1     30     30    1 8  96.36666 33.60472 100.775  96.36666 45.03397 55.3333 137.40  82.0667 -0.06466746 -2.021438 11.88106
 # # X12    2     38     30    1 8 176.51250 64.87798 184.200 176.51250 77.98476 72.2000 262.50 190.3000 -0.19847208 -1.518422 22.93783
 # # X13    3     30     38    1 7  82.90714 62.47202 101.200  82.90714 28.16940  0.0000 168.75 168.7500 -0.25695578 -1.584581 23.61221
 # # X14    4     38     38    1 8  93.54584 50.76787  93.100  93.54584 58.48857  4.6667 161.80 157.1333 -0.33679859 -1.228791 17.94915
 # 
 # names(acclcheckave)
 # 
 # acclimation30oc  <- acclcheckave[acclcheckave$Rearing.Temp== "30",]
 # acclimation30oc$Rearing.Temp <- NULL
 # acclimation38oc <- acclcheckave[acclcheckave$Rearing.Temp== "38",]
 # acclimation38oc$Rearing.Temp <- NULL
 # acclimation30thl  <- acclcheckave[acclcheckave$Regime== "30",]
 # acclimation30thl$Regime <- NULL
 # acclimation38thl <- acclcheckave[acclcheckave$Regime== "38",]
 # acclimation38thl$Regime <- NULL
 # 
 # 
 
 
 # ################## SIMPLE STATS #############################
 # 
 # 
 # ######### within 30oC #################
 # # 30 thl
 # hist(acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "30"], 
 #      col = "black", density = 30, angle = 180, border = "red", 
 #      main = list("30 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
 #      nclass = 5)  # keep nclass = 10, keep scales default
 # # 38 thl
 # hist(acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "38"], 
 #      col = "red", density = 30, angle = 180, border = "red", 
 #      main = list("38 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
 #      nclass = 5)  # keep nclass = 10, keep scales default
 # ## groups often platykurtotic,  small samples with averages
 # 
 # ###### plotting differences
 # # base boxplots of data distribution grouped by temperature
 # boxplot(acclimation30oc$Adult.count.20D ~ acclimation30oc$Regime, ylab="survival", xlab="Regime")
 # ## 38 thl more variable 
 # 
 # 
 # ### 30thl not normal
 # shapiro.test (acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "30"]) # W = 0.87071, p-value = 0.1531
 # ks.test(acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "30"], pnorm)  # D = D = 1, p-value < 2.2e-16
 # ### 38thl not normal
 # shapiro.test (acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "38"]) # W = 0.97529, p-value = 0.9359
 # ks.test(acclimation30oc$Adult.count.20D[acclimation30oc$Regime == "38"], pnorm)  # D = 1, p-value < 2.2e-16
 # 
 # #! need library(car)
 # ### simialr
 # bartlett.test(acclimation30oc$Adult.count.20D ~ acclimation30oc$Regime) # Bartlett's K-squared = 2.6442, df = 1, p-value = 0.1039
 # fligner.test(acclimation30oc$Adult.count.20D ~ acclimation30oc$Regime) # Fligner-Killeen:med chi-squared = 2.4887, df = 1, p-value = 0.1147
 # leveneTest(acclimation30oc$Adult.count.20D ~ acclimation30oc$Regime)   #Df F value Pr(>F) 1  2.7274 0.1209
 # 
 # 
 # # ! similar and not normal 
 # wilcox.test(acclimation30oc$Adult.count.20D ~ acclimation30oc$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE)
 # # W = 9, p-value = 0.01476
 # 
 # 
 # 
 # ######## within 38oC ##################
 # 
 # # 30 thl
 # hist(acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "30"], 
 #      col = "black", density = 30, angle = 180, border = "red", 
 #      main = list("30 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
 #      nclass = 5)  # keep nclass = 10, keep scales default
 # # 38 thl
 # hist(acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "38"], 
 #      col = "red", density = 30, angle = 180, border = "red", 
 #      main = list("38 thl", cex = 2), xlab = "survival", ylab ="Frequency", ylim = c(0,20),
 #      nclass = 5)  # keep nclass = 10, keep scales default
 # ## groups often platykurtotic,  small samples with averages
 # 
 # ###### plotting differences
 # # base boxplots of data distribution grouped by temperature
 # boxplot(acclimation38oc$Adult.count.20D ~ acclimation38oc$Regime, ylab="survival", xlab="Regime")
 # ## 30 thl slightly more variable 
 # 
 # 
 # ### 30thl ok
 # shapiro.test (acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "30"]) # 0.8945, p-value = 0.299
 # ks.test(acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "30"], pnorm)  # D = 0.71429, p-value = 0.001581
 # ### 38thl ok
 # shapiro.test (acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "38"]) #W = 0.97253, p-value = 0.9171
 # ks.test(acclimation38oc$Adult.count.20D[acclimation38oc$Regime == "38"], pnorm)  # D = 1, p-value < 2.2e-16
 # 
 # #! need library(car)
 # ### similar
 # bartlett.test(acclimation38oc$Adult.count.20D ~ acclimation38oc$Regime) # Bartlett's K-squared =   0.25898, df = 1, p-value = 0.6108
 # fligner.test(acclimation38oc$Adult.count.20D ~ acclimation38oc$Regime) # Fligner-Killeen:med chi-squared =  0.24236, df = 1, p-value = 0.6225
 # leveneTest(acclimation38oc$Adult.count.20D ~ acclimation38oc$Regime)   #Df F value Pr(>F)  1   0.125 0.7293
 # 
 # 
 # # ! similar and not normal 
 # wilcox.test(acclimation38oc$Adult.count.20D ~ acclimation38oc$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE)
 # #  W = 25, p-value = 0.7721
 # 
 # 
 # 
 # 
 # 
 # ######## within 30 thl ################
 # 
 # # 30 thl at 30 oc not normal
 # # 30 thl at 38 oc not normal
 # 
 # ###### plotting differences
 # # base boxplots of data distribution grouped by temperature
 # boxplot(acclimation30thl$Adult.count.20D ~ acclimation30thl$Rearing.Temp, ylab="survival", xlab="Regime")
 # ## 38 thl slightly more variable 
 # 
 # 
 # 
 # #! need library(car)
 # ### similar
 # bartlett.test(acclimation30thl$Adult.count.20D ~ acclimation30thl$Rearing.Temp) # Bartlett's K-squared =   2.237, df = 1, p-value = 0.1347
 # fligner.test(acclimation30thl$Adult.count.20D ~ acclimation30thl$Rearing.Temp) # Fligner-Killeen:med chi-squared = 0.088369, df = 1, p-value = 0.7663
 # leveneTest(acclimation30thl$Adult.count.20D ~ acclimation30thl$Rearing.Temp)   #Df F value Pr(>F) 1   0.868 0.3685
 # 
 # 
 # # ! similar and  not normal 
 # acclimation30thlno30.4 <- acclimation30thl[acclimation30thl$Line != "30.4",]
 # wilcox.test(acclimation30thlno30.4$Adult.count.20D ~ acclimation30thlno30.4$Rearing.Temp, exact = TRUE, conf.int = TRUE, paired = TRUE)
 # # V = 19, p-value = 0.4688
 # 
 # 
 # 
 # 
 # ####### within 38 thl #################
 # 
 # 
 # # 30 thl at 30 oc not normal
 # # 30 thl at 38 oc not normal
 # 
 # ###### plotting differences
 # # base boxplots of data distribution grouped by temperature
 # boxplot(acclimation38thl$Adult.count.20D ~ acclimation38thl$Rearing.Temp, ylab="survival", xlab="Regime")
 # ## 30 thl slightly more variable 
 # 
 # #! need library(car)
 # ### similar just
 # bartlett.test(acclimation38thl$Adult.count.20D ~ acclimation38thl$Rearing.Temp) # Bartlett's K-squared =   0.38907, df = 1, p-value = 0.5328
 # fligner.test(acclimation38thl$Adult.count.20D ~ acclimation38thl$Rearing.Temp) # Fligner-Killeen:med chi-squared = 0.70186, df = 1, p-value = 0.4022
 # leveneTest(acclimation38thl$Adult.count.20D ~ acclimation38thl$Rearing.Temp)   #Df F value Pr(>F) 1  0.6683 0.4273
 # 
 # 
 # # ! similar and not normal
 # wilcox.test(acclimation38thl$Adult.count.20D ~ acclimation38thl$Rearing.Temp, exact = TRUE, conf.int = TRUE, paired = FALSE)
 # # V = 32, p-value = 0.05469
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 