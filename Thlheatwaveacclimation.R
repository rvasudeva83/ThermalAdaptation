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
acclcheckave <- read.csv("Thlheatwaveacclimationave.csv", header = TRUE) # full data set 

acclcheckave$Regime<-as.factor(acclcheckave$Regime) # changing to categorical factor as coding is not proportional to differences
acclcheckave$Rearing.Temp<-as.factor(acclcheckave$Rearing.Temp) # changing to categorical factor as coding is not proportional to differences
acclcheckave$Line<-as.factor(acclcheckave$Line)
str(acclcheckave)


acclchecktotal <- read.csv("Thlheatwaveacclimationfull.csv", header = TRUE) # full data set 

acclchecktotal$Regime<-as.factor(acclchecktotal$Regime) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Replicate<-as.factor(acclchecktotal$Replicate) # changing to categorical factor as coding is not proportional to differences
acclchecktotal$Line<-as.factor(acclchecktotal$Line)
acclchecktotal$Rearing<-as.factor(acclchecktotal$Rearing)

str(acclchecktotal)
# 'data.frame':	134 obs. of  7 variables:                    
# $ Line             : Factor w/ 16 levels "30.11","30.13",..: 4 4 4 4 5 5 5 5 5 6 ...         thl population code
# $ Replicate        : Factor w/ 5 levels "1","2","3","4",..: 1 2 3 5 1 2 3 4 5 1 ...          
# $ Regime           : Factor w/ 2 levels "30","38": 1 1 1 1 1 1 1 1 1 1 ...                   ancestral tempearture evolution
# $ Rearing          : Factor w/ 2 levels "30","38": 1 1 1 1 1 1 1 1 1 1 ...                   developmental temperature
# $ Adult.count.10D.1: int  35 47 140 82 1 99 111 0 44 46 ...                                  
# $ Adult.count.10D.2: int  0 0 99 107 0 0 42 0 0 0 ...                                        
# $ Adult.count20D   : int  35 47 239 189 1 99 153 0 44 46 ..                                  offspring count 20 day reproductive fitness


###################  FIGURE 4A PLOT #####################

xlabel <- expression("Focal generation rearing temperautre "(degree*C))


graphacclimtotal<-ggplot(acclchecktotal, aes(x=Rearing, y=Adult.count20D, fill= Regime)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
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
           y= expression(bold(atop("20 day offspring production following", paste("heatwave"))))) +  #adding title to the x axis and y axis
     scale_x_discrete(breaks=c("30", "38"), #the order of the variables on the x axis
                      labels=c("30", "38")) + # the names on the x axis
     coord_cartesian(ylim=c(-5, 422)) + #set axis limits
     scale_y_continuous(breaks=seq(0, 400, 100), #ticks from 0 to 16000 and show number every 16000
                        expand = c(0, 0)) + #cuts the axis off at 0
     annotate("text", x=c(0.85,1.15,1.5,1.85,2.15),
              y= c(408,408,416,408,408),  
           label= c("AB","A","***","B","AB"),
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
ggsave("graphacclimtotal.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")





###################  FIGURE 4B PLOT #####################



### Errorbar plot of total counts at each timepoint
## Making dataframe

acclchecktotalbar <- acclchecktotal
str(acclchecktotal)


barchartacclimation <- aggregate(acclchecktotalbar$Adult.count20D,
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
graphaacclimpattern<-ggplot(barchartacclimation, aes(x= Rear, y=mean, group= Line, colour= Line)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
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
     labs (x= expression(bold("Developmental temperature "( degree*C))),
           y= expression(bold(atop("Mean 20 day offspring production following", paste("heatwave +/- SE"))))) + # x and y axis titles
     scale_x_discrete(breaks=c("30", "38"), #the order of the variables on the x axis
                      labels=c("30", "38")) +
  annotate("text", x=c(0.85,1.15,1.5,1.85,2.15),
           y= c(408,408,416,408,408),  
           label= c("AB","A","***","B","AB"),
           size= 4) +
     coord_cartesian(ylim=c(-5, 422)) + # set axis limits
     scale_y_continuous(breaks=seq(0, 400, 100), expand = c(0,0))  #ticks every 0.2


setwd("/Users/krissales/Desktop")
ggsave("graphaacclimpattern.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")


 

 ###################   DESCRIPTIVE STATS ##################### 
 

 describeBy(acclchecktotal$Adult.count20D, list(acclchecktotal$Regime,acclchecktotal$Rearing),mat=TRUE) 
 #     item group1 group2 vars  n      mean        sd median   trimmed      mad min max range        skew    kurtosis       se
 # X11    1     30     30    1 31  94.83871  85.26199     99  86.52000  94.8864   0 339   339  0.74728367  0.08160194 15.31350
 # X12    2     38     30    1 39 174.30769 114.98585    167 173.96970 154.1904   0 386   386 -0.01303917 -1.19837004 18.41247
 # X13    3     30     38    1 29  94.24138 128.55645      1  80.96000   1.4826   0 387   387  0.96916049 -0.70153262 23.87233
 # X14    4     38     38    1 35 102.57143  94.15672    102  93.65517 111.1950   0 309   309  0.62006700 -0.73713483 15.91539
 
 acclchecktotalno30.4 <- acclchecktotal[acclchecktotal$Line != "30.4",]
 describeBy(acclchecktotalno30.4$Adult.count20D, list(acclchecktotalno30.4$Regime,acclchecktotalno30.4$Rearing),mat=TRUE) 
 # item group1 group2 vars  n      mean        sd median   trimmed      mad min max range        skew   kurtosis       se
 # X11    1     30     30    1 26 101.65385  87.88535     99  93.86364  94.8864   0 339   339  0.68085038 -0.1049747 17.23574
 # X12    2     38     30    1 39 174.30769 114.98585    167 173.96970 154.1904   0 386   386 -0.01303917 -1.1983700 18.41247
 # X13    3     30     38    1 29  94.24138 128.55645      1  80.96000   1.4826   0 387   387  0.96916049 -0.7015326 23.87233
 # X14    4     38     38    1 35 102.57143  94.15672    102  93.65517 111.1950   0 309   309  0.62006700 -0.7371348 15.91539
 

 
 
 ##############   MODEL SELECTION  #####################

 
 str(acclchecktotal)
 
 #### Poisson family error structures
 # As data is a discrete and 0 bounded count poisson best
 
 
 # Creating models 
 #! data comes from subpopulation sturcture (lines) within regimes. Lines uniquely coded so no nesting required

 # All data poisson
  
 globalmixmod<- glmer(Adult.count20D ~ Regime*Rearing  + (1|Line), data=acclchecktotal, family=poisson(link = "log"),  na.action=na.exclude) 
 summary(globalmixmod)
 #                    Estimate Std. Error z value Pr(>|z|)    
 # (Intercept)         4.37036    0.14987  29.160   <2e-16 ***
 # Regime38            0.76331    0.21113   3.615   0.0003 ***
 # Rearing38          -0.03280    0.02739  -1.198   0.2310    
 # Regime38:Rearing38 -0.51306    0.03441 -14.910   <2e-16 ***
 
 
 # http://glmm.wikidot.com/faq
 overdisp_fun <- function(model) {
      ## number of variance parameters in 
      ##   an n-by-n variance-covariance matrix
      vpars <- function(m) {
           nrow(m)*(nrow(m)+1)/2
      }
      model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
      rdf <- nrow(model.frame(model))-model.df
      rp <- residuals(model,type="pearson")
      Pearson.chisq <- sum(rp^2)
      prat <- Pearson.chisq/rdf
      pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
      c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
 }
 
 overdisp_fun(globalmixmod) # overdispersed
 # chisq       ratio         rdf           p 
 # 12023.11850    93.20247   129.00000     0.00000 
 
 r.squaredGLMM(globalmixmod)
 #     R2m       R2c 
 # 0.3718752 0.9708585
 
 
 # Minus 30.4 as unbalanced across treatments. slightly more over dispersed slightly hiher R*2 for random factors
 
 
 globalmixmod<- glmer(Adult.count20D ~ Regime*Rearing  + (1|Line), data=acclchecktotalno30.4, family=poisson(link = "log"),  na.action=na.exclude) 
 summary(globalmixmod)
 #                    Estimate Std. Error z value Pr(>|z|)    
 # (Intercept)         4.41144    0.16266  27.121  < 2e-16 ***
 # Regime38            0.72222    0.22192   3.254  0.00114 ** 
 # Rearing38          -0.03342    0.02740  -1.220  0.22253    
 # Regime38:Rearing38 -0.51245    0.03442 -14.889  < 2e-16 ***
 
 overdisp_fun(globalmixmod) # overdispersed
 # chisq       ratio         rdf           p
 # 11728.87101    94.58767   124.00000     0.00000
 r.squaredGLMM(globalmixmod)
 #       R2m       R2c 
 # 0.3461672 0.9714784
 
 


 # Models overdispersed, atempt to fix with negative binomial, doesn't iterate properly estimates not changing much
globalmixmodnegbin<- glmer.nb(Adult.count20D ~ Regime*Rearing  + (1|Line), data=acclchecktotal, glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) # ! # cannot get convergence 6 errors
summary(globalmixmodnegbin)
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         4.37102    0.14980  29.179  < 2e-16 ***
# Regime38            0.76243    0.21102   3.613 0.000303 ***
# Rearing38          -0.03422    0.02763  -1.238 0.215617    
# Regime38:Rearing38 -0.51127    0.03475 -14.712  < 2e-16 ***

overdisp_fun(globalmixmodnegbin) # overdispersed
#      chisq       ratio         rdf           p 
# 11818.23405    91.61422   129.00000     0.00000 




#! fitting random observer effect
acclchecktotalno30.4$RID <- c(1:nrow(acclchecktotalno30.4))
 
 globalmixmod<- glmer(Adult.count20D ~ Regime*Rearing  + (1|Line) + (1|RID), data=acclchecktotalno30.4, family=poisson(link = "log"), na.action=na.exclude) 
 nointmixmod<- glmer(Adult.count20D ~ Regime + Rearing  + (1|Line) + (1|RID), data=acclchecktotalno30.4, family=poisson(link = "log"), na.action=na.exclude) 
 nullmixmod<- glmer(Adult.count20D ~ 1 + (1|Line) + (1|RID), data=acclchecktotalno30.4, family=poisson(link = "log"), na.action=na.exclude) 
 nullrandmixmod<- glm(Adult.count20D ~ Regime + Rearing, data=acclchecktotalno30.4, family=poisson(link = "log"), na.action=na.exclude)
 
 
 # Assumptions 
 par(mfrow=c(1,1))
 
 sresid<-resid(globalmixmod, type="pearson"); hist(sresid) # skewed
 fits<-fitted(globalmixmod); plot(sresid~fits) # checking for heteroscedasicity; scatter some wedge and slight trending formation/link/error family change needed

 mcp.fnc(globalmixmod)
 # plotLMER.fnc(globalmixmod)
 
 plot(sresid~acclchecktotalno30.4$Line)# no patterns no new x/interactions etc needed
 plot(sresid~acclchecktotalno30.4$Rep)
 
 ##############  MODEL SIGNIFICANCE  #####################
 
 
 # regimes behave differently; interaction significant
 drop1(globalmixmod, test = "Chi")
 # Adult.count20D ~ Regime * Rearing + (1 | Line) + (1 | RID)
 # Df    AIC     LRT Pr(Chi)
 # <none>            1490.2                
 # Regime:Rearing  1 1488.9 0.73027  0.3928
 
 # thl lines across temperatures no different
 # effect of temperatures
 drop1(nointmixmod, test = "Chi")
 # Adult.count.20D ~ Regime + Heatwave.temp + (1 | Line) + (1 | 
 #                                                               RID)
 #         Df    AIC    LRT  Pr(Chi)   
 # <none>     1488.9                   
 # Regime   1 1490.9 3.9504 0.046859 * 
 # Rearing  1 1494.4 7.5081 0.006142 **
 
 # overall model significant
 lrtest(globalmixmod, nullmixmod)
 #   #Df  LogLik Df Chisq Pr(>Chisq)   
 # 1   6 -739.09                       
 # 2   3 -745.35 -3 12.53   0.005773 **
 
 # random factor importance    
 lrtest(globalmixmod, nullrandmixmod)
 #   #Df  LogLik Df Chisq Pr(>Chisq)    
 #1   6  -739.1                        
 #2   3 -7565.7 -3 13653  < 2.2e-16 ***
 
 
 
 overdisp_fun(globalmixmod) # overdispersed
 #  chisq      ratio        rdf          p 
# 15.666265   0.127368 123.000000   1.000000 
 
 r.squaredGLMM(globalmixmod)
 #  R2m       R2c 
 # 0.119396 0.9999064
 
 ############## MODEL POST HOC  #####################
 
 
 summary(globalmixmod)
 # Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 # Family: poisson  ( log )
 # Formula: Adult.count20D ~ Regime * Rearing + (1 | Line) + (1 | RID)
 # Data: acclchecktotalno30.4
 # 
 # AIC      BIC   logLik deviance df.resid 
 # 1490.2   1507.3   -739.1   1478.2      123 
 # 
 # Scaled residuals: 
 #      Min       1Q   Median       3Q      Max 
 # -0.84215 -0.15656  0.00888  0.01832  0.05017 
 # 
 # Random effects:
 #      Groups Name        Variance Std.Dev.
 # RID    (Intercept) 6.5721   2.5636  
 # Line   (Intercept) 0.5324   0.7296  
 # Number of obs: 129, groups:  RID, 129; Line, 15
 # 
 # Fixed effects:
 #      Estimate Std. Error z value Pr(>|z|)    
 # (Intercept)          3.3827     0.5959   5.677 1.37e-08 ***
 # Regime38             0.9108     0.7716   1.180   0.2378    
 # Rearing38           -1.7960     0.7464  -2.406   0.0161 *  
 # Regime38:Rearing38   0.8236     0.9673   0.851   0.3946    
 # ---
 #      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 # 
 # Correlation of Fixed Effects:
 #      (Intr) Regm38 Rrng38
 # Regime38    -0.770              
 # Rearing38   -0.596  0.465       
 # Rgm38:Rrn38  0.464 -0.596 -0.771
 

 lsmeans(globalmixmod, pairwise~Regime*Rearing , adjust="tukey") 
 # contrast         estimate        SE df z.ratio p.value
 # 30,30 - 38,30 -0.91082006 0.7716104 NA  -1.180  0.6393
 # 30,30 - 30,38  1.79595896 0.7464491 NA   2.406  0.0759
 # 30,30 - 38,38  0.06158281 0.7882426 NA   0.078  0.9998
 # 38,30 - 30,38  2.70677901 0.7853289 NA   3.447  0.0032
 # 38,30 - 38,38  0.97240287 0.6161798 NA   1.578  0.3911
 # 30,38 - 38,38 -1.73437615 0.8009596 NA  -2.165  0.1330
 
 
 
 # 
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