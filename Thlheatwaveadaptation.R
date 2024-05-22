
## cleaning
# check what is in r console from last time
ls()
# remove everything
rm(list=ls())


library(car); library(MASS); library (lme4); library (nlme);  library(lmtest); 
library(ggplot2); library(MuMIn); library(emmeans); library(psych);
library(LMERConvenienceFunctions); library(car); library(AER); library(MASS); library(RLRsim)

temp <- expression(paste('Temperature (',degree,'C)',sep='')) #the temperature label with degrees sign

############################ ADAPTATION DATA CHECK AND ENTRY ###################################
#check what working directory is set
getwd()
#setting to required one where data is based
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")
list.files()


#reading in data 

# Simple data for non-mixed models
adaptationave <- read.csv("Thlheatwaveadaptationave.csv", header = TRUE) # full data set 
str(adaptationave)
adaptationave$Heatwave.temp<-adaptationave$Heatwave.temp
adaptationave$Regime<-as.factor(adaptationave$Regime) # changing to categorical factor as coding is not proportional to differences
adaptationave$Line<-as.factor(adaptationave$Line) # changing to categorical factor as coding is not proportional to differences
adaptationave$Heatwave.temp<-as.factor(adaptationave$Heatwave.temp) # changing to categorical factor as coding is not proportional to differences
adaptationave$no38.21 <-as.factor(adaptationave$no38.21 )
adaptationaveno38.21 <- adaptationave[adaptationave$no38.21 != "0",]

# Simple data for non-mixed models
adaptationtotal <- read.csv("Thlheatwaveadaptationfull.csv", header = TRUE) # full data set 
str(adaptationtotal)
adaptationtotal$Regime<-as.factor(adaptationtotal$Regime) # changing to categorical factor as coding is not proportional to differences
adaptationtotal$Heatwave.temp <-as.factor(adaptationtotal$Heatwave.temp ) # changing to categorical factor as coding is not proportional to differences
adaptationtotal$Line<-as.factor(adaptationtotal$Line) # changing to categorical factor as coding is not proportional to differences
adaptationtotal$Rep <-as.factor(adaptationtotal$Rep) # changing to categorical factor as coding is not proportional to differences





###############  FIGURE 2 A PLOT ##############################


xlabel <- expression(bold("5 day heatwave temperature "( degree*C)))



 


graphadapttotal<-ggplot(subset(adaptationtotal, Heatwave.temp %in% c("30","38","42")), aes(x=Heatwave.temp, y=Adult.count.20D, fill= Regime)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
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
     labs (x= expression(bold("5 day heatwave temperature "( degree*C))),
           y= "20 day offspring production") +  #adding title to the x axis and y axis
     scale_x_discrete(breaks=c("30", "38", "42"), #the order of the variables on the x axis
                      labels=c("30", "38", "42")) + # the names on the x axis
     coord_cartesian(ylim=c(-5, 422)) + #set axis limits
     scale_y_continuous(breaks=seq(0, 400, 100), #ticks from 0 to 16000 and show number every 16000
                        expand = c(0, 0)) + #cuts the axis off at 0
     theme_classic() + #the theme of the whole plot 
     annotate("text", x=c(0.85,1.15,1.85,2,2.15,2.85,3.15),
           y= c(408,408,408,416,408,408,408), 
           label= c("A","A","A","***","A","B","A"),
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
ggsave("graphadapttotal.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")





###############   FIGURE 2 B PLOT ##############################

barchartadaptation <- aggregate(adaptationtotal$Adult.count.20D, 
                                by = list(Regime = adaptationtotal$Regime, Line = adaptationtotal$Line, Temperature = adaptationtotal$Heatwave.temp),
                                FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                    n = length(x))) # aggregate data frame by 2 factors counts by temp and life stage (tapply can only do 1 factor)) then get matrix of descriptives
barchartadaptation

barchartadaptation <- do.call(data.frame, barchartadaptation) # formatting
barchartadaptation

barchartadaptation$se <- barchartadaptation$x.sd / sqrt(barchartadaptation$x.n) # SE calculation
colnames(barchartadaptation) <- c("Regime","Line", "Temperature", "mean", "sd", "n", "se") #renaming columns

barchartadaptation$upse <- barchartadaptation$mean + barchartadaptation$se # Upper CI
barchartadaptation$lowse  <- barchartadaptation$mean - barchartadaptation$se # Lower CI
barchartadaptation$ci <- barchartadaptation$se * 1.96 # CI calculation
barchartadaptation$upci <- barchartadaptation$mean + barchartadaptation$ci # Upper CI
barchartadaptation$lowci  <- barchartadaptation$mean - barchartadaptation$ci # Lower CI
barchartadaptation


## plotting data

# with solid errorbars
graphadaptpattern<-ggplot(subset(barchartadaptation, Temperature %in% c("30","38","42")), aes(x= Temperature, y=mean, group= Line, colour= Line)) +  #change fill to colour is just lines and change 'scale_fill_manual' below to scale_color_manual
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
    annotate("text", x=c(0.85,1.15,1.85,2,2.15,2.85,3.15),
           y= c(408,408,408,416,408,408,408), 
           label= c("A","A","A","***","A","B","A"),
           size= 4) +
     # facet_grid(.~ Regime) +
     labs (x= xlabel,
           y= "Mean 20 day offspring production +/- SE") + # x and y axis titles
     scale_x_discrete(breaks=c("30", "38", "42"), #the order of the variables on the x axis
                      labels=c("30", "38", "42")) +
     coord_cartesian(ylim=c(-5, 422)) + # set axis limits
     scale_y_continuous(breaks=seq(0, 400, 100), expand = c(0,0))  #ticks every 0.2

# turn of facet and strip x2 for overlap


setwd("/Users/krissales/Desktop")
ggsave("graphadaptpattern.png",width=4, height=4, dpi=300, bg = "transparent")
setwd("/Users/krissales/Documents/phd and techs/data/Neat data/thl adaptation/")




###############  TOTAL DESCRIPTIVE STATISTICS ##############################


str(adaptationtotal)
# $ ID               : Factor w/ 16 levels "L1 38.1","L1 38.2",..: 1 1 1 1 1 2 2 2 2 2 ...     male ID
# $ Line             : Factor w/ 16 levels "30.211","30.213",..: 9 9 9 9 9 10 10 10 10 10 ...  thl population. 30 = 30oC 38 = 38oC
# $ Regime           : Factor w/ 2 levels "30","38": 2 2 2 2 2 2 2 2 2 2 ...                   evlotionuary thl background
# $ Heatwave.temp    : Factor w/ 3 levels "30","38","42": 1 1 1 1 1 1 1 1 1 1 ...              heatwave exposure temp
# $ Rep              : Factor w/ 13 levels "1","2","3","4",..: 1 2 3 4 5 1 2 3 4 5 ...         
# $ Adult.count.10D.1: int  155 133 77 154 138 13 137 116 124 111 ...                          
# $ Adult.count.10D.2: int  155 67 40 143 108 11 134 136 127 72 ...                            
# $ Adult.count.20D  : int  310 200 117 297 246 24 271 252 251 183 ...                         total 20 day reprodutive fitness


# !
describeBy(adaptationtotal$Adult.count.20D, list(adaptationtotal$Regime,adaptationtotal$Heatwave.temp),mat=TRUE) 
#     item group1 group2 vars  n     mean        sd median  trimmed      mad min max range       skew   kurtosis       se
# X11    1     30     30    1 42 239.1429  90.99680  256.5 252.4412  62.2692   0 355   355 -1.1944101  0.9651776 14.04111
# X12    2     38     30    1 42 192.2619  93.72483  213.0 200.1176  84.5082   0 339   339 -0.6957860 -0.4913418 14.46206
# X13    3     30     38    1 42 228.2381  80.74767  239.0 231.5294  70.4235   0 392   392 -0.4998665  0.1003272 12.45964
# X14    4     38     38    1 45 227.1111 111.56047  252.0 238.0270  93.4038   0 362   362 -0.9581290 -0.2425429 16.63045
# X15    5     30     42    1 67 131.2687 123.76855  127.0 122.5818 188.2902   0 398   398  0.3423454 -1.3139417 15.12073
# X16    6     38     42    1 39 203.3846 107.46006  214.0 207.5758 102.2994   0 369   369 -0.5136619 -0.6612675 17.20738


# ! balanced looks ok 

describeBy(adaptationtotal$Adult.count.20D, list(adaptationtotal$Line,adaptationtotal$Heatwave.temp),mat=TRUE) 
#      item group1 group2 vars  n     mean          sd median  trimmed      mad min max range        skew   kurtosis       se
# X11     1 30.211     30    1  5 286.0000  39.9186673  265.0 286.0000  17.7912 253 348    95  0.57030300 -1.6831749 17.85217
# X12     2 30.213     30    1  5 231.6000  73.0602491  221.0 231.6000  31.1346 137 341   204  0.23568828 -1.4318545 32.67354
# X13     3 30.215     30    1  5 217.0000 122.7354879  266.0 217.0000  34.0998   0 290   290 -1.01310248 -0.9952230 54.88898
# X14     4  30.23     30    1  7 268.5714  56.5710678  275.0 268.5714  35.5824 163 338   175 -0.56851231 -0.8508290 21.38185
# X15     5  30.24     30    1  5 216.8000 112.1770921  221.0 216.8000 103.7820  32 320   288 -0.68055714 -1.3453897 50.16712
# X16     6  30.26     30    1  5 256.4000  70.6915837  260.0 256.4000  71.1648 170 355   185  0.14126686 -1.7602805 31.61424
# X17     7  30.27     30    1  5 183.8000  93.0413886  208.0 183.8000  66.7170  31 259   228 -0.69545064 -1.3975184 41.60937
# X18     8  30.29     30    1  5 241.2000 148.5486452  315.0 241.2000  48.9258   0 348   348 -0.69068931 -1.4910429 66.43297

# X19     9  38.11     30    1  5 234.0000  78.6352338  246.0 234.0000  75.6126 117 310   193 -0.39082104 -1.7558167 35.16675
# X110   10  38.12     30    1  5 196.2000 101.9053482  251.0 196.2000  29.6520  24 271   247 -0.82242867 -1.2764808 45.57346
# X111   11  38.13     30    1  5 192.0000  55.6372178  220.0 192.0000   8.8956  95 226   131 -0.95064243 -1.0895892 24.88172
# X112   12  38.14     30    1  5 207.0000  67.8638343  203.0 207.0000  97.8516 118 278   160 -0.11057157 -2.0053052 30.34963
# X113   13  38.24     30    1  7 246.7143  50.2882169  245.0 246.7143  48.9258 189 339   150  0.61842995 -1.0496416 19.00716
# X114   14  38.26     30    1  5 149.6000 122.0667850  114.0 149.6000 169.0164   0 285   285  0.03778467 -2.0681442 54.58993
# X115   15  38.27     30    1  5 187.8000  84.1290675  172.0 187.8000  74.1300 119 325   206  0.66184150 -1.4206406 37.62366
# X116   16  38.28     30    1  5 103.0000 141.7109735    0.0 103.0000   0.0000   0 277   277  0.31275253 -2.2132449 63.37507

# X117   17 30.211     38    1  5 208.6000 148.8196896  287.0 208.6000  99.3342   0 354   354 -0.35953148 -1.9641348 66.55419
# X118   18 30.213     38    1  7 221.2857  86.3147617  192.0 221.2857  41.5128 150 392   242  0.97778223 -0.6690504 32.62391
# X119   19 30.215     38    1  5 242.0000  34.8425028  239.0 242.0000  17.7912 201 296    95  0.40316039 -1.4556685 15.58204
# X120   20  30.23     38    1  5 216.2000  87.3138019  228.0 216.2000 128.9862 127 343   216  0.27718477 -1.7809533 39.04792
# X121   21  30.24     38    1  5 273.0000  26.5329983  271.0 273.0000  23.7216 243 311    68  0.25440020 -1.7809659 11.86592
# X122   22  30.26     38    1  5 183.8000  80.2352790  195.0 183.8000  97.8516  78 261   183 -0.19526449 -2.0385166 35.88231
# X123   23  30.27     38    1  5 209.4000  82.1936737  230.0 209.4000 103.7820 103 320   217  0.01852164 -1.7542014 36.75813
# X124   24  30.29     38    1  5 274.4000  41.3436815  272.0 274.4000  48.9258 236 337   101  0.41554727 -1.6901038 18.48946

# X125   25  38.11     38    1  5 219.0000 139.8141624  212.0 219.0000 179.3946  12 362   350 -0.33250618 -1.7299862 62.52679
# X126   26  38.12     38    1  5 231.8000 151.5031353  310.0 231.8000  71.1648   0 358   358 -0.53589465 -1.7402499 67.75426
# X127   27  38.13     38    1  5 230.0000 124.9859992  254.0 230.0000  84.5082  24 343   319 -0.69198361 -1.3549151 55.89544
# X128   28  38.14     38    1  5 272.8000  37.4459611  271.0 272.8000  38.5476 233 328    95  0.32957423 -1.7354482 16.74634
# X129   29  38.24     38    1 10 141.1000 123.5110881  207.5 140.5000  90.4386   0 287   287 -0.25769992 -1.9930821 39.05764
# X130   30  38.26     38    1  5 294.2000  52.5994297  307.0 294.2000  37.0650 211 350   139 -0.52589490 -1.4799517 23.52318
# X131   31  38.27     38    1  5 262.8000  60.5367657  263.0 262.8000  77.0952 168 316   148 -0.50523145 -1.5671583 27.07286
# X132   32  38.28     38    1  5 251.2000  86.3435000  289.0 251.2000  71.1648 155 337   182 -0.21076061 -2.2091591 38.61399

# X133   33 30.211     42    1 10  79.6000 107.7890945    7.5  65.2500  11.1195   0 274   274  0.67404564 -1.4056289 34.08590
# X134   34 30.213     42    1 12 130.1667  99.8615709  158.5 127.1000  63.7518   0 291   291 -0.17284430 -1.5361232 28.82755
# X135   35 30.215     42    1  5 214.8000 133.0834325  211.0 214.8000 189.7728  59 356   297 -0.01995226 -2.1392943 59.51672
# X136   36  30.23     42    1 13 175.9231 133.2431746  222.0 177.1818 127.5036   0 338   338 -0.33735242 -1.6787592 36.95501
# X137   37  30.24     42    1  8 167.6250 148.8805250  174.5 167.6250 182.3598   0 398   398  0.14521661 -1.7418762 52.63721
# X138   38  30.26     42    1  5  54.0000  66.1702350   51.0  54.0000  75.6126   0 162   162  0.65219054 -1.4032854 29.59223
# X139   39  30.27     42    1  5  34.4000  68.8425740    0.0  34.4000   0.0000   0 157   157  1.05027743 -0.9496302 30.78734
# X140   40  30.29     42    1  9 143.6667 134.6300487  131.0 143.6667 194.2206   0 312   312  0.11464634 -1.9145023 44.87668

# X141   41  38.11     42    1  5 232.8000  51.8044400  242.0 232.8000  25.2042 145 280   135 -0.79520639 -1.1986568 23.16765
# X142   42  38.12     42    1  5 183.8000  32.8892080  203.0 183.8000   5.9304 131 207    76 -0.66524639 -1.5539509 14.70850
# X143   43  38.13     42    1  5 225.8000 135.5274880  263.0 225.8000  72.6474   0 347   347 -0.73799576 -1.3065624 60.60974
# X144   44  38.14     42    1  7 284.8571  41.9897947  280.0 284.8571  44.4780 233 341   108  0.11794737 -1.9082383 15.87065
# X145   45  38.24     42    1  2 170.5000   0.7071068  170.5 170.5000   0.7413 170 171     1  0.00000000 -2.7500000  0.50000
# X146   46  38.26     42    1  5 272.4000  85.2073940  297.0 272.4000  84.5082 144 369   225 -0.37905352 -1.6234166 38.10591
# X147   47  38.27     42    1  5  45.2000  70.9309523    0.0  45.2000   0.0000   0 162   162  0.74146339 -1.4295984 31.72129
# X148   48  38.28     42    1  5 159.4000 136.5184969  119.0 159.4000 127.5036   0 366   366  0.36577970 -1.5708908 61.05293

describeBy(adaptationtotal$Adult.count.20D, list(adaptationtotal$Line,adaptationtotal$Heatwave.temp), mat=TRUE) 





############### TOTAL MIXED MODEL SELECTION ##############################

#### Poisson family error structures
# As data is a discrete and 0 bounded count poisson best

# Creating models 
#! data comes from subpopulation sturcture (lines) within regimes. Lines uniquely coded so no nesting required

globalmixmod<- glmer(Adult.count.20D ~ Regime*Heatwave.temp  + (1|Line), data=adaptationtotal, family=poisson(link = "log"),  na.action=na.exclude) 
drop1(globalmixmod, test = "Chi")
# Df   AIC    LRT   Pr(Chi)    
# <none>                  23201                     
# Regime:Heatwave.temp  2 24100 903.76 < 2.2e-16 ***
summary(globalmixmod)
# Fixed effects:
#      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               5.45573    0.05736   95.12  < 2e-16 ***
# Regime38                 -0.20196    0.08126   -2.49  0.01294 *  
# Heatwave.temp38          -0.03727    0.01432   -2.60  0.00924 ** 
# Heatwave.temp42          -0.61741    0.01468  -42.06  < 2e-16 ***
# Regime38:Heatwave.temp38  0.21295    0.02068   10.30  < 2e-16 ***
# Regime38:Heatwave.temp42  0.64353    0.02167   29.70  < 2e-16 ***

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
# 15799.96495    58.51839   270.00000     0.00000 
mean(adaptationtotal$Adult.count.20D); var(adaptationtotal$Adult.count.20D)# variance much bigger than mean 197.2996; 12365.
table(adaptationtotal$Adult.count.20D);str(adaptationtotal$Adult.count.20D);38/277 # 13% data 0s
r.squaredGLMM(globalmixmod)
# R2m       R2c 
# 0.1362588 0.1444276 



# Assumptions 
par(mfrow=c(1,1))

sresid<-resid(globalmixmod, type="pearson"); hist(sresid) # not necassary for normal residuals, but looks it
fits<-fitted(globalmixmod); plot(sresid~fits) # checking for heteroscedasicity; scatter some wedge and slight trending formation/link/error family change needed

mcp.fnc(globalmixmod)
# plotLMER.fnc(globalmixmod)

plot(sresid~adaptationtotal$Line)# no patterns no new x/interactions etc needed
plot(sresid~adaptationtotal$Rep)

globalmixmodnegbin<- glmer.nb(Adult.count.20D ~ Regime * Heatwave.temp  + (1|Regime/Line), data=adaptationtotal, glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) # ! # cannot get convergence 6 errors
drop1(globalmixmodnegbin, test = "Chi")
# Df   AIC    LRT   Pr(Chi)    
# <none>                  22970                     
# Regime:Heatwave.temp  2 23855 888.83 < 2.2e-16 ***
summary(globalmixmodnegbin)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               5.45602    0.05752   94.86  < 2e-16 ***
#      Regime38                 -0.20196    0.08148   -2.48  0.01318 *  
#      Heatwave.temp38          -0.03744    0.01447   -2.59  0.00966 ** 
#      Heatwave.temp42          -0.61821    0.01480  -41.77  < 2e-16 ***
#      Regime38:Heatwave.temp38  0.21292    0.02088   10.20  < 2e-16 ***
#      Regime38:Heatwave.temp42  0.64356    0.02186   29.44  < 2e-16 ***



#! RID random observer effect, produces perameter estimates more logical than over dispersed poisson. Cant fit negative binomial; not iteracting as too great overdispersion. Mixed model better than the lost of power and interest of the random factors. 

adaptationtotal$RID <- c(1:nrow(adaptationtotal))

globalmixmod<- glmer(Adult.count.20D ~ Regime*Heatwave.temp  + (1|Line) + (1|RID), data=adaptationtotal, family=poisson(link = "log"), na.action=na.exclude) 
nointmixmod<- glmer(Adult.count.20D ~ Regime + Heatwave.temp  + (1|Line) + (1|RID), data=adaptationtotal, family=poisson(link = "log"), na.action=na.exclude) 
nullmixmod<- glmer(Adult.count.20D ~ 1  + (1|Line) + (1|RID), data=adaptationtotal, family=poisson(link = "log"),  na.action=na.exclude) 
nullrandmixmod<- glm(Adult.count.20D ~ Regime * Heatwave.temp, data=adaptationtotal, family=poisson(link = "log"), na.action=na.exclude)

summary(globalmixmod)


############### TOTAL MIXED MODEL SIGNIFICANCE ##############################

# regimes behave differently; interaction significant
drop1(globalmixmod, test = "Chi")
# Adult.count.20D ~ Regime * Heatwave.temp + (1 | Line) + (1 | 
#                                                               RID)
# Df    AIC    LRT   Pr(Chi)    
# <none>                  3679.1                     
# Regime:Heatwave.temp  2 3689.1 14.076 0.0008778 ***

# thl lines across temperatures no different
# effect of temperatures
drop1(nointmixmod, test = "Chi")
# Adult.count.20D ~ Regime + Heatwave.temp + (1 | Line) + (1 | 
#                                                               RID)
#               Df    AIC     LRT   Pr(Chi)    
# <none>           3689.1                      
# Regime         1 3687.9  0.7464    0.3876    
# Heatwave.temp  2 3707.7 22.5996 1.238e-05 ***

# overall model significant
lrtest(globalmixmod, nullmixmod)
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   8 -1831.5                         
# 2   3 -1851.0 -5 38.845  2.552e-07 ***

# random factor importance    
lrtest(globalmixmod, nullrandmixmod)
# #Df   LogLik Df Chisq Pr(>Chisq)    
# 1   8  -1831.5                        
# 2   6 -12196.8 -2 20731  < 2.2e-16 ***



overdisp_fun(globalmixmod) # overdispersed
# chisq       ratio         rdf           p 
# 42.8585591   0.1593255 269.0000000   1.0000000 

r.squaredGLMM(globalmixmod)
# R2m       R2c 
# 0.1365718 0.9996558


############### TOTAL MIXED MODEL POST HOC ##############################

summary(globalmixmod)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: Adult.count.20D ~ Regime * Heatwave.temp + (1 | Line) + (1 |      RID)
# Data: adaptationtotal
# 
# AIC      BIC   logLik deviance df.resid 
# 3679.1   3708.1  -1831.5   3663.1      269 
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.19120  0.00052  0.00921  0.01495  0.04317 
# 
# Random effects:
#      Groups Name        Variance Std.Dev.
# RID    (Intercept) 3.45917  1.860   
# Line   (Intercept) 0.03313  0.182   
# Number of obs: 277, groups:  RID, 277; Line, 16
# 
# Fixed effects:
#                          Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               5.18174    0.29560  17.529  < 2e-16 ***
# Regime38                 -0.41317    0.41909  -0.986  0.32419    
# Heatwave.temp38           0.08467    0.40752   0.208  0.83540    
# Heatwave.temp42          -1.90140    0.37381  -5.087 3.65e-07 ***
# Regime38:Heatwave.temp38 -0.02225    0.57332  -0.039  0.96904    
# Regime38:Heatwave.temp42  1.81103    0.56180   3.224  0.00127 ** 
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#      (Intr) Regm38 Htw.38 Htw.42 R38:H.3
# Regime38    -0.705                             
# Hetwv.tmp38 -0.691  0.487                      
# Hetwv.tmp42 -0.751  0.531  0.546               
# Rgm38:Ht.38  0.491 -0.695 -0.711 -0.388        
# Rgm38:Ht.42  0.501 -0.712 -0.363 -0.664  0.517 


lsmeans(globalmixmod, pairwise~Regime*Heatwave.temp, adjust="tukey") 

# contrast         estimate        SE df z.ratio p.value
# 30,30 - 38,30  0.41317162 0.4190869 NA   0.986  0.9226 === regime comparison 30oc          -
# 30,30 - 30,38 -0.08467328 0.4075151 NA  -0.208  0.9999 = 30 thl 30-38 -
# 30,30 - 38,38  0.35075317 0.4134036 NA   0.848  0.9583
# 30,30 - 30,42  1.90140044 0.3738054 NA   5.087  <.0001 = 30 thl 30-42 ***
# 30,30 - 38,42  0.50354441 0.4279840 NA   1.177  0.8482 
# 38,30 - 30,38 -0.49784490 0.4186877 NA  -1.189  0.8423 
# 38,30 - 38,38 -0.06241845 0.4032638 NA  -0.155  1.0000 = 38 thl 30-38 - 
# 38,30 - 30,42  1.48822882 0.3861927 NA   3.854  0.0016 <- 30 thl @ 42 < 38 thl @ 30 ***
# 38,30 - 38,42  0.09037279 0.4202327 NA   0.215  0.9999 = 38 thl 30-42 -
# 30,38 - 38,38  0.43542644 0.4130026 NA   1.054  0.8993 === regime comparison 38oc         -
# 30,38 - 30,42  1.98607372 0.3736338 NA   5.316  <.0001 = 30 thl 38-42 ***
# 30,38 - 38,42  0.58821769 0.4276727 NA   1.375  0.7419
# 38,38 - 30,42  1.55064727 0.3801641 NA   4.079  0.0006 <- 30 thl @ 42 < 38 thl @ 38 ***
# 38,38 - 38,42  0.15279124 0.4154504 NA   0.368  0.9991 = 38 thl 38-42 -
# 30,42 - 38,42 -1.39785603 0.3949369 NA  -3.539  0.0054 === regime comparison 42oc         ***

# OLD CROSS CHECK
# $contrasts
# contrast         estimate         SE df z.ratio p.value
# 30,30 - 38,30  0.20195746 0.08125680 NA   2.485  0.1283 = regime comparison 30         -
# 30,30 - 30,38  0.03726526 0.01431661 NA   2.603  0.0965 = heat comparison 30 thl 30-38 .
# 30,30 - 38,38  0.02627549 0.08110592 NA   0.324  0.9995
# 30,30 - 30,42  0.61741030 0.01467895 NA  42.061  <.0001 = heat comparison 30 thl 30-42 ***
# 30,30 - 38,42  0.17583519 0.08129321 NA   2.163  0.2553
# 38,30 - 30,38 -0.16469219 0.08128242 NA  -2.026  0.3273 = heat comparison 38 thl 30-38 -
# 38,30 - 38,38 -0.17568196 0.01491620 NA -11.778  <.0001
# 38,30 - 30,42  0.41545284 0.08137063 NA   5.106  <.0001
# 38,30 - 38,42 -0.02612226 0.01593967 NA  -1.639  0.5726 = heat comparison 38 thl 30-42 -
# 30,38 - 38,38 -0.01098977 0.08113157 NA  -0.135  1.0000 = regime comparison 38         -
# 30,38 - 30,42  0.58014504 0.01486736 NA  39.021  <.0001 = heat comparison 30 thl 38-42 ***
# 30,38 - 38,42  0.13856993 0.08131885 NA   1.704  0.5290
# 38,38 - 30,42  0.59113480 0.08121999 NA   7.278  <.0001
# 38,38 - 38,42  0.14955970 0.01524232 NA   9.812  <.0001 = heat comparison 38 thl 38-42 ***
# 30,42 - 38,42 -0.44157511 0.08140686 NA  -5.424  <.0001 = regime comparison 42         ***





# 
# 
# 
# #################################### ADAPTATION SIMPLE ANALYSIS ########################
# #! Note that to avoid pseudo replication averages ave being used for this analysis 
# str(adaptationave)
# 
# summary(adaptationave) 
# # Regime  Heatwave.temp      Line    Adult.count.ave.20D    std.dev             count          std.error        no38.21      
# # 30:24   30:16         30.211 : 3   Min.   : 34.4       Min.   :  0.7071   Min.   : 2.000   Min.   : 0.50   Min.   :0.0000  
# # 38:25   38:16         30.213 : 3   1st Qu.:170.5       1st Qu.: 55.6372   1st Qu.: 5.000   1st Qu.:23.52   1st Qu.:1.0000  
# # 42:17         30.215 : 3   Median :216.2       Median : 84.1291   Median : 5.000   Median :35.17   Median :1.0000  
# # 30.23  : 3   Mean   :201.1       Mean   : 86.6357   Mean   : 5.735   Mean   :36.36   Mean   :0.9796  
# # 30.24  : 3   3rd Qu.:242.0       3rd Qu.:122.7355   3rd Qu.: 5.000   3rd Qu.:48.94   3rd Qu.:1.0000  
# # 30.26  : 3   Max.   :294.2       Max.   :151.5031   Max.   :13.000   Max.   :67.75   Max.   :1.0000  
# # (Other):31 
# 
# 

# #gives you vars  n, mean, sd,  median,  trimmed, mad, min, max, range, skew, kurtosis, se
# describeBy(adaptationave$Adult.count.ave.20D, list(adaptationave$Regime,adaptationave$Heatwave.temp),mat=TRUE) 
# # item group1 group2 vars n     mean       sd   median  trimmed      mad   min      max    range        skew   kurtosis       se
# # X11    1     30     30    1 8 237.6714 32.63563 236.4000 237.6714 29.35548 183.8 286.0000 102.2000 -0.09168413 -1.3156146 11.53844
# # X12    2     38     30    1 8 189.5393 45.80744 194.1000 189.5393 39.14064 103.0 246.7143 143.7143 -0.56100586 -0.9184688 16.19538
# # X13    3     30     38    1 8 228.5857 32.14142 218.7429 228.5857 24.75942 183.8 274.4000  90.6000  0.29749513 -1.5155913 11.36371
# # X14    4     38     38    1 8 237.8625 46.30899 241.5000 237.8625 32.46894 141.1 294.2000 153.1000 -0.84843432 -0.2880052 16.37270
# # X15    5     30     42    1 8 123.7314 62.23520 134.6515 123.7314 71.40430  34.4 209.0000 174.6000 -0.14688700 -1.7062240 22.00347
# # X16    6     38     42    1 9 190.5563 73.90491 183.8000 190.5563 64.56723  45.2 284.8571 239.6571 -0.47650328 -0.8301330 24.63497
# 
# 
# 
# describeBy(adaptationaveno38.21$Adult.count.ave.20D, list(adaptationaveno38.21$Regime,adaptationaveno38.21$Heatwave.temp),mat=TRUE) 
# #     item group1 group2 vars n     mean       sd   median  trimmed      mad
# # X11    1     30     30    1 8 237.6714 32.63563 236.4000 237.6714 29.35548
# # X12    2     38     30    1 8 189.5393 45.80744 194.1000 189.5393 39.14064
# # X13    3     30     38    1 8 228.5857 32.14142 218.7429 228.5857 24.75942
# # X14    4     38     38    1 8 237.8625 46.30899 241.5000 237.8625 32.46894
# # X15    5     30     42    1 8 123.7314 62.23520 134.6515 123.7314 71.40430
# # X16    6     38     42    1 8 196.8446 76.39036 204.8000 196.8446 59.08161
# #       min      max    range        skew   kurtosis       se
# # X11 183.8 286.0000 102.2000 -0.09168413 -1.3156146 11.53844
# # X12 103.0 246.7143 143.7143 -0.56100586 -0.9184688 16.19538
# # X13 183.8 274.4000  90.6000  0.29749513 -1.5155913 11.36371
# # X14 141.1 294.2000 153.1000 -0.84843432 -0.2880052 16.37270
# # X15  34.4 209.0000 174.6000 -0.14688700 -1.7062240 22.00347
# # X16  45.2 284.8571 239.6571 -0.66636815 -0.7011399 27.00807
# 
# 
# 
# describeBy(adaptationtotal$Adult.count.20D, list(adaptationtotal$Regime,adaptationtotal$Heatwave.temp),mat=TRUE) 
# #     item group1 group2 vars  n     mean        sd median  trimmed      mad min
# # X11    1     30     30    1 42 239.1429  90.99680  256.5 252.4412  62.2692   0
# # X12    2     38     30    1 42 192.2619  93.72483  213.0 200.1176  84.5082   0
# # X13    3     30     38    1 42 228.2381  80.74767  239.0 231.5294  70.4235   0
# # X14    4     38     38    1 45 227.1111 111.56047  252.0 238.0270  93.4038   0
# # X15    5     30     42    1 67 131.2687 123.76855  127.0 122.5818 188.2902   0
# # X16    6     38     42    1 39 203.3846 107.46006  214.0 207.5758 102.2994   0
# #     max range       skew   kurtosis       se
# # X11 355   355 -1.1944101  0.9651776 14.04111
# # X12 339   339 -0.6957860 -0.4913418 14.46206
# # X13 392   392 -0.4998665  0.1003272 12.45964
# # X14 362   362 -0.9581290 -0.2425429 16.63045
# # X15 398   398  0.3423454 -1.3139417 15.12073
# # X16 369   369 -0.5136619 -0.6612675 17.20738
# 
# 
# adaptation30 <- adaptationave[adaptationave$Heatwave.temp== "30",]
# adaptation38 <- adaptationave[adaptationave$Heatwave.temp== "38",]
# adaptation42 <- adaptationave[adaptationave$Heatwave.temp== "42",]
# 
# 
# 
# 
# par(mfrow=c(2,2)) #plotting the graphs next to get other in a 4x4 gird
# 
# #### adaptation 30
# ### in base
# # 30
# hist(adaptation30$Adult.count.ave[adaptation30$Regime == "30"], 
#      main = list("30", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 3) 
# # 38
# hist(adaptation30$Adult.count.ave[adaptation30$Regime == "38"], 
#      col = "red", density = 30, angle = 180, border = "red", 
#      main = list("38", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 3)  # keep nclass = 10, keep scales default
# 
# 
# #### adaptation 38
# ### in base
# # 30
# hist(adaptation38$Adult.count.ave[adaptation38$Regime == "30"], 
#      main = list("30", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5) 
# # 38
# hist(adaptation38$Adult.count.ave[adaptation38$Regime == "38"], 
#      col = "red", density = 30, angle = 180, border = "red", 
#      main = list("38", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5)  # keep nclass = 10, keep scales default
# 
# 
# #### adaptation 42
# ### in base
# # 30
# hist(adaptation42$Adult.count.ave[adaptation42$Regime == "30"], 
#      main = list("30", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5) 
# # 38
# hist(adaptation42$Adult.count.ave[adaptation42$Regime == "38"], 
#      col = "red", density = 30, angle = 180, border = "red", 
#      main = list("38", cex = 2), xlab = "20D offspring count", ylab ="Frequency", ylim = c(0,20),
#      nclass = 5)  # keep nclass = 10, keep scales default
# 
# 
# ## groups often platykurtotic,  small samples with averages
# 
# 
# ###### plotting differences
# # base boxplots of data distribution grouped by temperature
# boxplot(adaptation30$Adult.count.ave ~ adaptation30$Regime, ylab="20D offspring count", xlab="Regime")
# boxplot(adaptation38$Adult.count.ave ~ adaptation38$Regime, ylab="20D offspring count", xlab="Regime")
# boxplot(adaptation42$Adult.count.ave ~ adaptation42$Regime, ylab="20D offspring count", xlab="Regime")
# 
# # spread looks relatively similar
# 
# 
# 
# ########### Normality 
# ### adapation 30 - passed in sw
# shapiro.test (adaptation30$Adult.count.ave[adaptation30$Regime == "30"]) # W = 0.98405, p-value = 0.9801
# ks.test(adaptation30$Adult.count.ave[adaptation30$Regime == "30"], pnorm)  # D = 1, p-value < 2.2e-16
# shapiro.test (adaptation30$Adult.count.ave[adaptation30$Regime == "38"]) # W = 0.93597, p-value = 0.5719
# ks.test(adaptation30$Adult.count.ave[adaptation30$Regime == "38"], pnorm) # D = 1, p-value < 2.2e-16
# 
# ### adapation 38 - passed in sw
# shapiro.test (adaptation38$Adult.count.ave[adaptation38$Regime == "30"]) # W = 0.91143, p-value = 0.3642
# ks.test(adaptation38$Adult.count.ave[adaptation38$Regime == "30"], pnorm)  # D = 1, p-value < 2.2e-16
# shapiro.test (adaptation38$Adult.count.ave[adaptation38$Regime == "38"]) # W = 0.90398, p-value = 0.3136
# ks.test(adaptation38$Adult.count.ave[adaptation38$Regime == "38"], pnorm) # D = 1, p-value < 2.2e-16
# 
# ### adapation 42 - passed in sw
# shapiro.test (adaptation42$Adult.count.ave[adaptation42$Regime == "30"]) # W = 0.95179, p-value = 0.7293
# ks.test(adaptation42$Adult.count.ave[adaptation42$Regime == "30"], pnorm)  # D = 1, p-value < 2.2e-16
# shapiro.test (adaptation42$Adult.count.ave[adaptation42$Regime == "38"]) # W = 0.94995, p-value = 0.6895
# ks.test(adaptation42$Adult.count.ave[adaptation42$Regime == "38"], pnorm) # D = 1, p-value = 4.441e-16
# 
# # normality passed in sw and in sw and ks in spss 
# 
# ########### Homogeneity of Variances - Failed in all groups except 20 days 

# ### adaptation 30
# bartlett.test(adaptation30$Adult.count.ave ~ adaptation30$Regime) # Bartlett's K-squared = 0.737, df = 1, p-value = 0.3906
# fligner.test(adaptation30$Adult.count.ave ~ adaptation30$Regime) # Fligner-Killeen:med chi-squared = 0.11383, df = 1, p-value = 0.7358
# leveneTest(adaptation30$Adult.count.ave ~ adaptation30$Regime)   #Df F value Pr(>F) 2  1  0.2225 0.6444
# 
# ### adaptation 38
# bartlett.test(adaptation38$Adult.count.ave ~ adaptation38$Regime) # Bartlett's K-squared = 0.85263, df = 1, p-value = 0.3558
# fligner.test(adaptation38$Adult.count.ave ~ adaptation38$Regime) # Fligner-Killeen:med chi-squared = 0.17377, df = 1, p-value = 0.6768
# leveneTest(adaptation38$Adult.count.ave ~ adaptation38$Regime)   #Df F value Pr(>F) 1  0.3824 0.5462
# 
# ### adaptation 42
# bartlett.test(adaptation42$Adult.count.ave ~ adaptation42$Regime) # Bartlett's K-squared = 0.20411, df = 1, p-value = 0.6514
# fligner.test(adaptation42$Adult.count.ave ~ adaptation42$Regime) # Fligner-Killeen:med chi-squared = 0.25579, df = 1, p-value = 0.613
# leveneTest(adaptation42$Adult.count.ave ~ adaptation42$Regime)   #Df F value Pr(>F) 1  0.0738 0.7896
# 
# # tests passed
# 
# 
# 
# 
# par(mfrow=c(1,1))
# 
# 
# ################### SIMPLE TESTING LOOKING WITHIN HEATWAVE TEMPERATURES ################################
# 
# 
# # As the data is both  normal and not homogenous in variance in groups t-tests valid 
# 
# #### adaptation 30oc ############
# 
# 
# t.test(adaptation30$Adult.count.ave.20D ~ adaptation30$Regime, var.equal = TRUE, paired = FALSE)
# #t = 2.4205, df = 14, p-value = 0.02968
# #( check
# wilcox.test(adaptation30$Adult.count.ave.20D ~ adaptation30$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE) # W = 51, p-value = 0.04988)
# 
# adaptation30$Adult.count.ave.20D<- round(adaptation30$Adult.count.ave.20D,digits = 0)
# globalmodposs<-glm(Adult.count.ave.20D ~ Regime, poisson(link = "log"), data=adaptation30) 
# summary(globalmodposs) # AIC: 234.18
# pseudoR<-(globalmodposs$null.deviance-globalmodposs$deviance) / globalmodposs$null.deviance; pseudoR # 0.2736082
# par(mfrow=c(2,2)); plot(globalmodposs);par(mfrow=c(1,1))
# theta<-globalmodposs$deviance/globalmodposs$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 8.23868, massively overdispersed is >1 is overdispersion
# ; dispersiontest(globalmodposs) # 2.2426, p-value = 0.01246
# var(adaptation30$Adult.count.ave.20D); mean(adaptation30$Adult.count.ave.20D) # #2092.096 vs. 213.6875
# table(adaptation30$Adult.count.ave.20D)# no data 0s
# globalnvebinom<-glm.nb(Adult.count.ave.20D ~ Regime, link = "log", data=adaptation30)
# summary(globalnvebinom) # AIC: 170.19
# pseudoR<-(globalnvebinom$null.deviance-globalnvebinom$deviance) / globalnvebinom$null.deviance; pseudoR # 0.2504235
# par(mfrow=c(2,2)); plot(globalnvebinom);par(mfrow=c(1,1))
# theta<-globalnvebinom$deviance/globalnvebinom$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 8.23868, massively overdispersed is >1 is overdispersion # 1.170881
# lrtest(globalmodposs, globalnvebinom) # negative binomial better, confirming with assumptions
# #Df   LogLik Df  Chisq Pr(>Chisq)    
# # 1   2 -115.089                         
# # 2   3  -82.095  1 65.987  4.539e-16 ***
# nullmod<-glm.nb(Adult.count.ave.20D ~ 1, link = "log", data=adaptation30); summary(nullmod) # 172.88
# pseudoR<-(nullmod$null.deviance-nullmod$deviance) / nullmod$null.deviance; pseudoR # (thomas et al., 2015) # -5.237103e-15
# drop1(globalnvebinom, test = "Chi")
# # Adult.count.ave.20D ~ Regime
# # Df Deviance    AIC    LRT Pr(>Chi)  
# # <none>      16.392 168.19                  
# # Regime  1   21.869 171.67 5.4765  0.01927 *
# library(lmtest); lrtest(globalnvebinom, nullmod)
# # Model 1: Adult.count.ave.20D ~ Regime
# # Model 2: Adult.count.ave.20D ~ 1
# # #Df  LogLik Df  Chisq Pr(>Chisq)  
# # 1   3 -82.095                       
# # 2   2 -84.442 -1 4.6941    0.03027 *
# summary(globalnvebinom)
# # glm.nb(formula = Adult.count.ave.20D ~ Regime, data = adaptation30, 
# #        link = "log", init.theta = 30.69875666)
# # 
# # Deviance Residuals: 
# #      Min        1Q    Median        3Q       Max  
# # -2.80029  -0.46820   0.06755   0.50841   1.42946  
# # 
# # Coefficients:
# #                  Estimate Std. Error z value Pr(>|z|)    
# #     (Intercept)  5.47122    0.06781  80.690   <2e-16 ***
# #     Regime38    -0.22617    0.09658  -2.342   0.0192 *  
# 
# exp(5.47122 )# 237.
# exp(5.47122-0.22617) # 189.6253
# library(lsmeans); lsmeans(globalnvebinom, pairwise~Regime, adjust="tukey")
# # $contrasts
# # contrast  estimate         SE df z.ratio p.value
# # 30 - 38  0.2261713 0.09658473 NA   2.342  0.0192
# library(psych); describeBy(adaptation30$Adult.count.ave.20D, adaptation30$Regime)
# 
# 
# 
# 
# 
# #### adaptation 38oc ###################
# t.test(adaptation38$Adult.count.ave ~ adaptation38$Regime, var.equal = TRUE, paired = FALSE)
# #t = -0.46547, df = 14, p-value = 0.6488
# #( check
# wilcox.test(adaptation38$Adult.count.ave ~ adaptation38$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE) # W = 24, p-value = 0.4418)
# 
# globalnvebinom<-glm.nb(Adult.count.ave.20D ~ Regime, link = "log", data=adaptation38)
# nullmod<-glm.nb(Adult.count.ave.20D ~ 1, link = "log", data=adaptation38)
# summary(globalnvebinom)
# # Coefficients:
# #      Estimate Std. Error z value Pr(>|z|)    
# #     (Intercept)  5.43191    0.05909  91.922   <2e-16 ***
# #      Regime38     0.03978    0.08344   0.477    0.634  
# lrtest(globalnvebinom, nullmod)
# # 1   3 -81.225                     
# # 2   2 -81.338 -1 0.2257     0.6348
# drop1(globalnvebinom, test = "Chi")
# # <none>      16.205 166.45                 
# # Regime  1   16.433 164.68 0.22729   0.6335
# 
# 
# #### adaptation 42oc ###################
# t.test(adaptation42$Adult.count.ave ~ adaptation42$Regime, var.equal = TRUE, paired = FALSE)
# # t = -2.0016, df = 15, p-value = 0.06375
# #( check
# wilcox.test(adaptation42$Adult.count.ave ~ adaptation42$Regime, exact = TRUE, conf.int = TRUE, paired = FALSE)
# # W = 17, p-value = 0.07445
# 
# adaptation42$Adult.count.ave.20D<- round(adaptation42$Adult.count.ave.20D,digits = 0)
# globalmodposs<-glm(Adult.count.ave.20D ~ Regime, poisson(link = "log"), data=adaptation42) 
# summary(globalmodposs) # AIC: 234.18
# pseudoR<-(globalmodposs$null.deviance-globalmodposs$deviance) / globalmodposs$null.deviance; pseudoR # 0.1887321
# par(mfrow=c(2,2)); plot(globalmodposs);par(mfrow=c(1,1))
# theta<-globalmodposs$deviance/globalmodposs$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 34.34048, massively overdispersed is >1 is overdispersion
# library(AER); dispersiontest(globalmodposs) # 3.479, p-value = 0.0002516
# var(adaptation42$Adult.count.ave.20D); mean(adaptation42$Adult.count.ave.20D) # # 5611.735 vs. 159.1176
# table(adaptation42$Adult.count.ave.20D)# no data 0s
# library(MASS)
# globalnvebinom<-glm.nb(Adult.count.ave.20D ~ Regime, link = "log", data=adaptation42)
# summary(globalnvebinom) # AIC=198.85
# pseudoR<-(globalnvebinom$null.deviance-globalnvebinom$deviance) / globalnvebinom$null.deviance; pseudoR # 0.1532105
# par(mfrow=c(2,2)); plot(globalnvebinom);par(mfrow=c(1,1))
# theta<-globalnvebinom$deviance/globalnvebinom$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 1.182979, massively overdispersed is >1 is overdispersion # 1.170881
# lrtest(globalmodposs, globalnvebinom) # negative binomial better, confirming with assumptions
# # #Df   LogLik Df  Chisq Pr(>Chisq)    
# # 1   2 -315.007                         
# # 2   3  -96.427  1 437.16  < 2.2e-16 ***
# nullmod<-glm.nb(Adult.count.ave.20D ~ 1, link = "log", data=adaptation42); summary(nullmod) # 199.
# pseudoR<-(nullmod$null.deviance-nullmod$deviance) / nullmod$null.deviance; pseudoR # (thomas et al., 2015) # 7.176204e-15
# drop1(globalnvebinom, test = "Chi")
# #Adult.count.ave.20D ~ Regime
# # Df Deviance    AIC    LRT Pr(>Chi)  
# # <none>      17.745 196.85                  
# # Regime  1   20.955 198.06 3.2106  0.07316 .
# library(lmtest); lrtest(globalnvebinom, nullmod)
# # Model 1: Adult.count.ave.20D ~ Regime
# # Model 2: Adult.count.ave.20D ~ 1
# # #Df  LogLik Df  Chisq Pr(>Chisq)  
# # 1   3 -96.427                       
# # 2   2 -97.904 -1 2.9542    0.08565 .
# summary(globalnvebinom)
# # glm.nb(formula = Adult.count.ave.20D ~ Regime, data = adaptation42, 
# #        link = "log", init.theta = 4.279505645)
# # 
# # Deviance Residuals: 
# #      Min       1Q   Median       3Q      Max  
# # -2.3632  -0.5981   0.0347   0.6538   1.1681  
# # 
# # Coefficients:
# #                    Estimate Std. Error z value Pr(>|z|)    
# #     (Intercept)   4.8193     0.1738  27.724   <2e-16 ***
# #      Regime38      0.4301     0.2383   1.805    0.071 .  
# exp(4.8193 )# 123.8783
# exp(4.8193+0.4301) # 190.452
# library(lsmeans); lsmeans(globalnvebinom, pairwise~Regime, adjust="tukey")
# # $contrasts
# # contrast  estimate         SE df z.ratio p.value
# #  30 - 38  -0.4300875 0.2382541 NA  -1.805  0.0710
# library(psych); describeBy(adaptation42$Adult.count.ave.20D, adaptation42$Regime)
# 
# 
# 
# 
# 
# 
# 
# 
# ################## SIMPLE TESTING LOOKING WITHIN EVOLUTION TEMPERATURES #####################
# 
# #### adaptation thl 30 ############
# 
# # 30thls 
# adaptation30thl<- adaptationave[adaptationave$Regime== "30",]
# #adaptation30thl<- subset(adaptation30thl, Heatwave.temp != "40") 
# adaptation30thl$Heatwave.temp<- droplevels(adaptation30thl$Heatwave.temp)
# adaptation30thl$Regime<- droplevels(adaptation30thl$Regime)
# adaptation30thl$Line<- droplevels(adaptation30thl$Line)
# describeBy(adaptation30thl$Adult.count.ave, list(adaptation30thl$Regime,adaptation30thl$Heatwave.temp),mat=TRUE) 
# describeBy(adaptation30thl$Adult.count.ave, list(adaptation30thl$Regime,adaptation30thl$Line),mat=TRUE) 
# describeBy(adaptation30thl$Adult.count.ave, list(adaptation30thl$Regime,adaptation30thl$Line,adaptation30thl$Heatwave.temp),mat=TRUE) 
# 
# # data normal from with heatwave exploration
# boxplot(adaptation30thl$Adult.count.ave ~ adaptation30thl$Heatwave.temp, ylab="20D offspring count", xlab="Regime")
# bartlett.test(adaptation30thl$Adult.count.ave ~ adaptation30thl$Heatwave.temp) # Bartlett's K-squared = 4.0507, df = 2, p-value = 0.1319
# fligner.test(adaptation30thl$Adult.count.ave ~ adaptation30thl$Heatwave.temp) # Fligner-Killeen:med chi-squared = 4.5584, df = 2, p-value = 0.1024
# leveneTest(adaptation30thl$Adult.count.ave ~ adaptation30thl$Heatwave.temp)   #Df F value Pr(>F) 2  2.7239 0.08876 .
# # variances similar
# 
# 
# # unpaired total
# oneway.test(adaptation30thl$Adult.count.ave~ adaptation30thl$Heatwave.temp, var.equal=TRUE)
# # F = 16.116, num df = 2, denom df = 21, p-value = 5.733e-05
# 
# # 30-38 no difference
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="30"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="38"], var.equal = TRUE, paired = FALSE)
# #t = 0.56103, df = 14, p-value = 0.5836
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="30"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="38"], var.equal = TRUE, paired = T)
# #t = 0.49337, df = 7, p-value = 0.6368
# #!
# pairedtest<-subset(adaptation30thl, Heatwave.temp != "42"); pairedtest$Heatwave.temp<- droplevels(pairedtest$Heatwave.temp);str(pairedtest)
# t.test(pairedtest$Adult.count.ave[pairedtest$Heatwave.temp=="30"], pairedtest$Adult.count.ave[pairedtest$Heatwave.temp=="38"], var.equal = TRUE, paired = T)
# # t = 0.49337, df = 7, p-value = 0.6368
# 
# # 30-42 42 worse
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="30"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="42"], var.equal = TRUE, paired = FALSE)
# #t = 4.586, df = 14, p-value = 0.0004236
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="30"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="42"], var.equal = TRUE, paired = T)
# #t = 4.6354, df = 7, p-value = 0.002383
# 
# # 38-42 42 worse
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="38"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="42"], var.equal = TRUE, paired = FALSE)
# #t = 4.234, df = 14, p-value = 0.0008336
# t.test(adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="38"], adaptation30thl$Adult.count.ave[adaptation30thl$Heatwave.temp=="42"], var.equal = TRUE, paired = T)
# #t = 6.1622, df = 7, p-value = 0.000462
# 
# 
# adaptation30thl$Adult.count.ave.20D <- round(adaptation30thl$Adult.count.ave.20D,digits = 0)
# globalmodposs<-glm(Adult.count.ave.20D ~ Heatwave.temp, poisson(link = "log"), data=adaptation30thl) 
# summary(globalmodposs) # AIC: 482.01
# pseudoR<-(globalmodposs$null.deviance-globalmodposs$deviance) / globalmodposs$null.deviance; pseudoR # 0.5337787
# par(mfrow=c(2,2)); plot(globalmodposs);par(mfrow=c(1,1))
# theta<-globalmodposs$deviance/globalmodposs$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 14.64025, massively overdispersed is >1 is overdispersion
# library(AER); dispersiontest(globalmodposs) # z = 2.9331, p-value = 0.001678
# var(adaptation30thl$Adult.count.ave.20D); mean(adaptation30thl$Adult.count.ave.20D) # # 4600. vs. 196.7083
# table(adaptation30thl$Adult.count.ave.20D)# no data 0s
# library(MASS)
# globalnvebinom<-glm.nb(Adult.count.ave.20D ~ Heatwave.temp, link = "log", data=adaptation30thl)
# summary(globalnvebinom) # AIC=272.08
# pseudoR<-(globalnvebinom$null.deviance-globalnvebinom$deviance) / globalnvebinom$null.deviance; pseudoR # 0.4328159
# par(mfrow=c(2,2)); plot(globalnvebinom);par(mfrow=c(1,1))
# theta<-globalnvebinom$deviance/globalnvebinom$df.residual; theta #dispersion perameter (thomas et al 2015) how much variation left unexplained after fitting distribution # theta = 1.20383
# lrtest(globalmodposs, globalnvebinom) # negative binomial better, confirming with assumptions
# # #Df   LogLik Df  Chisq Pr(>Chisq)    
# # 1   3 -238.00                         
# # 2   4 -132.04  1 211.92  < 2.2e-16 ***
# nullmod<-glm.nb(Adult.count.ave.20D ~ 1, link = "log", data=adaptation30thl); summary(nullmod) # 199.
# pseudoR<-(nullmod$null.deviance-nullmod$deviance) / nullmod$null.deviance; pseudoR # (thomas et al., 2015) # 2.490185e-14
# drop1(globalnvebinom, test = "Chi")
# # Adult.count.ave.20D ~ Heatwave.temp
# #               Df Deviance    AIC    LRT Pr(>Chi)    
# # <none>             25.280 270.08                    
# # Heatwave.temp  2   44.572 285.38 19.291 6.47e-05 ***
# library(lmtest); lrtest(globalnvebinom, nullmod)
# # Model 1: Adult.count.ave.20D ~ Heatwave.temp
# # Model 2: Adult.count.ave.20D ~ 1
# # #Df  LogLik Df  Chisq Pr(>Chisq)    
# # 1   4 -132.04                         
# # 2   2 -139.09 -2 14.098  0.0008684 ***
# summary(globalnvebinom)
# # glm.nb(formula = Adult.count.ave.20D ~ Heatwave.temp, data = adaptation30thl, 
# #        link = "log", init.theta = 10.36988723)
# # 
# # Deviance Residuals: 
# #      Min       1Q   Median       3Q      Max  
# # -3.2134  -0.2834  -0.0170   0.5035   1.7825  
# # 
# # Coefficients:
# #      Estimate Std. Error z value Pr(>|z|)    
# # (Intercept)      5.47122    0.11216  48.780  < 2e-16 ***
# # Heatwave.temp38 -0.03968    0.15869  -0.250    0.803    
# # Heatwave.temp42 -0.65195    0.16013  -4.071 4.68e-05 ***  
# exp(5.47122)# 237.7501
# exp(5.47122-0.03968) # 228.5009
# exp(5.47122-0.65195) # 123.8746
# library(lsmeans); lsmeans(globalnvebinom, pairwise~Heatwave.temp, adjust="tukey")
# # $contrasts
# # contrast   estimate        SE df z.ratio p.value
# # 30 - 38  0.03968349 0.1586855 NA   0.250  0.9661
# # 30 - 42  0.65194671 0.1601347 NA   4.071  0.0001
# # 38 - 42  0.61226322 0.1602011 NA   3.822  0.0004
# library(psych); describeBy(adaptation30thl$Adult.count.ave.20D, adaptation30thl$Heatwave.temp)
# 
# 
# 
# #### adaptation thl 38 ############
# 
# adaptation38thl<- adaptationave[adaptationave$Regime== "38",]
# adaptation38thl<- subset(adaptation38thl, Line != "38.21") 
# adaptation38thl$Heatwave.temp<- droplevels(adaptation38thl$Heatwave.temp)
# adaptation38thl$Regime<- droplevels(adaptation38thl$Regime)
# adaptation38thl$Line<- droplevels(adaptation38thl$Line)
# describeBy(adaptation38thl$Adult.count.ave, list(adaptation38thl$Regime,adaptation38thl$Heatwave.temp),mat=TRUE) 
# describeBy(adaptation38thl$Adult.count.ave, list(adaptation38thl$Regime,adaptation38thl$Line),mat=TRUE) 
# describeBy(adaptation38thl$Adult.count.ave, list(adaptation38thl$Regime,adaptation38thl$Line,adaptation38thl$Heatwave.temp),mat=TRUE) 
# 
# # unpaired
# oneway.test(adaptation38thl$Adult.count.ave~ adaptation38thl$Heatwave.temp, var.equal=TRUE)
# # F = 1.6157, num df = 2, denom df = 21, p-value = 0.2225
# 
# # 30-38 no difference
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="30"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="38"], var.equal = TRUE, paired = FALSE)
# #t = -2.0983, df = 14, p-value = 0.0545
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="30"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="38"], var.equal = TRUE, paired = T)
# #t = -1.6465, df = 7, p-value = 0.1437
# 
# # 30-42 no difference
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="30"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="42"], var.equal = TRUE, paired = FALSE)
# #t = -0.23198, df = 14, p-value = 0.8199
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="30"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="42"], var.equal = TRUE, paired = T)
# #t = -0.24128, df = 7, p-value = 0.8163
# 
# # 38-42 no difference
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="38"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="42"], var.equal = TRUE, paired = FALSE)
# #t = 1.2987, df = 14, p-value = 0.215
# t.test(adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="38"], adaptation38thl$Adult.count.ave[adaptation38thl$Heatwave.temp=="42"], var.equal = TRUE, paired = T)
# #t = 1.4263, df = 7, p-value = 0.1968
# 
# 
