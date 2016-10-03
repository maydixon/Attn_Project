#analysis for attention project
#T1termining initial interest
#if trend is weird could look at only the first presentation for each stimulus type. particularly t and ts 
#to do that could sort by treatment and then by trial number by hand, just take first one for dataset

ERRORS FROM GITHUB: (COMMAND LINE)
Marjories-gift:Attn_project_github maydixon$ git pull origin master
From https://github.com/maydixon/Attn_Project
* branch            master     -> FETCH_HEAD
fatal: refusing to merge unrelated histories
Marjories-gift:Attn_project_github maydixon$ 
      
# I think my problems are solved?      
#rename dataset here
attn<- attention_Rcopy
#verify
names(attn)
#subsetting the data. Ultimately I want only want the columns containing the number of twitches per set, and the iT1ntifying columns, and just the first call per bat
#pulling just the first entry for every trial, to get the set level values

attnsub1<- attn[attn$call_num == 1.1, ]
attnsub1
str(attnsub1)
#making vectors of all of my treatments
vT1_t_T1 <- attnsub1[attnsub1$trial_name == "T1_t_T1",attnsub1$md_tw__call]
vT1_t_T1
# ok making a box plot of the inital # of large twitches 
boxplot(attnsub1$md_tw_call[attnsub1trial_name=="t_T1_t",])
attnsub1
----
      #attention for initial interest subset
      #gonna make a box plot of initial interest
      #file has only responses to the first set
      #maybe gotta throw out some batz
      #renaming for easy use
      attn_int <- attn_for_initial_interest #baseline database

#just checking out the values
unique (attn_int$primary_stim)

#making a subset referring to only De
SelT1 <- attn_int$primary_stim == "De"
T1_subset <- attn_int[SelDe, ]
De_subset
#making subset for Ra
SelRa <- attn_int$primary_stim == "Ra"
Ra_subset <- attn_int[SelRa, ]

#for Rra
SelRra <- attn_int$primary_stim == "Rra"
Rra_subset <- attn_int[SelRra, ]

#for Rt
SelRt <- attn_int$primary_stim == "Rt"
Rt_subset <- attn_int[SelRt, ]


#for T
SelT <- attn_int$primary_stim == "T"
T_subset <- attn_int[SelT, ]

#for Tc
SelTc <- attn_int$primary_stim == "Tc"
Tc_subset <- attn_int[SelTc, ]


#for T1
SelT1 <- attn_int$primary_stim == "T1"
T1_subset <- attn_int[SelT1, ]

#for T2
SelT2 <- attn_int$primary_stim == "T2"
T2_subset <- attn_int[SelT2, ]

#for T3
SelT3 <- attn_int$primary_stim == "T3"
T3_subset <- attn_int[SelT3, ]

#for T4
SelT4 <- attn_int$primary_stim == "T4"
T4_subset <- attn_int[SelT4, ]

#for Tc
SelTc <- attn_int$primary_stim == "Tc"
Tc_subset <- attn_int[SelTc, ]

#for Ts
SelTs <- attn_int$primary_stim == "Ts"
Ts_subset <- attn_int[SelTs, ]

# boxplot for all twitches in first set

boxplot(De_subset$all_tw_set, Ra_subset$all_tw_set, Rra_subset$all_tw_set, Rt_subset$all_tw_set, T_subset$all_tw_set, Ts_subset$all_tw_set, Tc_subset$all_tw_set, T1_subset$all_tw_set, T2_subset$all_tw_set, T3_subset$all_tw_set, T4_subset$all_tw_set, ylab="all twitches first set", names = c("De", "Ra", "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ) )

# all large twitches first set
boxplot(De_subset$lg_tw_set, Ra_subset$lg_tw_set, Rra_subset$lg_tw_set, Rt_subset$lg_tw_set, T_subset$lg_tw_set, Ts_subset$lg_tw_set, Tc_subset$lg_tw_set, T1_subset$lg_tw_set, T2_subset$lg_tw_set, T3_subset$lg_tw_set, T4_subset$lg_tw_set, ylab="large twitches first set", names = c("De", "Ra", "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ) )

#boxplot for all twitches in a trial

boxplot(De_subset$all_tw_trial, Ra_subset$all_tw_trial, Rra_subset$all_tw_trial, Rt_subset$all_tw_trial, T_subset$all_tw_trial, Ts_subset$all_tw_trial, Tc_subset$all_tw_trial, T1_subset$all_tw_trial, T2_subset$all_tw_trial, T3_subset$all_tw_trial, T4_subset$all_tw_trial, ylab = "all twitches in trial", names = c("De", "Ra",  "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ))

#boxplot for all large twitches in trial

boxplot(De_subset$lg_tw_trial, Ra_subset$lg_tw_trial, Rra_subset$lg_tw_trial, Rt_subset$lg_tw_trial, T_subset$lg_tw_trial, Ts_subset$lg_tw_trial, Tc_subset$lg_tw_trial, T1_subset$lg_tw_trial, T2_subset$lg_tw_trial, T3_subset$lg_tw_trial, T4_subset$lg_tw_trial, ylab = "all large twitches in trial",names = c("De", "Ra",  "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ))

####################
#ok trying to make a subset of the data without the dud bats.calling it attn_nob1_nob16 and removing "Blackbeard"/ Bat16 and "Wramplemeier / Bat1################# MAKE SURE TO SELECT WHOLE SECTION WHEN RUNNING BC OBJECTS HAVE SAME NAMES 
#Sel2 refers to databse with no duds (blackbeard and wramplemeier)
#subset2 refers to all subsets with no dud bats
#refers now to attn_nob1_nob16, not attn_int
#removing bat1 and bat16
attn_int

attn_nob1_nob16 <- subset(attn_int, attn_int$bat_name != "Blackbeard" & attn_int$bat_name != "Wramplemeier") 


View (attn_nob1_nob16)


#making a combined medium and large twitches per set
mdlg_tw_set <- attn_nob1_nob16$lg_tw_set + attn_nob1_nob16$md_tw_set

#making a combined medium and large twitches per trial
mdlg_tw_trial <- attn_nob1_nob16$lg_tw_trial + attn_nob1_nob16$md_tw_trial

#addings these to attn_nob1_nob16
attn_nob1_nob16 <- cbind(attn_nob1_nob16, mdlg_tw_trial, mdlg_tw_set)
View(attn_nob1_nob16)

#remaking subsets
#making a subset2 referring to only De

Sel2De <- attn_nob1_nob16$primary_stim == "De"
De_subset2 <- attn_nob1_nob16[Sel2T1, ]
De_subset2
#making subset2 for Ra
Sel2Ra <- attn_nob1_nob16$primary_stim == "Ra"
Ra_subset2 <- attn_nob1_nob16[Sel2Ra, ]

#for Rra
Sel2Rra <- attn_nob1_nob16$primary_stim == "Rra"
Rra_subset2 <- attn_nob1_nob16[Sel2Rra, ]

#for Rt
Sel2Rt <- attn_nob1_nob16$primary_stim == "Rt"
Rt_subset2 <- attn_nob1_nob16[Sel2Rt, ]


#for T
Sel2T <- attn_nob1_nob16$primary_stim == "T"
T_subset2 <- attn_nob1_nob16[Sel2T, ]

#for Tc
Sel2Tc <- attn_nob1_nob16$primary_stim == "Tc"
Tc_subset2 <- attn_nob1_nob16[Sel2Tc, ]


#for T1
Sel2T1 <- attn_nob1_nob16$primary_stim == "T1"
T1_subset2 <- attn_nob1_nob16[Sel2T1, ]

#for T2
Sel2T2 <- attn_nob1_nob16$primary_stim == "T2"
T2_subset2 <- attn_nob1_nob16[Sel2T2, ]

#for T3
Sel2T3 <- attn_nob1_nob16$primary_stim == "T3"
T3_subset2 <- attn_nob1_nob16[Sel2T3, ]

#for T4
Sel2T4 <- attn_nob1_nob16$primary_stim == "T4"
T4_subset2 <- attn_nob1_nob16[Sel2T4, ]

#for Tc
Sel2Tc <- attn_nob1_nob16$primary_stim == "Tc"
Tc_subset2 <- attn_nob1_nob16[Sel2Tc, ]

#for Ts
Sel2Ts <- attn_nob1_nob16$primary_stim == "Ts"
Ts_subset2 <- attn_nob1_nob16[Sel2Ts, ]

# boxplot for all twitches in first set

boxplot(De_subset2$all_tw_set, Ra_subset2$all_tw_set, Rra_subset2$all_tw_set, Rt_subset2$all_tw_set, T_subset2$all_tw_set, Ts_subset2$all_tw_set, Tc_subset2$all_tw_set, T1_subset2$all_tw_set, T2_subset2$all_tw_set, T3_subset2$all_tw_set, T4_subset2$all_tw_set, ylab="all twitches first set, no duds", names = c("De", "Ra", "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ) )

# all large twitches first set
boxplot(De_subset2$lg_tw_set, Ra_subset2$lg_tw_set, Rra_subset2$lg_tw_set, Rt_subset2$lg_tw_set, T_subset2$lg_tw_set, Ts_subset2$lg_tw_set, Tc_subset2$lg_tw_set, T1_subset2$lg_tw_set, T2_subset2$lg_tw_set, T3_subset2$lg_tw_set, T4_subset2$lg_tw_set, ylab="large twitches first set no duds", names = c("De", "Ra", "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ) )

#boxplot for all twitches in a trial

boxplot(De_subset2$all_tw_trial, Ra_subset2$all_tw_trial, Rra_subset2$all_tw_trial, Rt_subset2$all_tw_trial, T_subset2$all_tw_trial, Ts_subset2$all_tw_trial, Tc_subset2$all_tw_trial, T1_subset2$all_tw_trial, T2_subset2$all_tw_trial, T3_subset2$all_tw_trial, T4_subset2$all_tw_trial, ylab = "all twitches in trial, no duds", names = c("De", "Ra",  "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ))

#boxplot for all large twitches in trial

boxplot(De_subset2$lg_tw_trial, Ra_subset2$lg_tw_trial, Rra_subset2$lg_tw_trial, Rt_subset2$lg_tw_trial, T_subset2$lg_tw_trial, Ts_subset2$lg_tw_trial, Tc_subset2$lg_tw_trial, T1_subset2$lg_tw_trial, T2_subset2$lg_tw_trial, T3_subset2$lg_tw_trial, T4_subset2$lg_tw_trial, ylab = "all large twitches in trial, no duds",names = c("De", "Ra",  "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ))

#boxplot for medium and large twitches in trial
### log transformed for funzies

boxplot(log(De_subset2$mdlg_tw_trial), log (Ra_subset2$mdlg_tw_trial), log(Rra_subset2$mdlg_tw_trial), log(Rt_subset2$mdlg_tw_trial), log(T_subset2$mdlg_tw_trial), log(Ts_subset2$mdlg_tw_trial), log(Tc_subset2$mdlg_tw_trial), log(T1_subset2$mdlg_tw_trial), log(T2_subset2$mdlg_tw_trial), log(T3_subset2$mdlg_tw_trial), log(T4_subset2$mdlg_tw_trial), ylab = "all med and large twitches in trial, no duds",names = c("De", "Ra",  "Rra", "Rt",  "T" , "Ts" , "Tc",  "T1" , "T2" , "T3" , "T4" ))

#there are 13 bats now:
unique(attn_nob1_nob16$Bat_ID)

#goals: 
#try to see/think which data are best for measuring initial interest
#do anovas and post hocs

#playing with transformations
qqnorm(attn_nob1_nob16$all_tw_set)
hist(attn_nob1_nob16$all_tw_set)
hist(transformdata <-log(attn_nob1_nob16$all_tw_set)+1 )
#running anovas
#everything logged
#all twitches set
summary(aov((log_all_tw_set<-log(attn_nob1_nob16$all_tw_set+1)) ~ attn_nob1_nob16$primary_stim))
#large twitches set
summary(aov((log(attn_nob1_nob16$lg_tw_set+1)) ~ attn_nob1_nob16$primary_stim))

#mdlg twitches set
summary(aov((log(mdlg_tw_set+1)) ~ attn_nob1_nob16$primary_stim))
# mdlg_tw_set <- attn_nob1_nob16$lg_tw_set + attn_nob1_nob16$md_tw_set
#all twitches trial
summary(aov((Logtwitch<-log(attn_nob1_nob16$all_tw_trial+1)) ~ attn_nob1_nob16$primary_stim))

#mdlg twitches trial
summary(mdlg_aov<-aov((Logtwitch2 <-log(mdlg_tw_trial + 1)) ~ attn_nob1_nob16$primary_stim))
hist(Logtwitch2)

posthoc <- TukeyHSD(mdlg_aov,ordered=FALSE, conf.level=0.95)
posthoc

#log lg twitches trial
summary(aov((Logtwitch3<-log(attn_nob1_nob16$lg_tw_trial+1)) ~ attn_nob1_nob16$primary_stim))
hist(Logtwitch3)

## I think this isn't working bc I'm making too many comparisons
## Would probably make a lot of sense to remove all the t1s, this is pointless without making the baseline data a little clearer (remove or consolidate T1-4, figure out how I want to refer to first T), start there and then move on! 
#all twitches set not logged
summary(aov_all_tw_set <- aov((attn_nob1_nob16$all_tw_set) ~ attn_nob1_nob16$primary_stim))
posthoc1 <-TukeyHSD(aov_all_tw_set, ordered=FALSE, conf.level=0.95)
posthoc1
#large twitches set
summary(aov((attn_nob1_nob16$lg_tw_set+1 ~ attn_nob1_nob16$primary_stim))
        
        
        
        #variables here: independent variable: trials (categorical=) response variable: (continuous) (would be ordinal if recoded) 
        #recode to find responses to first tungara? 
        #see if there is a correlation between twitches in the first set and twitches in the first trial
        #should I recode so that only the first tungara set is included? 
        #(probably)
        #figure out how to automate rescoring/ look up and see if loops are the way
        
        #plot of the correlation between twitches in the first set and twitches in the rest of the trial: 
        twitchplot<- plot(attn_nob1_nob16$all_tw_trial,attn_nob1_nob16$all_tw_set)
        linearregressiontwitch<-lm(attn_nob1_nob16$all_tw_set ~ attn_nob1_nob16$all_tw_trial)
        abline(linearregressiontwitch) #adds trend line
        #plot of the relationship removing the first set
        #vector of all the twitches in first trial minus the first set
        all_tw_no_set1 <- c(attn_nob1_nob16$all_tw_trial - attn_nob1_nob16$all_tw_set)
        
        twitchplot2<- plot(all_tw_no_set1,attn_nob1_nob16$all_tw_set)
        linearregressiontwitch2<-lm(all_tw_no_set1 ~ attn_nob1_nob16$all_tw_trial)
        linearregressiontwitch2
        abline(linearregressiontwitch) #adds trend line
        #so more twitches in the first set means more twitches in the rest of the trial 
        
        
        
        ######################
        
        
        
        
        #seeing whether there is any trend in twitches across trials (significantly negative regression) 
        
        # open file
        attention_Rcopy_set1thru5 <- read.delim("~/Dropbox/Attention Project/R/attention_Rcopy_set1thru5.txt")
        #rename file
        attn_habit<- attention_Rcopy_set1thru5
        
        #remove the dud bats 
        
        attn_habit_1<- subset(attn_habit, attn_habit$bat_name != "Blackbeard" & attn_habit$bat_name != "Wramplemeier") 
        
        # subset by call 1-5 #don't have to bc just edited in excel
        
        # subset by treatment
        
        ## making the full loop. Will make a plot of each distribution (the #twitches/the call number, send it to a folder, then print summary stats for the linear model)
        # open file
        attention_Rcopy_set1thru5_condensed_t <- read.delim("~/Dropbox/Attention Project/R/attention_Rcopy_set1thru5_individuals_condensed_t.txt", header=TRUE)
        
        #read.delim("~/Dropbox/Attention Project/R/attention_Rcopy_set1thru5.txt") #commented out
        
        attn_habit<- attention_Rcopy_set1thru5_condensed_t
        #remove the dud bats 
        
        attn_habit_a<- subset(attn_habit, attn_habit$bat_name != "Blackbeard" & attn_habit$bat_name != "Wramplemeier") #removes unresponsive bats
        attn_habit_b<-subset(attn_habit_a, attn_habit_a$trial_name != "t_rt_t" & attn_habit_a$trial_name != "rt_t_rt") 
        attn_habit_1<-subset(attn_habit_b, attn_habit_b$trial_name_2 != "t_rt_t" & attn_habit_b$trial_name_2 != "rt_t_rt") #removes variables only tested on one bat#removes variables only tested on one bat
        
        
        # remember to set working directory wherever I want to save the plots
        setwd("~/Dropbox/Attention Project/R/Sessions/09.06.16/plots")
        
        
        #full loop:(see p119 on abobe reader for a beginners guide to R for directions on constructing loop)
        
        All_trmts <- unique(attn_habit_1$trial_name_2) #will move through my treatments and refer to them in turn
        for (i in 1:11) {  #there are 11 (21 when individual trials are seperated by frog individual) levels right now
              
              trmt.i <- All_trmts [i] # "treatment", generalizing pulled treatment
              attn_habit_i <- attn_habit_1[attn_habit_1$trial_name_2==trmt.i,] #pulling treatment data (if not combining individuals, remove 2 from row pull)
              Myfilename <- paste(trmt.i, ".pdf", sep="") #makes a pdf for the subsequent plot (can make jpg if betta)
              pdf(file = Myfilename)
              plot_i <- plot(attn_habit_i$call_num, attn_habit_i$all_tw_call, xlab="call number", ylab="number of twitches in response to call", main=trmt.i)
              
              lm_i <- lm(attn_habit_i$all_tw_call ~ attn_habit_i$call_num  ) #makes a linear model
              trend_i<- abline(lm_i) #Adds trendline to plot
              dev.off()
              summary(lm_i) # gives you summary stats
              print(trmt.i) #just so its easier to tell which summary refers to what treatment in output 
              
              
              #print(x)
              print(summary(lm_i))
        }
        
        
        
        # Are the slopes significantly different?
        #2nd idea for analysis
        
        #********  this sort of interaction is the best way***********
        lm_int<- lm(attn_habit_1$all_tw_call ~ attn_habit_1$call_num*attn_habit_1$trial_name_2) #in this lm, call number and trial are treated as interactions, and there is also a main effect (need * operator, not :)
        summary(lm_int)
        #^^^^^^^^^^^this is the best way^^^^^^^^^^^^##BUT I NEED TO ADD BAT AS A RANDOM EFFECT
        # 9/14/16 I am now going to use a mixed effect model, the one recommended to me by Sally Amen, super useful stats consultant
        #this f(x) wants me to have the random effect be a list
        #  as.list(attn_habit_1$Bat_ID)
        # class(attn_habit_1$Bat_ID)
        
        
        #can start here: 
        
        nlme_int<- lme(attn_habit_1$all_tw_call ~ attn_habit_1$call_num*attn_habit_1$trial_name_2, random = ~1|attn_habit_1$Bat_ID)
        #use following write.table only if I have changes the original data somehow
        # write.table(attn_habit_1, "/Users/maydixon/Dropbox/Attention Project/R/attn_habit_1.txt", sep="\t") 
        # write.table(attn_habit_1, "/Users/maydixon/Dropbox/Attention Project/R/attn.project.rproject/attn_habit_1.txt", sep="\t") 
        #use local or web souce for data:
        
        #local
       # attn_habit_3<-read.table("/Users/maydixon/Dropbox/Attention Project/R/attn_habit_1.txt", header=TRUE, sep="\t") #reads in table in palatable format
        #web
        attn_habit_3<-read.table("https://raw.githubusercontent.com/maydixon/Attn_Project/master/attn_habit_1.txt", sep="\t", header=TRUE)
        View(attn_habit_3)
        #glm for all twitches with an interaction between call number and trial name as main effect, bat ID as random effect, for all twitches
        library(nlme)
        nlme_int <- lme(all_tw_call ~ call_num*trial_name_2, random = ~1|Bat_ID, data= attn_habit_3) # does glm
        nlme_int
        sum_lme<- summary(nlme_int)
        sum_lme
        
        #glm for medium and large twitches with  an interaction between call number and trial name as main effect, bat ID as random effect
        nlme_int2 <- lme(mdlg_tw_call ~ call_num*trial_name_2, random = ~1|Bat_ID, data= attn_habit_3) # does glm
        sum_lme2<- summary(nlme_int)
        sum_lme2
        
        # can I explain any more of the variation by including trial number as a random variable? or a fixed for that matter...
        # glm for all twitches with  an interaction between call number and trial name as main effect, bat ID *and trial number* as random effect
        # work on this, right now the way I am calling random isn't right 
        nlme_int3 <- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3) # does glm
        sum_lme3<- summary(nlme_int3)
        sum_lme3
        write.csv( summary(nlme_int), file="/Users/maydixon/Dropbox/Attention Project/R/nlme_output.csv" )
        #######################
        #glm for mdlg twitches with an interaction between call number and trial as main effect, bat ID and trial number as random effects
        nlme_int4 <- lme(mdlg_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1, trial_number= ~1), data= attn_habit_3) # does glm
        sum_lme4<- summary(nlme_int4)
        sum_lme4
        
        nlme_int5 <- lme(mdlg_tw_call ~ call_num*trial_name_2 + trial_number, random = list(Bat_ID=~1, trial_number= ~1), data= attn_habit_3) # does glm
        sum_lme5<- summary(nlme_int5)
        sum_lme5
        
        
        #####    RETURN TO THIS ### 
        #I think that right now I have all fixed slopes, so I'm assuming that everything has the same slope. This can lead to an excess of type I errors, and is distinctly not what I want. http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf recommends, based on an ecology paper and some others: (Schielzeth & Forstmeier, 2009) That people be "maximal" about this part of model design, and to make random slopes and not just intercepts on all factors that warrent it. So...
        #Which of my variables deserve random slope? 
        # yes: call_num, trial_name_2
        # no: Bat_ID (MAYBE)
        # maybe: trial_number
        # want to say call_num and trial_name_2 may both respond differently per bat. This makes a pretty intensely maximal model, though....
        
        #start with just Bat_ID (maybe less justified, bc trial_number explains more variation)
        
        #the random intecept should be implicit (don't need random=~1+y|id)
        nlme_int5 <- lme(all_tw_call ~ call_num*trial_name_2, random = ~ trial_name_2 + call_num|Bat_ID , data= attn_habit_3) # does glm
        sum_lme5<- summary(nlme_int5)
        sum_lme5
        
        ##### gotta figure out how to call this. for now I am going to just try to work on how to compare slopes between calls with relevel
        
        #slope represents the change from prev level to next level, alphabetically. May I relevel first to call number? #why is call number alone positive?
        
        nlme_int3 <- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3) #later should specify this is de ref
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"ra_t_ra") #MAKE RA REFERENCE
        nlme_ra_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_ra_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"rra_rt_rra") #MAKE RRA REFERENCE
        nlme_rra_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_rra_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"rt_rra_rt") #MAKE RT REFERENCE
        nlme_rt_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_rt_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"t_de_t") #MAKE t_de REFERENCE
        nlme_t_de_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_t_de_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"t_ra_t") #MAKE t_ra REFERENCE
        nlme_t_ra_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_t_ra_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"tc_ts_tc") #MAKE TC REFERENCE
        nlme_tc_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_tc_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"tia_tib_tia") #MAKE TIA REFERENCE
        nlme_tia_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_tia_ref)
        
        attn_habit_3$trial_name_2 <- relevel(attn_habit_3$trial_name_2,"ts_tc_ts") #MAKE ts REFERENCE
        nlme_ts_ref<- lme(all_tw_call ~ call_num*trial_name_2, random = list(Bat_ID=~1 , trial_number= ~1), data= attn_habit_3)
        summary(nlme_ts_ref)
        
        
        attn_habit_3 <- with(ChickWeight_new, relevel(Diet, ref = "2"))
        mod2 <- lme(weight ~ Diet * Time_factor, random = ~ 1|Chick, data = ChickWeight_new)
        
        
        
        
        #trying to figure out what changes when I change the order of things I enter. What is my reference level/ intercept? Call_number? 
        nlme_int7 <- lme(all_tw_call ~ trial_name_2*call_num, random = list(Bat_ID=~1, trial_number= ~1), data= attn_habit_3) # does glm
        sum_lme7<- summary(nlme_int7)
        sum_lme7
        
        
        
        #playing with other package, lme4
        lmer_int<- lmer(all_tw_call ~ call_num*trial_name_2 + (1|Bat_ID), data=attn_habit_3)
        summary(lmer_int, correlation=TRUE)
        print (summary(lmer_int), correlation=TRUE)
        lmer_int2 <- lmer(all_tw_call ~ call_num*trial_name_2 + (1+trial_name_2|Bat_ID), data=attn_habit_3)
        summary(lmer_int2)
        # What to make alpha?   
        # #maybe too many contrasts
        # #a less parameterized model(with no intercept or grand mean, is):
        #  lm_int2<- lm(attn_habit_1$all_tw_call ~ -1 + attn_habit_1$call_num + attn_habit_1$trial_name_2)
        #  lm_int2
        #  summary(lm_int2)
        #  #everything highly significant
        #  #compare to 
        #  lm_int3<- lm(attn_habit_1$all_tw_call ~ attn_habit_1$call_num+ attn_habit_1$trial_name_2)
        #  lm_int3
        #  summary(lm_int3)
        #1st idea for analysis           
        #1 get lm() of each variable
        #####
        #main goal in this section is to run an anovaon the different linear regressions to see if any of the slopes are different
        #have to make all the lm's explicit strings, though, first
        #this is just a "quick" way to generate a bunch of lm's:
        
        All_trmts <- unique(attn_habit_1$trial_name_2) #will move through my treatments and refer to them in turn
        
        for (i in 1:11) {  #there are 11  levels right now
              
              trmt.i <- All_trmts [i]
              x<-paste('attn_habit_', trmt.i, '<- attn_habit_1[attn_habit_1$trial_name_2=="', trmt.i , '",]', '      ', 'lm_', trmt.i, '<- lm(attn_habit_', trmt.i, '$all_tw_call ~ attn_habit_', trmt.i, '$call_num', ')' , 'lm_', trmt.i , sep='')
              
              print(x)
        }
        
        #
        #output turned into runable code 
        
        attn_habit_de_t_de<- attn_habit_1[attn_habit_1$trial_name_2=="de_t_de",]
        lm_de_t_de<- lm(attn_habit_de_t_de$all_tw_call ~ attn_habit_de_t_de$call_num)
        lm_de_t_de
        
        attn_habit_ra_t_ra<- attn_habit_1[attn_habit_1$trial_name_2=="ra_t_ra",]
        lm_ra_t_ra<- lm(attn_habit_ra_t_ra$all_tw_call ~ attn_habit_ra_t_ra$call_num)
        lm_ra_t_ra
        
        attn_habit_rra_rt_rra<- attn_habit_1[attn_habit_1$trial_name_2=="rra_rt_rra",]     
        lm_rra_rt_rra<- lm(attn_habit_rra_rt_rra$all_tw_call ~ attn_habit_rra_rt_rra$call_num)
        lm_rra_rt_rra
        
        attn_habit_rt_rra_rt<- attn_habit_1[attn_habit_1$trial_name_2=="rt_rra_rt",] 
        lm_rt_rra_rt<- lm(attn_habit_rt_rra_rt$all_tw_call ~ attn_habit_rt_rra_rt$call_num)
        lm_rt_rra_rt
        
        
        attn_habit_t_de_t<- attn_habit_1[attn_habit_1$trial_name_2=="t_de_t",] 
        lm_t_de_t<- lm(attn_habit_t_de_t$all_tw_call ~ attn_habit_t_de_t$call_num)
        lm_t_de_t
        
        
        attn_habit_t_ra_t<- attn_habit_1[attn_habit_1$trial_name_2=="t_ra_t",] 
        lm_t_ra_t<- lm(attn_habit_t_ra_t$all_tw_call ~ attn_habit_t_ra_t$call_num)
        lm_t_ra_t
        
        
        attn_habit_tc_ts_tc<- attn_habit_1[attn_habit_1$trial_name_2=="tc_ts_tc",]    
        lm_tc_ts_tc<- lm(attn_habit_tc_ts_tc$all_tw_call ~ attn_habit_tc_ts_tc$call_num)
        lm_tc_ts_tc
        
        
        attn_habit_tia_tib_tia<- attn_habit_1[attn_habit_1$trial_name_2=="tia_tib_tia",]  
        lm_tia_tib_tia<- lm(attn_habit_tia_tib_tia$all_tw_call ~ attn_habit_tia_tib_tia$call_num)
        lm_tia_tib_tia
        
        attn_habit_ts_tc_ts<- attn_habit_1[attn_habit_1$trial_name_2=="ts_tc_ts",] 
        lm_ts_tc_ts<- lm(attn_habit_ts_tc_ts$all_tw_call ~ attn_habit_ts_tc_ts$call_num)
        lm_ts_tc_ts
        #running the following
        lm_de_t_de
        lm_ra_t_ra
        lm_rra_rt_rra
        lm_t_de_t
        lm_t_ra_t
        lm_tc_ts_tc
        lm_tia_tib_tia
        lm_ts_tc_ts
        #2 run anova including all
        #3 compare which comparisons are significantly different
        
        #making an anove
        aov(lm_de_t_de,lm_ra_t_ra,lm_rra_rt_rra,lm_t_de_t, lm_t_ra_t, lm_tc_ts_tc, lm_tia_tib_tia, lm_ts_tc_ts)
        
        
        
        
        ######apparently my response variable is different for some or all of these, gotta work on that    
        
## PLotting the slopes in ggplot2
        
library(ggplot2)      
p <- ggplot(data = attn_habit_3, aes(x = set_num, y = all_tw_set, color = factor(trial_name_2)))  # first, we build a plot object and color points by trial
p <- p + xlab("set number") + ylab("number twitches per set")  # then we modify the axis labels
p <- p + geom_point( position=position_jitterdodge(jitter.width = 5, jitter.height = 1, dodge.width = NULL))  # then we make a scatterplot
p <- p + theme(legend.position = "bottom", legend.title = element_blank())  # then we modify the legend
 
p <- p + facet_wrap(~trial_name_2, ncol = 3) #facet by trial
p <- p + theme(legend.position = "none")
p <-p + geom_smooth(method = "glm", se = TRUE)  
p       

        #################QUESTION 3 ##############
        
        #IS there a significant difference in the number of twitches to 5th to 6th, and 5th to 7th set? 
        #Ways to analyse. 
        # can do a T-test between 5 and 6, and then 5 and 7
        # can do an anova between all three
        #then plot
        #Contents:
        #1 need to make a dummy variable to refer to just to 1 row per set per bat (for all_tw_set)
        #2 need to subset data to refer to one treatment
        #look up what goes into anova
        #make variables referring to set#==5, set#==6, set#==7 then run anova on those
        
        
        
        #1 need to make a dummy variable to refer to just to 1 row per set per bat (for all_tw_set)
        
        #import data, remove duds, remove bad treatments
        attention_Rcopy_individuals_condensed <- read.delim("~/Dropbox/Attention Project/R/attention_Rcopy_individuals_condensed.txt") #this file has all sets, and a row with condensed Tia_tib_Tia
        attention_Rcopy1<- subset(attention_Rcopy_individuals_condensed, attention_Rcopy_individuals_condensed$bat_name != "Blackbeard" & attention_Rcopy_individuals_condensed$bat_name != "Wramplemeier") #removes unresponsive bats
        attention_Rcopy2<-subset(attention_Rcopy1, attention_Rcopy1$trial_name != "t_rt_t" & attention_Rcopy1$trial_name != "rt_t_rt") #removes variables only tested on one bat
        #condenses all the 
        attn_r2 <- attention_Rcopy2 #short version
        #pull partial string match  x.1 for call number using grep
        
        attn_set <- attn_r2[grep(".1", attn_r2$call_num), ]
        class(attn_set$call_num)
        View(attn_set)
        
        
        #2 need to subset data to refer to one treatment at a time
        unique(attn_set$trial_name_2)
        
        subset_de <- subset(attn_set, attn_set$trial_name_2=="de_t_de")
        subset_ra <- subset(attn_set, attn_set$trial_name_2=="ra_t_ra")
        subset_rra <- subset(attn_set, attn_set$trial_name_2=="rra_rt_rra")
        subset_rt <- subset(attn_set, attn_set$trial_name_2=="rt_rra_rt")
        subset_t_de <- subset(attn_set, attn_set$trial_name_2=="t_de_t")
        subset_t_ra <- subset(attn_set, attn_set$trial_name_2=="t_ra_t")
        subset_tc <- subset(attn_set, attn_set$trial_name_2=="tc_ts_tc")
        subset_tia <- subset(attn_set, attn_set$trial_name_2=="tia_tib_tia")
        subset_ts <- subset(attn_set, attn_set$trial_name_2=="ts_tc_ts")
        
        #further subset to only include sets 5, 6, 7
        subset_de1 <- subset(subset_de, subset_de$set_num=="5" | subset_de$set_num=="6" | subset_de$set_num=="7" )
        subset_ra1 <- subset(subset_ra, subset_ra$set_num=="5" | subset_ra$set_num=="6" | subset_ra$set_num=="7" ) 
        subset_rra1 <- subset(subset_rra, subset_rra$set_num=="5" | subset_rra$set_num=="6" | subset_rra$set_num=="7" )
        subset_rt1 <- subset(subset_rt, subset_rt$set_num=="5" | subset_rt$set_num=="6" | subset_rt$set_num=="7" )
        subset_t_de1 <- subset(subset_t_de, subset_t_de$set_num=="5" | subset_t_de$set_num=="6" | subset_t_de$set_num=="7" )
        subset_t_ra1 <- subset(subset_t_ra, subset_t_ra$set_num=="5" | subset_t_ra$set_num=="6" | subset_t_ra$set_num=="7" )
        subset_tc1 <- subset(subset_tc, subset_tc$set_num=="5" | subset_tc$set_num=="6" | subset_tc$set_num=="7" )
        subset_tia1 <- subset(subset_tia, subset_tia$set_num=="5" | subset_tia$set_num=="6" | subset_tia$set_num=="7" )
        subset_ts1 <- subset(subset_ts, subset_ts$set_num=="5" | subset_ts$set_num=="6" | subset_ts$set_num=="7" )
        
        #load library
        library(nlme)
        #set working directory
        setwd("~/Dropbox/Attention Project/R/Sessions/09.26.16")        
        # run one of the lme
        # Model should be twitches per set ~ set num * treatment , random= batid #(trial number does not add explanatoryness)
        
        qqnorm(subset_de1$all_tw_set)
        de_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_de1) # use as.factor so model sees these as factors
        summary(de_desc)
        #boxplot
        de_t_de_desc <- boxplot(all_tw_set ~ (as.factor(set_num)), main = "de_t_de", data=subset_de1)
        de_t_de_desc
        # no evidence for descrimination          
        
        #ra
        qqnorm(subset_ra1$all_tw_set)
        ra_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_ra1) # use as.factor so moral sees these as factors
        summary(ra_desc)
        #boxplot
        ra_descrim_plot<- boxplot(all_tw_set ~ (as.factor(set_num)), main = "ra_t_ra", data=subset_ra1)
        
        #evidence for discrimination
        
        #rra
        qqnorm(subset_rra1$all_tw_set)
        #  Should be twitches/set ~set num*treatment rrandom= batid #trial number does not add explanatoryness
        rra_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_rra1) # use as.factor so morral sees these as factors
        summary(rra_desc)
        #boxplot
        boxplot(all_tw_set ~ (as.factor(set_num)), main = "rra_rt_rra", data=subset_rra1)
        # Evidence for descrimination between 5 and 6
        
        #rt
        qqnorm((subset_rt1$all_tw_set))
        #may not be appropriate for this test given right-skewed distribution
        rt_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_rt1) # use as.factor so mortl sees these as factors
        summary(rt_desc)
        #boxplot
        boxplot(all_tw_set ~ (as.factor(set_num)), main = "rt_rra_rt", data=subset_rt1)
        
        #If okay to transform: 
        qqnorm(log(subset_rt1$all_tw_set+1))
        rt_desc<- lme((log(all_tw_set+1)) ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_rt1) # use as.factor so mortl sees these as factors
        summary(rt_desc)
        #boxplot
        rt_desc_transformed<-boxplot((log(all_tw_set+1)) ~ (as.factor(set_num)), main = "rt_rra_rt log transformed", data=subset_rt1)
        #Regardless, there is no evidence of descrimination between reversed tungara and reversed alata, due mainly to low overall values
        
        
        #t_de
        qqnorm(subset_t_de1$all_tw_set)
        #  Should be twitches/set ~set num*treatment t_dendom= batid #trial number does not add explanatoryness
        t_de_t_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_t_de1) # use as.factor so mot_del sees these as factors
        summary(t_de_t_desc)
        #boxplot
        t_de_t_desc_plot<- boxplot(all_tw_set ~ (as.factor(set_num)), main = "t_de_t", data=subset_t_de1)
        # no evidence for descrimination between t and de. Perhaps due to high overall responses
        
        #t_ra
        #remember to reset relevel if running this again
        qqnorm(subset_t_ra1$all_tw_set) 
        hist(subset_t_ra1$all_tw_set)
        #maybe can't justify normal dist
        t_ra_t_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_t_ra1) # use as.factor so  sees these as factors
        summary(t_ra_t_desc)
        #boxplot
        t_ra_t_desc_plot<-boxplot(all_tw_set ~ (as.factor(set_num)), main = "t_ra_t", data=subset_t_ra1)
        
        #if can tranform:
        #not particularly better, but ah well
        qqnorm((sqrt(subset_t_ra1$all_tw_set)))
        hist((sqrt(subset_t_ra1$all_tw_set)))
        
        t_ra_t_desc_sqrt_trns<- lme((sqrt(all_tw_set)) ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_t_ra1) # use as.factor so  sees these as factors
        summary(t_ra_t_desc_sqrt_trns)
        #boxplot
        t_ra_t_desc_plot<-boxplot((sqrt(all_tw_set)) ~ (as.factor(set_num)), main = "t_ra_t sq.rt transformed", data=subset_t_ra1)
        #this way there is a significant difference at alpha.01 for 5 and 6, but it goes in the negative direction. no significant between 5 and 7. If I relevel, is there a difference between 6 and 7?
        subset_t_ra1$set_num <- relevel(as.factor(subset_t_ra1$set_num), ref="6")
        t_ra_t_desc_2<- lme((sqrt(all_tw_set)) ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_t_ra1) # releveled lme
        summary(t_ra_t_desc_2)
        
        #there is a marginally significant difference between 6 and 7 (p=0.945), and 6 and 5 (p=0.0988)
        #not sure if we are counting these (check back)
        #Shouldn't conclude too much, because data are so irregular
        
        #tc
        qqnorm(subset_tc1$all_tw_set)
        #one total outlier (what should I do about that?)
        tc_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_tc1) # use as.factor so  sees these as factors
        summary(tc_desc)
        # boxplot
        tc_ts_tc_plot<- boxplot(all_tw_set ~ (as.factor(set_num)), main = "tc_ts_tc", data=subset_tc1)
        #no apparent difference
        #but an outlier... should see if that's a problem, and how to fix
        #possibly bc little habituation to complex call
        
        
        #tia
        qqnorm(subset_tia1$all_tw_set)
        # not justifiably normal (left/0 skewed)
        tia_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_tia1) # use as.factor so  sees these as factors
        summary(tia_desc)
        # boxplot
        tia_tib_tia_plot<- boxplot(all_tw_set ~ (as.factor(set_num)), main = "tia_tib_tia", data=subset_tia1)
        
        #if can transform:
        qqnorm(sqrt(subset_tia1$all_tw_set))
        hist(sqrt(subset_tia1$all_tw_set))
        
        # not justifiably normal (left/0 skewed)
        tia_desc_sqrt_trans<- lme((sqrt(all_tw_set)) ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_tia1) # use as.factor so  sees these as factors
        summary(tia_desc_sqrt_trans)
        # boxplot
        tia_tib_tia_plot_sqrt<- boxplot((sqrt(all_tw_set)) ~ (as.factor(set_num)), main = "tia_tib_tia sqrt transformed", data=subset_tia1)
        #Anyway, no evidence of discrimination
        
        
        #ts
        qqnorm(subset_ts1$all_tw_set)
        hist(subset_ts1$all_tw_set)
        #looks oookay (pretty 0 weighted)
        ts_desc<- lme(all_tw_set ~ as.factor(set_num), random = ~1|Bat_ID, data = subset_ts1) # use as.factor so  sees these as factors
        summary(ts_desc)
        # boxplot
        ts_tc_ts_plot<- boxplot(all_tw_set ~ (as.factor(set_num)), main = "ts_tc_ts", data=subset_ts1)          
        #evidence for discrimination
        #no evidence for dishabituation
        
 ## Using ggplot to plot all of this, and also in one graph
 #attn_set   
 attn_set_sub <- subset(attn_set, attn_set$set_num=="5" | attn_set$set_num=="6" | attn_set$set_num=="7" )
        
p2 <- ggplot(data = subset_ts1, aes(x = set_num, y = all_tw_set, color = factor(set_num)))  # first, we build a plot object and color points by trial
        p2 <- p2 + xlab("set number") + ylab("number twitches per set")  # then we modify the axis labels
        p2 <- p2 + geom_boxplot()  # then we make a boxplot
        p2 <- p2 + theme(legend.position = "bottom", legend.title = element_blank())  # then we modify the legend
        
 
        
multiplot(p...)
    #figure out initial interest, add the glm results to the r markdown mile, send it out. send out sally one too with all the questions. Probably more urgent than this graphing.     
        
        
        
        #look up what goes into anova
        # need one-way repeated measures ANOVA, bc I have 1 dependent variable (twitches), and 1 IV with 3 levels, and the 3 levels are matched, not independent. Command will be anova() 
        
        
        
        #make variables referring to set#==5, set#==6, set#==7 then run anova on those
        
        make variable for 5
        make variable for 6
        nake variable for 7
        cbind(variable of set 5, 6, 7)
        anova()
        plot()
        
        
        
        
        
        
        #### Read full bodowinter article
        ### check for significant points using provided code
        ### see if there is anyway I can make my model more normal
        ### graph modelfor each treatment (intercept/ slope / conf intervals?)
        ?interaction.plot
        ?Enter slopes directly?
        #discrimination? 
        #if I do lme for this, I don't care very much accross treatments
        # so, can I subset my data? Do one treatment at a time
        # for example de_t_de
        # twitches ~ set# |BatID (include trial #?)
        
        #initial interest? (can TOTALLY use a lme for this, if not what is already included in model, then->
        # lme(twitches ~treatment(only in 1.1) random=batID
        # ***** the good stuff  ****
        attn_int_2 <- read.table("/Users/maydixon/Dropbox/Attention Project/R/attn_for_initial_interest.txt", header=TRUE)
        View(attn_int_2)
        initial_int_1 <-  lme(all_tw_set ~ trial_name_2, random = list(Bat_ID=~1, trial_number= ~1), data = attn_int_2)
        summary(initial_int_1)
        levels(attn_set$trial_name_2)
        