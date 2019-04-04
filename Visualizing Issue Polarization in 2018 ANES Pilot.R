
anes_data<-read.csv("~/Desktop/Dropbox/Polarization Lab DiscussIT/ANES work/anes_pilot_2018.csv")


#fix missing data on variables of interest

anes_data<- mutate_at(anes_data, vars(immignum),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(birthright:famsep),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(votestop1:integrity5),
                      list(~ ifelse( .==-1, NA, .)))

anes_data<- mutate_at(anes_data, vars(warmcause:warmyou),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(guncheck:gunteach),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(acaapprove:costins),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(richpoor),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(taxecon:taxapproval),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(freetrade:tariff_con),
                      list(~ ifelse( .<0, NA, .)))

anes_data<- mutate_at(anes_data, vars(opioiddo),
                      list(~ ifelse( .<0, NA, .)))

#code party id
anes_data$party<-NA
anes_data$party[anes_data$pid1d==1|anes_data$pid1r==1]<-"dem."
anes_data$party[anes_data$pid1d==2|anes_data$pid1r==2]<-"rep."
anes_data$party<-as.factor(anes_data$party)


#drop non-party-ided
anes_for_plot<-anes_data[!is.na(anes_data$party),]



#subset variables of interest

plotdata<-anes_data[,c(
"muellerinv", #7 point
"harass", #2 point
"harassstr", #5 point
"disc_selfsex", #5 point
"sexadvance", #5 point
"immignum", #7 point
"immigpol", #4 point
"birthright", #7 point
"wall", #7 point
"diversity", #7 point
"illimcrime", #7 point
"illimschool",
"illimecon",
"imigcit",
"ice",
"famsep",
"acaapprove",
"acarepeal",
"acains",
"loseins",
"costins",
"richpoor",
"taxecon",
"taxfam",
"taxapproval",
"rr1", # 5point and all below
"rr2",
"rr3",
"rr4",
"warmus",# also 5 point
"warmcom",
"warmyou",
"violence1", #7
"violence2", #5
"freetrade", #7 point
"tarriff_work",
"tariff_con", #7 point
"opioiddo", #7 point
"guncheck",
"gunsar",
"gunteach",
"sd1", #5 point below
"sd2",
"sd3",
"sd4",
"sd5",
"sd6",
"media1",
"media2",
"media3",
"media4",
"mediaviol",
"selfcensor",
"rural1",
"rural2",
"rural3",
"impeach1",
"impeach2")]



#create survey object
library(survey)
anes_design <-
  svydesign(
    id=~caseid ,
    data = anes_data ,
    weights = ~weight ,
    nest = TRUE
  )

library(srvyr)
anes_srvyr_design <- as_survey(anes_design)

#create summaries of variables
lr_gen_w2 <- data %>%
  mutate(sex = ifelse(gndr == 1, "Male", "Female")) %>%
  as_survey_design(weights = c(dweight, pspwght)) %>%
  group_by(cntry, sex) %>%
  summarize(n = survey_mean(lrscale, na.rm = T, vartype = "ci"))



library(ggplot2)
#plot
ggplot(anes_srvyr_design, aes(x=gunteach,fill=party))+
  geom_density(alpha = 0.4, position = "stack")+
  scale_fill_manual(values=c("blue", "red"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))









