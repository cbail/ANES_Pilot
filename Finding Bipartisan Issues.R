
#maybe soething like this:
# https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html


anes_data<-read.csv("~/Desktop/Dropbox/Polarization Lab DiscussIT/ANES work/anes_pilot_2018.csv")

# library(haven)
# anes_data<-read_spss("~/Documents/Github/ANES_Pilot/anes_pilot_2018.sav")
# anes_data<-as_factor(anes_data)

#potential variables to analyze

# muellerinv
# harass:sexadvance
# 
# immignum
# birthright:famsep
# acaapprove:costins
# richpoor
# taxecon:taxapproval
# rr1:rr4
# warmcause:warmyou
# violence1:violence2
# freetrade:tariff_con
# opioiddo
# 
# 
# guncheck:gunteach
# sd1:sd6
# media1:media4
# mediaviol
# selfcensor
# rural1:rural3
# impeach1:impeach2


#code party id
anes_data$party<-NA
anes_data$party[anes_data$pid1d==1|anes_data$pid1r==1]<-"dem."
anes_data$party[anes_data$pid1d==2|anes_data$pid1r==2]<-"rep."
anes_data$party<-as.factor(anes_data$party)


#find variables of interest

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






#drop non-party-ided
anes_for_plot<-anes_data[!is.na(anes_data$party),]



#plot
ggplot(anes_for_plot, aes(x=gunteach,fill=party))+
  geom_density(alpha = 0.4, position = "stack")+
  scale_fill_manual(values=c("blue", "red"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))





