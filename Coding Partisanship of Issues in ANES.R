
anes_data<-read.csv("~/Desktop/Dropbox/Polarization Lab DiscussIT/ANES work/anes_pilot_2018.csv")


#code party id
anes_data$party<-NA
anes_data$party[anes_data$pid1d==1|anes_data$pid1r==1]<-"dem."
anes_data$party[anes_data$pid1d==2|anes_data$pid1r==2]<-"rep."
anes_data$party<-as.factor(anes_data$party)


#recode missing values
anes_data<- mutate_at(anes_data, vars(fttrump:ftantifa),
                   list(~ ifelse( .> 100, NA, .)))
anes_data<- mutate_at(anes_data, vars(fttrump:ftantifa),
                      list(~ ifelse( .< 0, NA, .)))


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

#calculate feeling thermometers by group
summary_table<-anes_srvyr_design %>%
  group_by(party) %>%
  summarize(fttrump_mean=survey_mean(fttrump, na.rm=TRUE),
            ftobama_mean=survey_mean(ftobama, na.rm=TRUE),
            ftblack_mean=survey_mean(ftblack, na.rm=TRUE),
            ftwhite_mean=survey_mean(ftwhite, na.rm=TRUE),
            fthisp_mean=survey_mean(fthisp, na.rm=TRUE),
            ftasian_mean=survey_mean(ftasian, na.rm=TRUE),
            ftgay_mean=survey_mean(ftgay, na.rm=TRUE),
            fthrc_mean=survey_mean(fthrc, na.rm=TRUE),
            ftrural_mean=survey_mean(ftrural, na.rm=TRUE),
            ftsocialists_mean=survey_mean(ftsocialists, na.rm=TRUE),
            ftcapitalists_mean=survey_mean(ftcapitalists, na.rm=TRUE),
            ftimmig_mean=survey_mean(ftimmig, na.rm=TRUE),
            ftpolice_mean=survey_mean(ftpolice, na.rm=TRUE),
            ftjournal_mean=survey_mean(ftjournal, na.rm=TRUE),
            ftmuslim_mean=survey_mean(ftmuslim, na.rm=TRUE),
            ftmueller_mean=survey_mean(ftmueller, na.rm=TRUE),
            ftfbi_mean=survey_mean(ftfbi, na.rm=TRUE),
            ftkavanaugh_mean=survey_mean(ftkavanaugh, na.rm=TRUE),
            ftaltright_mean=survey_mean(ftaltright, na.rm=TRUE),
            ftantifa_mean=survey_mean(ftantifa, na.rm=TRUE)
            )
    

#wide to long
evens<-c(seq(2,40, by=2))
odds<-c(seq(3,41, by=2))

estimates<-summary_table %>%
  select(evens)

standard_errors<-summary_table %>%
        select(odds)
  
length(names(estimates))
length(estimates[1,])

democrats<-as.data.frame(cbind(as.character(names(estimates)), 
                         as.numeric(estimates[1,]),
                         as.numeric(standard_errors[1,])),
                         stringsAsFactors = FALSE
                          )

democrats$party<-"Democrats"
names(democrats)<-c("thermometer","mean","se","Party")


republicans<-as.data.frame(cbind(as.character(names(estimates)), 
                               as.numeric(estimates[2,]),
                               as.numeric(standard_errors[2,])),
                           stringsAsFactors = FALSE
)

republicans$party<-"Republicans"
names(republicans)<-c("thermometer","mean","se","Party")

results<-rbind(democrats, republicans)
results$mean<-as.numeric(as.character(results$mean))
results$se<-as.numeric(as.character(results$se))

results$thermometer<-factor(results$thermometer,
  levels=c(
          #Individuals    
          "fttrump_mean",
          "ftobama_mean",
          "fthrc_mean",
          "ftkavanaugh_mean",
          "ftmueller_mean",
          
          #Groups
          "ftfbi_mean",
          "ftpolice_mean",
          "ftscotus_mean",
          "ftjournal_mean",
          "ftsocialists_mean",
          "ftcapitalists_mean",
          "ftaltright_mean",
          "ftantifa_mean",
          "ftmetoo_mean",
          "ftrural_mean",
          
          #Minority Groups
           
          "ftblack_mean",
          "ftasian_mean",
          "fthisp_mean",
          "ftwhite_mean",
          "ftmuslim_mean",
          "ftimmig_mean",
          "ftgay_mean",
          "fttrans_mean"),
  
  labels=c("Donald Trump",
           "Barack Obama",
           "Hillary Clinton",
           "Brett Kavanaugh",
           "Robert Mueller",
           
           "FBI",
           "Police",
           "Supreme Court",
           "Journalists",
           "Socialists",
           "Capitalists",
           "Alt-Right",
           "Antifa",
           "#Metoo",
           "Rural Americans",
          
        
           "Blacks",
           "Asians",
           "Hispanics",
           "Whites",
           "Muslims",
           "Immigrants",
           "Gays/Lesbians",
           "Transexuals"
           ))
  

results$upper<-results$mean+results$se
results$lower<-results$mean-results$se

library(ggplot2)

plotout<-ggplot(results, aes(x=Party, y=mean))+
  geom_bar(stat="identity",aes(fill=Party))+
  #geom_errorbar(aes(ymin=lower, ymax=upper))+
  facet_wrap(~thermometer)+
  theme_minimal()+
  scale_fill_manual(values=c("blue", "red"))+
  labs(title = "How Democrats and Republicans Rate Different People & Groups",
       subtitle = "Mean 'Thermometer' Rating (0=very unfavorable, 100=very favorable)",
       caption = "Source: 2018 American National Election Study", 
       x = "", y = "Thermometer Rating")+
  theme(axis.text.x=element_blank(),
        plot.title = element_text(face="bold"))

ggsave(plotout, file="ANES Thermometer Plot.png", width=8, height=6)
  
  




