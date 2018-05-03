library("rjson")
library(ggplot2)
library(reshape2)

CollegeDropout_json_data<- fromJSON(file= 'CollegeDropout.json')
LateRegistration_json_data<- fromJSON(file= 'LateRegistration.json')
Graduation_json_data<- fromJSON(file= 'Graduation.json')
Heartbreak_json_data<- fromJSON(file= 'Heartbreak.json')
MBDTF_json_data<- fromJSON(file= 'MBDTF.json')
Yeezus_json_data<- fromJSON(file= 'Yeezus.json')
TLOP_json_data<- fromJSON(file= 'TLOP.json')



Openness<- c(CollegeDropout_json_data$personality[[1]][4][[1]], LateRegistration_json_data$personality[[1]][4][[1]], 
             Graduation_json_data$personality[[1]][4][[1]], Heartbreak_json_data$personality[[1]][4][[1]],
             MBDTF_json_data$personality[[1]][4][[1]], Yeezus_json_data$personality[[1]][4][[1]], 
             TLOP_json_data$personality[[1]][4][[1]])

Conscientiousness<- c(CollegeDropout_json_data$personality[[2]][4][[1]], LateRegistration_json_data$personality[[2]][4][[1]], 
                      Graduation_json_data$personality[[2]][4][[1]], Heartbreak_json_data$personality[[2]][4][[1]],
                      MBDTF_json_data$personality[[2]][4][[1]], Yeezus_json_data$personality[[2]][4][[1]], 
                      TLOP_json_data$personality[[2]][4][[1]])

Extraversion<- c(CollegeDropout_json_data$personality[[3]][4][[1]], LateRegistration_json_data$personality[[3]][4][[1]], 
                 Graduation_json_data$personality[[3]][4][[1]], Heartbreak_json_data$personality[[3]][4][[1]], 
                 MBDTF_json_data$personality[[3]][4][[1]], Yeezus_json_data$personality[[3]][4][[1]], 
                 TLOP_json_data$personality[[3]][4][[1]])


Agreeableness<- c(CollegeDropout_json_data$personality[[4]][4][[1]], LateRegistration_json_data$personality[[4]][4][[1]], 
                  Graduation_json_data$personality[[4]][4][[1]], Heartbreak_json_data$personality[[4]][4][[1]],
                  MBDTF_json_data$personality[[4]][4][[1]], Yeezus_json_data$personality[[4]][4][[1]], 
                  TLOP_json_data$personality[[4]][4][[1]])

Emotional_range<- c(CollegeDropout_json_data$personality[[5]][4][[1]], LateRegistration_json_data$personality[[5]][4][[1]], 
                    Graduation_json_data$personality[[5]][4][[1]], Heartbreak_json_data$personality[[5]][4][[1]],
                    MBDTF_json_data$personality[[5]][4][[1]], Yeezus_json_data$personality[[5]][4][[1]], 
                    TLOP_json_data$personality[[5]][4][[1]])


Album_name= factor(c("College Dropout", "Late Registration", "Graduation", "808's & Heartbreak", "My Beautiful Dark Twisted Fantasy",
              "Yeezus", "The Life of Pablo"))

Album_name= factor(Album_name, levels(Album_name)[c(2,4,3,1,5,7,6)])

Kanye_Big5<- data.frame(Album_name, Openness, Conscientiousness, Extraversion, Agreeableness, Emotional_range)

Kanye.long<- melt(Kanye_Big5)

ggplot(Kanye.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")+ 
  labs(title="Kanye's Big 5 Traits", fill= "Album", x= "Personality Traits", y= "Percentile Rank")

Adventurousness<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    LateRegistration_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    Graduation_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    Heartbreak_json_data$personality[[1]][5][[1]][[1]][[4]],
                    MBDTF_json_data$personality[[1]][5][[1]][[1]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    TLOP_json_data$personality[[1]][5][[1]][[1]][[4]])

Artistic_Interest<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      LateRegistration_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      Graduation_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      Heartbreak_json_data$personality[[1]][5][[1]][[2]][[4]],
                      MBDTF_json_data$personality[[1]][5][[1]][[2]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      TLOP_json_data$personality[[1]][5][[1]][[2]][[4]])

Emotionality<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 LateRegistration_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 Graduation_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 Heartbreak_json_data$personality[[1]][5][[1]][[3]][[4]],
                 MBDTF_json_data$personality[[1]][5][[1]][[3]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 TLOP_json_data$personality[[1]][5][[1]][[3]][[4]])

Imagination<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[4]][[4]], 
                LateRegistration_json_data$personality[[1]][5][[1]][[4]][[4]], 
                Graduation_json_data$personality[[1]][5][[1]][[4]][[4]], 
                Heartbreak_json_data$personality[[1]][5][[1]][[4]][[4]],
                MBDTF_json_data$personality[[1]][5][[1]][[4]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[4]][[4]], 
                TLOP_json_data$personality[[1]][5][[1]][[4]][[4]])


Intellect<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[5]][[4]], 
              LateRegistration_json_data$personality[[1]][5][[1]][[5]][[4]], 
              Graduation_json_data$personality[[1]][5][[1]][[5]][[4]], 
              Heartbreak_json_data$personality[[1]][5][[1]][[5]][[4]],
              MBDTF_json_data$personality[[1]][5][[1]][[5]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[5]][[4]], 
              TLOP_json_data$personality[[1]][5][[1]][[5]][[4]])

Authority_Challenge<- c(CollegeDropout_json_data$personality[[1]][5][[1]][[6]][[4]], 
                        LateRegistration_json_data$personality[[1]][5][[1]][[6]][[4]], 
                        Graduation_json_data$personality[[1]][5][[1]][[6]][[4]], 
                        Heartbreak_json_data$personality[[1]][5][[1]][[6]][[4]],
                        MBDTF_json_data$personality[[1]][5][[1]][[6]][[4]], Yeezus_json_data$personality[[1]][5][[1]][[6]][[4]], 
                        TLOP_json_data$personality[[1]][5][[1]][[6]][[4]])

Kanye_Openness<- data.frame(Album_name, Adventurousness, Artistic_Interest, Emotionality, Imagination, Intellect, 
                               Authority_Challenging)

Kanye_Openness.long<- melt(Kanye_Openness)

ggplot(Kanye_Openness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")+ 
  labs(title="Kanye's Openness", fill= "Album", x= "Openness Facets", y= "Percentile Rank")

Achievement_Striving<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         LateRegistration_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         Graduation_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         Heartbreak_json_data$personality[[2]][5][[1]][[1]][[4]],
                         MBDTF_json_data$personality[[2]][5][[1]][[1]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         TLOP_json_data$personality[[2]][5][[1]][[1]][[4]])

Cautiousness<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[2]][[4]], 
                  LateRegistration_json_data$personality[[2]][5][[1]][[2]][[4]], 
                  Graduation_json_data$personality[[2]][5][[1]][[2]][[4]], 
                  Heartbreak_json_data$personality[[2]][5][[1]][[2]][[4]],
                  MBDTF_json_data$personality[[2]][5][[1]][[2]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[2]][[4]], 
                  TLOP_json_data$personality[[2]][5][[1]][[2]][[4]])

Dutifulness<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[3]][[4]], 
                LateRegistration_json_data$personality[[2]][5][[1]][[3]][[4]], 
                Graduation_json_data$personality[[2]][5][[1]][[3]][[4]], 
                Heartbreak_json_data$personality[[2]][5][[1]][[3]][[4]],
                MBDTF_json_data$personality[[2]][5][[1]][[3]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[3]][[4]], 
                TLOP_json_data$personality[[2]][5][[1]][[3]][[4]])

Orderliness<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[4]][[4]], 
                LateRegistration_json_data$personality[[2]][5][[1]][[4]][[4]], 
                Graduation_json_data$personality[[2]][5][[1]][[4]][[4]], 
                Heartbreak_json_data$personality[[2]][5][[1]][[4]][[4]],
                MBDTF_json_data$personality[[2]][5][[1]][[4]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[4]][[4]], 
                TLOP_json_data$personality[[2]][5][[1]][[4]][[4]])

Self_Discipline<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    LateRegistration_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    Graduation_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    Heartbreak_json_data$personality[[2]][5][[1]][[5]][[4]],
                    MBDTF_json_data$personality[[2]][5][[1]][[5]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    TLOP_json_data$personality[[2]][5][[1]][[5]][[4]])

Self_Efficacy<- c(CollegeDropout_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  LateRegistration_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  Graduation_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  Heartbreak_json_data$personality[[2]][5][[1]][[6]][[4]],
                  MBDTF_json_data$personality[[2]][5][[1]][[6]][[4]], Yeezus_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  TLOP_json_data$personality[[2]][5][[1]][[6]][[4]])

Kanye_Conscientiousness<- data.frame(Album_name, Achievement_Striving, Cautiousness, Dutifulness, Orderliness,
                                        Self_Discipline, Self_Efficacy)

Kanye_Conscientiousness.long<- melt(Kanye_Conscientiousness)

ggplot(Kanye_Conscientiousness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")+ 
  labs(title="Kanye's Conscientiousness", fill= "Album", x= "Conscientiousness Facets", y= "Percentile Rank")

Activity<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[1]][[4]], 
             LateRegistration_json_data$personality[[3]][5][[1]][[1]][[4]], 
             Graduation_json_data$personality[[3]][5][[1]][[1]][[4]], 
             Heartbreak_json_data$personality[[3]][5][[1]][[1]][[4]],
             MBDTF_json_data$personality[[3]][5][[1]][[1]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[1]][[4]], 
             TLOP_json_data$personality[[3]][5][[1]][[1]][[4]])

Assertive<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[2]][[4]], 
              LateRegistration_json_data$personality[[3]][5][[1]][[2]][[4]], 
              Graduation_json_data$personality[[3]][5][[1]][[2]][[4]], 
              Heartbreak_json_data$personality[[3]][5][[1]][[2]][[4]],
              MBDTF_json_data$personality[[3]][5][[1]][[2]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[2]][[4]], 
              TLOP_json_data$personality[[3]][5][[1]][[2]][[4]])

Cheerful<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[3]][[4]], 
             LateRegistration_json_data$personality[[3]][5][[1]][[3]][[4]], 
             Graduation_json_data$personality[[3]][5][[1]][[3]][[4]], 
             Heartbreak_json_data$personality[[3]][5][[1]][[3]][[4]],
             MBDTF_json_data$personality[[3]][5][[1]][[3]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[3]][[4]], 
             TLOP_json_data$personality[[3]][5][[1]][[3]][[4]])

Thrill_Seeking<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   LateRegistration_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   Graduation_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   Heartbreak_json_data$personality[[3]][5][[1]][[4]][[4]],
                   MBDTF_json_data$personality[[3]][5][[1]][[4]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   TLOP_json_data$personality[[3]][5][[1]][[4]][[4]])


Warmth<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[5]][[4]], 
             LateRegistration_json_data$personality[[3]][5][[1]][[5]][[4]], 
             Graduation_json_data$personality[[3]][5][[1]][[5]][[4]], 
             Heartbreak_json_data$personality[[3]][5][[1]][[5]][[4]],
             MBDTF_json_data$personality[[3]][5][[1]][[5]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[5]][[4]], 
             TLOP_json_data$personality[[3]][5][[1]][[5]][[4]])

Gregariousness<- c(CollegeDropout_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   LateRegistration_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   Graduation_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   Heartbreak_json_data$personality[[3]][5][[1]][[6]][[4]],
                   MBDTF_json_data$personality[[3]][5][[1]][[6]][[4]], Yeezus_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   TLOP_json_data$personality[[3]][5][[1]][[6]][[4]])


Kanye_Extraversion<- data.frame(Album_name, Activity, Assertive, Cheerful,
                                   Thrill_Seeking, Outgoing, Gregariousness)

Kanye_Extraversion.long<- melt(Kanye_Extraversion)

ggplot(Kanye_Extraversion.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge") + 
  labs(title="Kanye's Extraversion", fill= "Album", x= "Extraversion Facets", y= "Percentile Rank")

Altruism<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[1]][[4]], 
             LateRegistration_json_data$personality[[4]][5][[1]][[1]][[4]], 
             Graduation_json_data$personality[[4]][5][[1]][[1]][[4]], 
             Heartbreak_json_data$personality[[4]][5][[1]][[1]][[4]],
             MBDTF_json_data$personality[[4]][5][[1]][[1]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[1]][[4]], 
             TLOP_json_data$personality[[4]][5][[1]][[1]][[4]])

Cooperation<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[2]][[4]], 
                LateRegistration_json_data$personality[[4]][5][[1]][[2]][[4]], 
                Graduation_json_data$personality[[4]][5][[1]][[2]][[4]], 
                Heartbreak_json_data$personality[[4]][5][[1]][[2]][[4]],
                MBDTF_json_data$personality[[4]][5][[1]][[2]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[2]][[4]], 
                TLOP_json_data$personality[[4]][5][[1]][[2]][[4]])


Modesty<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[3]][[4]], 
            LateRegistration_json_data$personality[[4]][5][[1]][[3]][[4]], 
            Graduation_json_data$personality[[4]][5][[1]][[3]][[4]], 
            Heartbreak_json_data$personality[[4]][5][[1]][[3]][[4]],
            MBDTF_json_data$personality[[4]][5][[1]][[3]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[3]][[4]], 
            TLOP_json_data$personality[[4]][5][[1]][[3]][[4]])

Uncomprimising<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[4]][[4]], 
                    LateRegistration_json_data$personality[[4]][5][[1]][[4]][[4]], 
                    Graduation_json_data$personality[[4]][5][[1]][[4]][[4]], 
                    Heartbreak_json_data$personality[[4]][5][[1]][[4]][[4]],
                    MBDTF_json_data$personality[[4]][5][[1]][[4]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[4]][[4]], 
                    TLOP_json_data$personality[[4]][5][[1]][[4]][[4]])


Sympathy<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[5]][[4]], 
             LateRegistration_json_data$personality[[4]][5][[1]][[5]][[4]], 
             Graduation_json_data$personality[[4]][5][[1]][[5]][[4]], 
             Heartbreak_json_data$personality[[4]][5][[1]][[5]][[4]],
             MBDTF_json_data$personality[[4]][5][[1]][[5]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[5]][[4]], 
             TLOP_json_data$personality[[4]][5][[1]][[5]][[4]])

Trust<- c(CollegeDropout_json_data$personality[[4]][5][[1]][[6]][[4]], 
          LateRegistration_json_data$personality[[4]][5][[1]][[6]][[4]], 
          Graduation_json_data$personality[[4]][5][[1]][[6]][[4]], 
          Heartbreak_json_data$personality[[4]][5][[1]][[6]][[4]],
          MBDTF_json_data$personality[[4]][5][[1]][[6]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[6]][[4]], 
          TLOP_json_data$personality[[4]][5][[1]][[6]][[4]])

Kanye_Agreeableness<- data.frame(Album_name, Altruism, Cooperation, Modesty, 
                                    Uncomprimising, Sympathy, Trust)

Kanye_Agreeableness.long<- melt(Kanye_Agreeableness)

ggplot(Kanye_Agreeableness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")+ 
  labs(title="Kanye's Agreeableness", fill= "Album", x= "Agreeableness Facets", y= "Percentile Rank")


Fiery<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[1]][[4]], 
          LateRegistration_json_data$personality[[5]][5][[1]][[1]][[4]], 
          Graduation_json_data$personality[[5]][5][[1]][[1]][[4]], 
          Heartbreak_json_data$personality[[5]][5][[1]][[1]][[4]],
          MBDTF_json_data$personality[[5]][5][[1]][[1]][[4]], Yeezus_json_data$personality[[5]][5][[1]][[1]][[4]], 
          TLOP_json_data$personality[[5]][5][[1]][[1]][[4]])

Worry_Prone<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[2]][[4]], 
                LateRegistration_json_data$personality[[5]][5][[1]][[2]][[4]], 
                Graduation_json_data$personality[[5]][5][[1]][[2]][[4]], 
                Heartbreak_json_data$personality[[5]][5][[1]][[2]][[4]],
                MBDTF_json_data$personality[[5]][5][[1]][[2]][[4]], Yeezus_json_data$personality[[5]][5][[1]][[2]][[4]], 
                TLOP_json_data$personality[[5]][5][[1]][[2]][[4]])

Melancholy<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[3]][[4]], 
                LateRegistration_json_data$personality[[5]][5][[1]][[3]][[4]], 
                Graduation_json_data$personality[[5]][5][[1]][[3]][[4]], 
                Heartbreak_json_data$personality[[5]][5][[1]][[3]][[4]],
                MBDTF_json_data$personality[[5]][5][[1]][[3]][[4]], Yeezus_json_data$personality[[5]][5][[1]][[3]][[4]], 
                TLOP_json_data$personality[[5]][5][[1]][[3]][[4]])
               

Immoderation<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 LateRegistration_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 Graduation_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 Heartbreak_json_data$personality[[5]][5][[1]][[4]][[4]],
                 MBDTF_json_data$personality[[5]][5][[1]][[4]][[4]], Yeezus_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 TLOP_json_data$personality[[5]][5][[1]][[4]][[4]])


Self_Conscious<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[5]][[4]], 
                   LateRegistration_json_data$personality[[5]][5][[1]][[5]][[4]], 
                   Graduation_json_data$personality[[5]][5][[1]][[5]][[4]], 
                   Heartbreak_json_data$personality[[5]][5][[1]][[5]][[4]],
                   MBDTF_json_data$personality[[5]][5][[1]][[5]][[4]], Yeezus_json_data$personality[[5]][5][[1]][[5]][[4]], 
                   TLOP_json_data$personality[[5]][5][[1]][[5]][[4]])


Stress<- c(CollegeDropout_json_data$personality[[5]][5][[1]][[6]][[4]], 
           LateRegistration_json_data$personality[[5]][5][[1]][[6]][[4]], 
           Graduation_json_data$personality[[5]][5][[1]][[6]][[4]], 
           Heartbreak_json_data$personality[[5]][5][[1]][[6]][[4]],
           MBDTF_json_data$personality[[5]][5][[1]][[6]][[4]], Yeezus_json_data$personality[[4]][5][[1]][[6]][[4]], 
           TLOP_json_data$personality[[5]][5][[1]][[6]][[4]])

Kanye_Neuroticism<- data.frame(Album_name, Fiery, Worry_Prone, Melancholy,
                                  Immoderation, Self_Conscious, Stress)

Kanye_Neuroticism.long<- melt(Kanye_Neuroticism)

ggplot(Kanye_Neuroticism.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge") + 
  labs(title="Kanye's Emotional Range", fill= "Album", x= "Emotional Range Facets", y= "Percentile Rank")


Challenge<- c(CollegeDropout_json_data$needs[[1]][[4]], 
              LateRegistration_json_data$needs[[1]][[4]], 
              Graduation_json_data$needs[[1]][[4]], 
              Heartbreak_json_data$needs[[1]][[4]],
              MBDTF_json_data$needs[[1]][[4]],
              Yeezus_json_data$needs[[1]][[4]], 
              TLOP_json_data$needs[[1]][[4]])

Closeness<- c(CollegeDropout_json_data$needs[[2]][[4]], 
              LateRegistration_json_data$needs[[2]][[4]], 
              Graduation_json_data$needs[[2]][[4]], 
              Heartbreak_json_data$needs[[2]][[4]],
              MBDTF_json_data$needs[[2]][[4]],
              Yeezus_json_data$needs[[2]][[4]], 
              TLOP_json_data$needs[[2]][[4]])

Curiosity<- c(CollegeDropout_json_data$needs[[3]][[4]], 
              LateRegistration_json_data$needs[[3]][[4]], 
              Graduation_json_data$needs[[3]][[4]], 
              Heartbreak_json_data$needs[[3]][[4]],
              MBDTF_json_data$needs[[3]][[4]],
              Yeezus_json_data$needs[[3]][[4]], 
              TLOP_json_data$needs[[3]][[4]])

Excitement<- c(CollegeDropout_json_data$needs[[4]][[4]], 
               LateRegistration_json_data$needs[[4]][[4]], 
               Graduation_json_data$needs[[4]][[4]], 
               Heartbreak_json_data$needs[[4]][[4]],
               MBDTF_json_data$needs[[4]][[4]],
               Yeezus_json_data$needs[[4]][[4]], 
               TLOP_json_data$needs[[4]][[4]])


Harmony<- c(CollegeDropout_json_data$needs[[5]][[4]], 
            LateRegistration_json_data$needs[[5]][[4]], 
            Graduation_json_data$needs[[5]][[4]], 
            Heartbreak_json_data$needs[[5]][[4]],
            MBDTF_json_data$needs[[5]][[4]],
            Yeezus_json_data$needs[[5]][[4]], 
            TLOP_json_data$needs[[5]][[4]])

Ideal<- c(CollegeDropout_json_data$needs[[6]][[4]], 
          LateRegistration_json_data$needs[[6]][[4]], 
          Graduation_json_data$needs[[6]][[4]], 
          Heartbreak_json_data$needs[[6]][[4]],
          MBDTF_json_data$needs[[6]][[4]],
          Yeezus_json_data$needs[[6]][[4]], 
          TLOP_json_data$needs[[6]][[4]])


Liberty<- c(CollegeDropout_json_data$needs[[7]][[4]], 
            LateRegistration_json_data$needs[[7]][[4]], 
            Graduation_json_data$needs[[7]][[4]], 
            Heartbreak_json_data$needs[[7]][[4]],
            MBDTF_json_data$needs[[7]][[4]],
            Yeezus_json_data$needs[[7]][[4]], 
            TLOP_json_data$needs[[7]][[4]])

Love<- c(CollegeDropout_json_data$needs[[8]][[4]], 
         LateRegistration_json_data$needs[[8]][[4]], 
         Graduation_json_data$needs[[8]][[4]], 
         Heartbreak_json_data$needs[[8]][[4]],
         MBDTF_json_data$needs[[8]][[4]],
         Yeezus_json_data$needs[[8]][[4]], 
         TLOP_json_data$needs[[8]][[4]])

Practicality<- c(CollegeDropout_json_data$needs[[9]][[4]], 
                 LateRegistration_json_data$needs[[9]][[4]], 
                 Graduation_json_data$needs[[9]][[4]], 
                 Heartbreak_json_data$needs[[9]][[4]],
                 MBDTF_json_data$needs[[9]][[4]],
                 Yeezus_json_data$needs[[9]][[4]], 
                 TLOP_json_data$needs[[9]][[4]])

Self_Expression<- c(CollegeDropout_json_data$needs[[10]][[4]], 
                    LateRegistration_json_data$needs[[10]][[4]], 
                    Graduation_json_data$needs[[10]][[4]], 
                    Heartbreak_json_data$needs[[10]][[4]],
                    MBDTF_json_data$needs[[10]][[4]],
                    Yeezus_json_data$needs[[10]][[4]], 
                    TLOP_json_data$needs[[10]][[4]])

Stability<- c(CollegeDropout_json_data$needs[[11]][[4]], 
              LateRegistration_json_data$needs[[11]][[4]], 
              Graduation_json_data$needs[[11]][[4]], 
              Heartbreak_json_data$needs[[11]][[4]],
              MBDTF_json_data$needs[[11]][[4]],
              Yeezus_json_data$needs[[11]][[4]], 
              TLOP_json_data$needs[[11]][[4]])

Structure<- c(CollegeDropout_json_data$needs[[12]][[4]], 
              LateRegistration_json_data$needs[[12]][[4]], 
              Graduation_json_data$needs[[12]][[4]], 
              Heartbreak_json_data$needs[[12]][[4]],
              MBDTF_json_data$needs[[12]][[4]],
              Yeezus_json_data$needs[[12]][[4]], 
              TLOP_json_data$needs[[12]][[4]])


Kanye_Needs1<- data.frame(Album_name, Challenge, Closeness, Curiosity, Excitement, Harmony, Ideal)

Kanye_Needs1.long<- melt(Kanye_Needs1)

ggplot(Kanye_Needs1.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge") + 
  labs(title="Kanye's Consumer Needs", fill= "Album", x= "Consumer Needs", y= "Percentile Rank")

Kanye_Needs2<- data.frame(Album_name, Liberty, Love, Practicality, Self_Expression, Stability,
                          Structure)

Kanye_Needs2.long<- melt(Kanye_Needs2)

ggplot(Kanye_Needs2.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge") + 
  labs(title="Kanye's Consumer Needs", fill= "Album", x= "Consumer Needs", y= "Percentile Rank")

Tradition<- c(CollegeDropout_json_data$needs[[1]][[4]], 
              LateRegistration_json_data$needs[[1]][[4]], 
              Graduation_json_data$needs[[1]][[4]], 
              Heartbreak_json_data$needs[[1]][[4]],
              MBDTF_json_data$needs[[1]][[4]],
              Yeezus_json_data$needs[[1]][[4]], 
              TLOP_json_data$needs[[1]][[4]])

Stimulation<- c(CollegeDropout_json_data$needs[[2]][[4]], 
                LateRegistration_json_data$needs[[2]][[4]], 
                Graduation_json_data$needs[[2]][[4]], 
                Heartbreak_json_data$needs[[2]][[4]],
                MBDTF_json_data$needs[[2]][[4]],
                Yeezus_json_data$needs[[2]][[4]], 
                TLOP_json_data$needs[[2]][[4]])

Hedonism<- c(CollegeDropout_json_data$needs[[3]][[4]], 
             LateRegistration_json_data$needs[[3]][[4]], 
             Graduation_json_data$needs[[3]][[4]], 
             Heartbreak_json_data$needs[[3]][[4]],
             MBDTF_json_data$needs[[3]][[4]],
             Yeezus_json_data$needs[[3]][[4]], 
             TLOP_json_data$needs[[3]][[4]])

Achievement<- c(CollegeDropout_json_data$needs[[4]][[4]], 
                LateRegistration_json_data$needs[[4]][[4]], 
                Graduation_json_data$needs[[4]][[4]], 
                Heartbreak_json_data$needs[[4]][[4]],
                MBDTF_json_data$needs[[4]][[4]],
                Yeezus_json_data$needs[[4]][[4]], 
                TLOP_json_data$needs[[4]][[4]])

Transcendence<- c(CollegeDropout_json_data$needs[[5]][[4]], 
                  LateRegistration_json_data$needs[[5]][[4]], 
                  Graduation_json_data$needs[[5]][[4]], 
                  Heartbreak_json_data$needs[[5]][[4]],
                  MBDTF_json_data$needs[[5]][[4]],
                  Yeezus_json_data$needs[[5]][[4]], 
                  TLOP_json_data$needs[[5]][[4]])



Kanye_Values<- data.frame(Album_name, Tradition, Stimulation, Hedonism, Achievement, Transcendence)

Kanye_Values.long<- melt(Kanye_Values)

ggplot(Kanye_Values.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge") + 
  labs(title="Kanye's Values", fill= "Album", x= "Values", y= "Percentile Rank")
