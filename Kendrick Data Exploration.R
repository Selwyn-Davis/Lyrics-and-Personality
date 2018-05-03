library("rjson")
library(ggplot2)
library(reshape2)

Section80_json_data<- fromJSON(file= 'Section80.json')
GoodKidMaadCity_json_data<- fromJSON(file= 'GoodKidMaadCity.json')
ToPimpAButterfly_json_data<- fromJSON(file= 'ToPimpAButterfly.json')
Damn_json_data<- fromJSON(file= 'DAMN.json')
Untitled_json_data<- fromJSON(file= 'UntitledUnmastered.json')



Openness<- c(Section80_json_data$personality[[1]][4][[1]], GoodKidMaadCity_json_data$personality[[1]][4][[1]], 
            ToPimpAButterfly_json_data$personality[[1]][4][[1]], Damn_json_data$personality[[1]][4][[1]],
            Untitled_json_data$personality[[1]][4][[1]])

Conscientiousness<- c(Section80_json_data$personality[[2]][4][[1]], GoodKidMaadCity_json_data$personality[[2]][4][[1]], 
                      ToPimpAButterfly_json_data$personality[[2]][4][[1]], Damn_json_data$personality[[2]][4][[1]],
                      Untitled_json_data$personality[[2]][4][[1]])

Extraversion<- c(Section80_json_data$personality[[3]][4][[1]], GoodKidMaadCity_json_data$personality[[3]][4][[1]], 
              ToPimpAButterfly_json_data$personality[[3]][4][[1]], Damn_json_data$personality[[3]][4][[1]],
              Untitled_json_data$personality[[3]][4][[1]])

Agreeableness<- c(Section80_json_data$personality[[4]][4][[1]], GoodKidMaadCity_json_data$personality[[4]][4][[1]], 
                     ToPimpAButterfly_json_data$personality[[4]][4][[1]], Damn_json_data$personality[[4]][4][[1]],
                  Untitled_json_data$personality[[4]][4][[1]])

Emotional_range<- c(Section80_json_data$personality[[5]][4][[1]], GoodKidMaadCity_json_data$personality[[5]][4][[1]], 
                  ToPimpAButterfly_json_data$personality[[5]][4][[1]], Damn_json_data$personality[[5]][4][[1]],
                  Untitled_json_data$personality[[5]][4][[1]])

Album_name= factor(c("Section 80", "Good Kid Maad City", "To Pimp A Butterfly", "Damn",
                    "Untitled Unmastered"))

Album_name= factor(Album_name, levels(Album_name)[c(3,2,4,5,1)])


Kendrick_Big5<- data.frame(Album_name, Openness, Conscientiousness, Extraversion, Agreeableness, Emotional_range)


Kendrick.long<- melt(Kendrick_Big5)

ggplot(Kendrick.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Adventurousness<- c(Section80_json_data$personality[[1]][5][[1]][[1]][[4]], 
                      GoodKidMaadCity_json_data$personality[[1]][5][[1]][[1]][[4]], 
                      ToPimpAButterfly_json_data$personality[[1]][5][[1]][[1]][[4]], 
                      Damn_json_data$personality[[1]][5][[1]][[1]][[4]],
                      Untitled_json_data$personality[[1]][5][[1]][[1]][[4]])

Artistic_Interest<- c(Section80_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      GoodKidMaadCity_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      ToPimpAButterfly_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      Damn_json_data$personality[[1]][5][[1]][[2]][[4]],
                      Untitled_json_data$personality[[1]][5][[1]][[2]][[4]])

Emotionality<- c(Section80_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 GoodKidMaadCity_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 ToPimpAButterfly_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 Damn_json_data$personality[[1]][5][[1]][[3]][[4]],
                 Untitled_json_data$personality[[1]][5][[1]][[3]][[4]])

Imagination<- c(Section80_json_data$personality[[1]][5][[1]][[4]][[4]], 
                GoodKidMaadCity_json_data$personality[[1]][5][[1]][[4]][[4]], 
                ToPimpAButterfly_json_data$personality[[1]][5][[1]][[4]][[4]], 
                Damn_json_data$personality[[1]][5][[1]][[4]][[4]],
                Untitled_json_data$personality[[1]][5][[1]][[4]][[4]])


Intellect<- c(Section80_json_data$personality[[1]][5][[1]][[5]][[4]], 
              GoodKidMaadCity_json_data$personality[[1]][5][[1]][[5]][[4]], 
              ToPimpAButterfly_json_data$personality[[1]][5][[1]][[5]][[4]], 
              Damn_json_data$personality[[1]][5][[1]][[5]][[4]],
              Untitled_json_data$personality[[1]][5][[1]][[5]][[4]])

Authority_Challenging<- c(Section80_json_data$personality[[1]][5][[1]][[6]][[4]], 
                          GoodKidMaadCity_json_data$personality[[1]][5][[1]][[6]][[4]], 
                          ToPimpAButterfly_json_data$personality[[1]][5][[1]][[6]][[4]], 
                          Damn_json_data$personality[[1]][5][[1]][[6]][[4]],
                          Untitled_json_data$personality[[1]][5][[1]][[6]][[4]])

Kendrick_Openness<- data.frame(Album_name, Adventurousness, Artistic_Interest, Emotionality, Imagination, Intellect, 
                               Authority_Challenging)

Kendrick_Openness.long<- melt(Kendrick_Openness)

ggplot(Kendrick_Openness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")


Achievement_Striving<- c(Section80_json_data$personality[[2]][5][[1]][[1]][[4]], 
                    GoodKidMaadCity_json_data$personality[[2]][5][[1]][[1]][[4]], 
                    ToPimpAButterfly_json_data$personality[[2]][5][[1]][[1]][[4]], 
                    Damn_json_data$personality[[2]][5][[1]][[1]][[4]],
                    Untitled_json_data$personality[[2]][5][[1]][[1]][[4]])

Cautiousness<- c(Section80_json_data$personality[[2]][5][[1]][[2]][[4]], 
                      GoodKidMaadCity_json_data$personality[[2]][5][[1]][[2]][[4]], 
                      ToPimpAButterfly_json_data$personality[[2]][5][[1]][[2]][[4]], 
                      Damn_json_data$personality[[2]][5][[1]][[2]][[4]],
                 Untitled_json_data$personality[[2]][5][[1]][[2]][[4]])

Dutifulness<- c(Section80_json_data$personality[[2]][5][[1]][[3]][[4]], 
                 GoodKidMaadCity_json_data$personality[[2]][5][[1]][[3]][[4]], 
                 ToPimpAButterfly_json_data$personality[[2]][5][[1]][[3]][[4]], 
                 Damn_json_data$personality[[2]][5][[1]][[3]][[4]],
                Untitled_json_data$personality[[2]][5][[1]][[3]][[4]])

Orderliness<- c(Section80_json_data$personality[[2]][5][[1]][[4]][[4]], 
                GoodKidMaadCity_json_data$personality[[2]][5][[1]][[4]][[4]], 
                ToPimpAButterfly_json_data$personality[[2]][5][[1]][[4]][[4]], 
                Damn_json_data$personality[[2]][5][[1]][[4]][[4]],
                Untitled_json_data$personality[[2]][5][[1]][[4]][[4]])


Self_Discipline<- c(Section80_json_data$personality[[2]][5][[1]][[5]][[4]], 
              GoodKidMaadCity_json_data$personality[[2]][5][[1]][[5]][[4]], 
              ToPimpAButterfly_json_data$personality[[2]][5][[1]][[5]][[4]], 
              Damn_json_data$personality[[2]][5][[1]][[5]][[4]],
              Untitled_json_data$personality[[2]][5][[1]][[5]][[4]])

Self_Efficacy<- c(Section80_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  GoodKidMaadCity_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  ToPimpAButterfly_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  Damn_json_data$personality[[2]][5][[1]][[6]][[4]],
                  Untitled_json_data$personality[[2]][5][[1]][[6]][[4]])

Kendrick_Conscientiousness<- data.frame(Album_name, Achievement_Striving, Cautiousness, Dutifulness, Orderliness,
                                        Self_Discipline, Self_Efficacy)

Kendrick_Conscientiousness.long<- melt(Kendrick_Conscientiousness)

ggplot(Kendrick_Conscientiousness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Altruism<- c(Section80_json_data$personality[[4]][5][[1]][[1]][[4]], 
            GoodKidMaadCity_json_data$personality[[4]][5][[1]][[1]][[4]], 
            ToPimpAButterfly_json_data$personality[[4]][5][[1]][[1]][[4]], 
            Damn_json_data$personality[[4]][5][[1]][[1]][[4]],
            Untitled_json_data$personality[[4]][5][[1]][[1]][[4]])

Cooperation<- c(Section80_json_data$personality[[4]][5][[1]][[2]][[4]], 
                 GoodKidMaadCity_json_data$personality[[4]][5][[1]][[2]][[4]], 
                 ToPimpAButterfly_json_data$personality[[4]][5][[1]][[2]][[4]], 
                 Damn_json_data$personality[[4]][5][[1]][[2]][[4]],
                Untitled_json_data$personality[[4]][5][[1]][[2]][[4]])

Modesty<- c(Section80_json_data$personality[[4]][5][[1]][[3]][[4]], 
            GoodKidMaadCity_json_data$personality[[4]][5][[1]][[3]][[4]], 
            ToPimpAButterfly_json_data$personality[[4]][5][[1]][[3]][[4]], 
            Damn_json_data$personality[[4]][5][[1]][[3]][[4]],
            Untitled_json_data$personality[[4]][5][[1]][[3]][[4]])

Uncomprimising<- c(Section80_json_data$personality[[4]][5][[1]][[4]][[4]], 
                GoodKidMaadCity_json_data$personality[[4]][5][[1]][[4]][[4]], 
                ToPimpAButterfly_json_data$personality[[4]][5][[1]][[4]][[4]], 
                Damn_json_data$personality[[4]][5][[1]][[4]][[4]],
                Untitled_json_data$personality[[4]][5][[1]][[4]][[4]])


Sympathy<- c(Section80_json_data$personality[[4]][5][[1]][[5]][[4]], 
             GoodKidMaadCity_json_data$personality[[4]][5][[1]][[5]][[4]], 
             ToPimpAButterfly_json_data$personality[[4]][5][[1]][[5]][[4]], 
             Damn_json_data$personality[[4]][5][[1]][[5]][[4]],
             Untitled_json_data$personality[[4]][5][[1]][[5]][[4]])

Trust<- c(Section80_json_data$personality[[4]][5][[1]][[6]][[4]], 
          GoodKidMaadCity_json_data$personality[[4]][5][[1]][[6]][[4]], 
          ToPimpAButterfly_json_data$personality[[4]][5][[1]][[6]][[4]], 
          Damn_json_data$personality[[4]][5][[1]][[6]][[4]],
          Untitled_json_data$personality[[4]][5][[1]][[6]][[4]])

Kendrick_Agreeableness<- data.frame(Album_name, Altruism, Cooperation, Modesty, 
                                    Uncomprimising, Sympathy, Trust)

Kendrick_Agreeableness.long<- melt(Kendrick_Agreeableness)

ggplot(Kendrick_Agreeableness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Fiery<- c(Section80_json_data$personality[[5]][5][[1]][[1]][[4]], 
          GoodKidMaadCity_json_data$personality[[5]][5][[1]][[1]][[4]], 
          ToPimpAButterfly_json_data$personality[[5]][5][[1]][[1]][[4]], 
          Damn_json_data$personality[[5]][5][[1]][[1]][[4]],
          Untitled_json_data$personality[[5]][5][[1]][[1]][[4]])

Prone_to_Worry<- c(Section80_json_data$personality[[5]][5][[1]][[2]][[4]], 
                GoodKidMaadCity_json_data$personality[[5]][5][[1]][[2]][[4]], 
                ToPimpAButterfly_json_data$personality[[5]][5][[1]][[2]][[4]], 
                Damn_json_data$personality[[5]][5][[1]][[2]][[4]],
                Untitled_json_data$personality[[5]][5][[1]][[2]][[4]])

Melancholy<- c(Section80_json_data$personality[[5]][5][[1]][[3]][[4]], 
            GoodKidMaadCity_json_data$personality[[5]][5][[1]][[3]][[4]], 
            ToPimpAButterfly_json_data$personality[[5]][5][[1]][[3]][[4]], 
            Damn_json_data$personality[[5]][5][[1]][[3]][[4]],
            Untitled_json_data$personality[[5]][5][[1]][[3]][[4]])

Immoderation<- c(Section80_json_data$personality[[5]][5][[1]][[4]][[4]], 
                   GoodKidMaadCity_json_data$personality[[5]][5][[1]][[4]][[4]], 
                   ToPimpAButterfly_json_data$personality[[5]][5][[1]][[4]][[4]], 
                   Damn_json_data$personality[[5]][5][[1]][[4]][[4]],
                 Untitled_json_data$personality[[5]][5][[1]][[4]][[4]])


Self_Consciousness<- c(Section80_json_data$personality[[5]][5][[1]][[5]][[4]], 
             GoodKidMaadCity_json_data$personality[[5]][5][[1]][[5]][[4]], 
             ToPimpAButterfly_json_data$personality[[5]][5][[1]][[5]][[4]], 
             Damn_json_data$personality[[5]][5][[1]][[5]][[4]],
             Untitled_json_data$personality[[5]][5][[1]][[5]][[4]])

Stress_Sensitivity<- c(Section80_json_data$personality[[5]][5][[1]][[6]][[4]], 
          GoodKidMaadCity_json_data$personality[[5]][5][[1]][[6]][[4]], 
          ToPimpAButterfly_json_data$personality[[5]][5][[1]][[6]][[4]], 
          Damn_json_data$personality[[5]][5][[1]][[6]][[4]],
          Untitled_json_data$personality[[5]][5][[1]][[6]][[4]])

Kendrick_Neuroticism<- data.frame(Album_name, Fiery, Prone_to_Worry, Melancholy,
                                  Immoderation, Self_Consciousness, Stress_Sensitivity)

Kendrick_Neuroticism.long<- melt(Kendrick_Neuroticism)

ggplot(Kendrick_Neuroticism.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Activity<- c(Section80_json_data$personality[[3]][5][[1]][[1]][[4]], 
          GoodKidMaadCity_json_data$personality[[3]][5][[1]][[1]][[4]], 
          ToPimpAButterfly_json_data$personality[[3]][5][[1]][[1]][[4]], 
          Damn_json_data$personality[[3]][5][[1]][[1]][[4]],
          Untitled_json_data$personality[[3]][5][[1]][[1]][[4]])

Assertive<- c(Section80_json_data$personality[[3]][5][[1]][[2]][[4]], 
              GoodKidMaadCity_json_data$personality[[3]][5][[1]][[2]][[4]], 
              ToPimpAButterfly_json_data$personality[[3]][5][[1]][[2]][[4]], 
              Damn_json_data$personality[[3]][5][[1]][[2]][[4]],
              Untitled_json_data$personality[[3]][5][[1]][[2]][[4]])

Cheerful<- c(Section80_json_data$personality[[3]][5][[1]][[3]][[4]], 
             GoodKidMaadCity_json_data$personality[[3]][5][[1]][[3]][[4]], 
             ToPimpAButterfly_json_data$personality[[3]][5][[1]][[3]][[4]], 
             Damn_json_data$personality[[3]][5][[1]][[3]][[4]],
             Untitled_json_data$personality[[3]][5][[1]][[3]][[4]])

Thrill_Seeking<- c(Section80_json_data$personality[[3]][5][[1]][[4]][[4]], 
                 GoodKidMaadCity_json_data$personality[[3]][5][[1]][[4]][[4]], 
                 ToPimpAButterfly_json_data$personality[[3]][5][[1]][[4]][[4]], 
                 Damn_json_data$personality[[3]][5][[1]][[4]][[4]],
                 Untitled_json_data$personality[[3]][5][[1]][[4]][[4]])


Outgoing<- c(Section80_json_data$personality[[3]][5][[1]][[5]][[4]], 
                       GoodKidMaadCity_json_data$personality[[3]][5][[1]][[5]][[4]], 
                       ToPimpAButterfly_json_data$personality[[3]][5][[1]][[5]][[4]], 
                       Damn_json_data$personality[[3]][5][[1]][[5]][[4]],
             Untitled_json_data$personality[[3]][5][[1]][[5]][[4]])

Gregariousness<- c(Section80_json_data$personality[[3]][5][[1]][[6]][[4]], 
                       GoodKidMaadCity_json_data$personality[[3]][5][[1]][[6]][[4]], 
                       ToPimpAButterfly_json_data$personality[[3]][5][[1]][[6]][[4]], 
                       Damn_json_data$personality[[3]][5][[1]][[6]][[4]],
                   Untitled_json_data$personality[[3]][5][[1]][[6]][[4]])


Kendrick_Extraversion<- data.frame(Album_name, Activity, Assertive, Cheerful,
                                  Thrill_Seeking, Outgoing, Gregariousness)

Kendrick_Extraversion.long<- melt(Kendrick_Extraversion)

ggplot(Kendrick_Extraversion.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

