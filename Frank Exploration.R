library("rjson")
library(ggplot2)
library(reshape2)

nostalgiaUltra_json_data<- fromJSON(file= 'nostalgiaUltra.json')
ChannelOrange_json_data<- fromJSON(file= 'ChannelOrange.json')
Blonde_json_data<- fromJSON(file= 'Blonde.json')




Openness<- c(nostalgiaUltra_json_data$personality[[1]][4][[1]], ChannelOrange_json_data$personality[[1]][4][[1]], 
             Blonde_json_data$personality[[1]][4][[1]])

Conscientiousness<- c(nostalgiaUltra_json_data$personality[[2]][4][[1]], ChannelOrange_json_data$personality[[2]][4][[1]], 
                      Blonde_json_data$personality[[2]][4][[1]])

Extraversion<- c(nostalgiaUltra_json_data$personality[[3]][4][[1]], ChannelOrange_json_data$personality[[3]][4][[1]], 
                 Blonde_json_data$personality[[3]][4][[1]])

Agreeableness<- c(nostalgiaUltra_json_data$personality[[4]][4][[1]], ChannelOrange_json_data$personality[[4]][4][[1]], 
                  Blonde_json_data$personality[[4]][4][[1]])

Emotional_range<- c(nostalgiaUltra_json_data$personality[[5]][4][[1]], ChannelOrange_json_data$personality[[5]][4][[1]], 
                    Blonde_json_data$personality[[5]][4][[1]])

Album_name= factor(c("nostalgia, Ultra", "Channel Orange", "Blonde"))

Album_name= factor(Album_name, levels(Album_name)[c(3,2,1)])

Frank_Big5<- data.frame(Album_name, Openness, Conscientiousness, Extraversion, Agreeableness, Emotional_range)


Frank.long<- melt(Frank_Big5)

ggplot(Frank.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Adventurousness<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    ChannelOrange_json_data$personality[[1]][5][[1]][[1]][[4]], 
                    Blonde_json_data$personality[[1]][5][[1]][[1]][[4]])

Artistic_Interest<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      ChannelOrange_json_data$personality[[1]][5][[1]][[2]][[4]], 
                      Blonde_json_data$personality[[1]][5][[1]][[2]][[4]])

Emotionality<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 ChannelOrange_json_data$personality[[1]][5][[1]][[3]][[4]], 
                 Blonde_json_data$personality[[1]][5][[1]][[3]][[4]])

Imagination<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[4]][[4]], 
                ChannelOrange_json_data$personality[[1]][5][[1]][[4]][[4]], 
                Blonde_json_data$personality[[1]][5][[1]][[4]][[4]])


Intellect<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[5]][[4]], 
              ChannelOrange_json_data$personality[[1]][5][[1]][[5]][[4]], 
              Blonde_json_data$personality[[1]][5][[1]][[5]][[4]])

Authority_Challenging<- c(nostalgiaUltra_json_data$personality[[1]][5][[1]][[6]][[4]], 
                          ChannelOrange_json_data$personality[[1]][5][[1]][[6]][[4]], 
                          Blonde_json_data$personality[[1]][5][[1]][[6]][[4]])

Frank_Openness<- data.frame(Album_name, Adventurousness, Artistic_Interest, Emotionality, Imagination, Intellect, 
                               Authority_Challenging)

Frank_Openness.long<- melt(Frank_Openness)

ggplot(Frank_Openness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")


Achievement_Striving<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         ChannelOrange_json_data$personality[[2]][5][[1]][[1]][[4]], 
                         Blonde_json_data$personality[[2]][5][[1]][[1]][[4]])

Cautiousness<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[2]][[4]], 
                 ChannelOrange_json_data$personality[[2]][5][[1]][[2]][[4]], 
                 Blonde_json_data$personality[[2]][5][[1]][[2]][[4]])

Dutifulness<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[3]][[4]], 
                ChannelOrange_json_data$personality[[2]][5][[1]][[3]][[4]], 
                Blonde_json_data$personality[[2]][5][[1]][[3]][[4]])

Orderliness<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[4]][[4]], 
                ChannelOrange_json_data$personality[[2]][5][[1]][[4]][[4]], 
                Blonde_json_data$personality[[2]][5][[1]][[4]][[4]])


Self_Discipline<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    ChannelOrange_json_data$personality[[2]][5][[1]][[5]][[4]], 
                    Blonde_json_data$personality[[2]][5][[1]][[5]][[4]])

Self_Efficacy<- c(nostalgiaUltra_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  ChannelOrange_json_data$personality[[2]][5][[1]][[6]][[4]], 
                  Blonde_json_data$personality[[2]][5][[1]][[6]][[4]])

Frank_Conscientiousness<- data.frame(Album_name, Achievement_Striving, Cautiousness, Dutifulness, Orderliness,
                                        Self_Discipline, Self_Efficacy)

Frank_Conscientiousness.long<- melt(Frank_Conscientiousness)

ggplot(Frank_Conscientiousness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Altruism<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[1]][[4]], 
             ChannelOrange_json_data$personality[[4]][5][[1]][[1]][[4]], 
             Blonde_json_data$personality[[4]][5][[1]][[1]][[4]])

Cooperation<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[2]][[4]], 
                ChannelOrange_json_data$personality[[4]][5][[1]][[2]][[4]], 
                Blonde_json_data$personality[[4]][5][[1]][[2]][[4]])

Modesty<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[3]][[4]], 
            ChannelOrange_json_data$personality[[4]][5][[1]][[3]][[4]], 
            Blonde_json_data$personality[[4]][5][[1]][[3]][[4]])

Uncomprimising<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[4]][[4]], 
                   ChannelOrange_json_data$personality[[4]][5][[1]][[4]][[4]], 
                   Blonde_json_data$personality[[4]][5][[1]][[4]][[4]])


Sympathy<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[5]][[4]], 
             ChannelOrange_json_data$personality[[4]][5][[1]][[5]][[4]], 
             Blonde_json_data$personality[[4]][5][[1]][[5]][[4]])

Trust<- c(nostalgiaUltra_json_data$personality[[4]][5][[1]][[6]][[4]], 
          ChannelOrange_json_data$personality[[4]][5][[1]][[6]][[4]], 
          Blonde_json_data$personality[[4]][5][[1]][[6]][[4]])

Frank_Agreeableness<- data.frame(Album_name, Altruism, Cooperation, Modesty, 
                                    Uncomprimising, Sympathy, Trust)

Frank_Agreeableness.long<- melt(Frank_Agreeableness)

ggplot(Frank_Agreeableness.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Fiery<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[1]][[4]], 
          ChannelOrange_json_data$personality[[5]][5][[1]][[1]][[4]], 
          Blonde_json_data$personality[[5]][5][[1]][[1]][[4]])

Prone_to_Worry<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[2]][[4]], 
                   ChannelOrange_json_data$personality[[5]][5][[1]][[2]][[4]], 
                   Blonde_json_data$personality[[5]][5][[1]][[2]][[4]])

Melancholy<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[3]][[4]], 
               ChannelOrange_json_data$personality[[5]][5][[1]][[3]][[4]], 
               Blonde_json_data$personality[[5]][5][[1]][[3]][[4]])

Immoderation<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 ChannelOrange_json_data$personality[[5]][5][[1]][[4]][[4]], 
                 Blonde_json_data$personality[[5]][5][[1]][[4]][[4]])


Self_Consciousness<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[5]][[4]], 
                       ChannelOrange_json_data$personality[[5]][5][[1]][[5]][[4]], 
                       Blonde_json_data$personality[[5]][5][[1]][[5]][[4]])

Stress_Sensitivity<- c(nostalgiaUltra_json_data$personality[[5]][5][[1]][[6]][[4]], 
                       ChannelOrange_json_data$personality[[5]][5][[1]][[6]][[4]], 
                       Blonde_json_data$personality[[5]][5][[1]][[6]][[4]])

Frank_Neuroticism<- data.frame(Album_name, Fiery, Prone_to_Worry, Melancholy,
                                  Immoderation, Self_Consciousness, Stress_Sensitivity)

Frank_Neuroticism.long<- melt(Frank_Neuroticism)

ggplot(Frank_Neuroticism.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")

Activity<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[1]][[4]], 
             ChannelOrange_json_data$personality[[3]][5][[1]][[1]][[4]], 
             Blonde_json_data$personality[[3]][5][[1]][[1]][[4]])

Assertive<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[2]][[4]], 
              ChannelOrange_json_data$personality[[3]][5][[1]][[2]][[4]], 
              Blonde_json_data$personality[[3]][5][[1]][[2]][[4]])

Cheerful<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[3]][[4]], 
             ChannelOrange_json_data$personality[[3]][5][[1]][[3]][[4]], 
             Blonde_json_data$personality[[3]][5][[1]][[3]][[4]])

Thrill_Seeking<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   ChannelOrange_json_data$personality[[3]][5][[1]][[4]][[4]], 
                   Blonde_json_data$personality[[3]][5][[1]][[4]][[4]])


Outgoing<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[5]][[4]], 
             ChannelOrange_json_data$personality[[3]][5][[1]][[5]][[4]], 
             Blonde_json_data$personality[[3]][5][[1]][[5]][[4]])

Gregariousness<- c(nostalgiaUltra_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   ChannelOrange_json_data$personality[[3]][5][[1]][[6]][[4]], 
                   Blonde_json_data$personality[[3]][5][[1]][[6]][[4]])


Frank_Extraversion<- data.frame(Album_name, Activity, Assertive, Cheerful,
                                   Thrill_Seeking, Outgoing, Gregariousness)

Frank_Extraversion.long<- melt(Frank_Extraversion)

ggplot(Frank_Extraversion.long,aes(variable,value,fill=Album_name))+
  geom_bar(stat="identity",position="dodge")


