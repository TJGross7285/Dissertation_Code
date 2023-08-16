library(readr)
library(tidytext)
library(dplyr)
library(Matrix)






final_frame<-read_lines("segment21.txt")[-c(1)]%>%as.data.frame()
colnames(final_frame)[1]<-"Text"

final_frame$Text<-as.character(finalframe$Text)
final_frame_lines<-final_frame%>%unnest_tokens(word,Text)%>%count(word, sort = TRUE)
final_frame_words<-final_frame_lines%>%unnest_tokens(word, line, token = "words")%>%count(word,id, sort = TRUE)%>%bind_tf_idf(word, id, n)%>%cast_sparse(id,word, tf_idf)

write.csv(final_frame_words,file="fggfhj.csv")