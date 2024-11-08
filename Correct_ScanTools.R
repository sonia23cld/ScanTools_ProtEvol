#Correct output scantools

#Define finctions
gather_pos<- function(data) {#data is the dataframe you want to correct
  t<- data %>%
    group_by(scaff) %>%
    mutate(diff = start - lag(start, default = first(start)))
  
  pos<-which(t$diff < 0)
  return(pos)
}

correction_scantools<- function(data, pos) { #data is the dataframe you want to correct, pos is a vector with the poistions indexes to correct
  for(i in pos) {
    data$scaff[pos-1]<- data$scaff[pos-2]
  }
  return(data)
}


################
#Enter data
path<- "/Users/sonia.celestini/Desktop/PhD Project/Alyssum_2024/Results/PBE/"
results<- list.files(path, pattern = '_BPM.txt')

#import tables
library(data.table)
genes<- list()
for (i in results) {
  name<- substr(i, 1, 10)
  genes[[name]]<- fread(paste(path, i, sep=""),h=T)
}

#apply functions
pos2<- sapply(genes, function(x) {gather_pos(x)})

genes2<- list()
for(i in 1:length(genes)) {
  name<- names(genes)[i]
  genes2[[name]]<- correction_scantools(genes[[i]], pos2[[i]])
}

#Write down the corrected tables

for(i in 1:length(genes2)) {
  name<- names(genes2)[i]
  write.table(genes2[[i]], paste(path, name, "_WS1000_MS10_BPM.txt", sep=""), quote = F, sep="\t", row.names = F)
}


########################
#Just trials to check what's going on
errors<- genes[[3]] %>%
  group_by(scaff) %>%
  mutate(diff = start - lag(start, default = first(start)))

pos<-which(errors$diff < 0)

as.data.frame(errors[x[1],])
as.data.frame(errors[(x[1]-2):(x[1]+1),])

errors<- correction_scantools(genes[[3]], pos)
