# DEPENDENCIES
library(tidyverse)

# INGEST DATA ####
path.file <- "https://raw.githubusercontent.com/tikisen/6372_proj2/master/Data/breast-cancer-wisconsin-data.csv"

#bruce's original
#entire.dataset <- read.delim(path.file, sep = ",", header = TRUE, dec = ".")

#corrected to read from the web
entire.dataset <- read.csv(path.file, sep = ",",  row.names = NULL, stringsAsFactors = FALSE, header = TRUE)

rm(list = c("path.file"))


entire.dataset <- entire.dataset %>% mutate(CancerState = case_when(Class == 2 ~ "Benign",
                                                                    Class == 4 ~ "Malignanat"))

# CREATE TRAINING AND TEST SET ####
set.seed(12345) #to get repeatable data

data.train <- sample_frac(entire.dataset, 0.7, replace = FALSE)

train.index <- as.numeric(rownames(data.train))
data.test <- entire.dataset[-train.index,]

rm(train.index)


# SUMMARY STATISTICS ####
entire.dataset %>% select(c(2:9)) %>% summary() 

# PLOTTING ####
filepath = "/Users/bgranger/Documents/SMU/6372_Applied Statistics/MSDS6372/Applied_Stats_Project_2/Proj2_Doc/6372_proj2/R_Output"

# TOTAL COUNT OF CANCER TYPE ####
ggplot(data=entire.dataset, aes(x=CancerState, colour = CancerState)) +
  geom_bar() +
  geom_text(stat='Count', aes(label=..count..), vjust = 10) +
  theme(legend.position = "none") +
  ggtitle("Total Count of Cancer Type")
ggsave("CountOFCancerTpye.png", plot = last_plot(), path = filepath, width = 10, height = 15, units = "cm")

# PERCENT OF TOTAL CANCER TYPE ####
entire.dataset.percent <- entire.dataset %>% 
  count(CancerState) %>% 
  mutate(perc = n / nrow(entire.dataset))

ggplot(data=entire.dataset.percent, aes(x = CancerState, y = perc, colour = CancerState)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label=round(perc*100,2)), vjust = 10) +
  theme(legend.position = "none") +
  ggtitle("Percent of Cancer Type")
ggsave("PercentOFCancerTpye.png", plot = last_plot(), path = filepath, width = 10, height = 15, units = "cm")

rm(entire.dataset.percent)

# PAIRS ####
ed.small <- entire.dataset %>% select(-c(1,12))
cols <- character(nrow(ed.small))
cols[] <- "black"
cols[ed.small$Class == 2] <- "blue"
cols[ed.small$Class == 4] <- "red"
pairs(ed.small, col=cols, main = "Pairs Plot of Wisconsin Diagnostic Breast Cancer")
rm(cols)

ed.small$Bare_Nuclei <- as.integer(ed.small$Bare_Nuclei)
# the alpha argument in rgb() lets you set the transparency
cols2 = c(rgb(red=255, green=0, blue=0, alpha=50, maxColorValue=255), rgb(red=0, green=0, blue=255, alpha=50, maxColorValue=255))
cols2 = ifelse(ed.small$Class==2, cols2[1], cols2[2])

# here we jitter the data
set.seed(6141)  # this makes the example exactly reproducible
jbreast = apply(ed.small[,1:9], 2, FUN=function(x){ jitter(x, amount=.5) })
jbreast = cbind(jbreast, class=ed.small[,10])  # the class variable is not jittered

windows()  # the 1st 5 variables, using pch=16
pairs(jbreast[,1:5], col=cols2, pch=16)

windows()  # the 2nd 5 variables
pairs(jbreast[,6:10], col=cols2, pch=16, main = "Pairs Plot of Wisconsin Diagnostic Breast Cancer")

windows()  # to match up the 1st & 2nd sets requires more coding
layout(matrix(1:25, nrow=5, byrow=T))
par(mar=c(.5,.5,.5,.5), oma=c(2,2,2,2))

for(i in 1:5){
   for(j in 6:10){
    
    plot(jbreast[,j], jbreast[,i], col=cols2, pch=16,
         
         axes=F, main="", xlab="", ylab="")
    
    box()
    
    if(j==6 ){ mtext(colnames(jbreast)[i], side=2, cex=.7, line=1) }
    
    if(i==5 ){ mtext(colnames(jbreast)[j], side=1, cex=.7, line=1) }
    
    if(j==10){ axis(side=4, seq(2,10,2), cex.axis=.8) }
    
    if(i==1 ){ axis(side=3, seq(2,10,2), cex.axis=.8) }
    
  }
  
}







