# DEVELOPMENT
# GATHER DATA ####

entire.dataset.tall <- entire.dataset %>% 
  select(-c(1)) %>%
  gather(1:10, key = "Variable", value = "Value") %>%
  arrange(Variable)

# filter(Value != "?") %>%

# ATTRIBUTE COERCION ####
entire.dataset.tall$Variable <- as.factor(entire.dataset.tall$Variable)
entire.dataset.tall$Value <- as.integer(entire.dataset.tall$Value)

# DENSITY
ggplot(data = entire.dataset.tall, aes(x=Variable)) + 
  geom_density() + 
  facet_wrap(~Variable) +
  ggtitle("Density Plot1: Variables 1 through 9") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# SCATTER
ggplot(data = entire.dataset.tall, aes(x=Variable)) + 
  geom_dotplot(position = "identity", binwidth = 60, mapping = ) + 
  geom_jitter() +
  facet_wrap(~Variable)

# IN DEVELOPMENT
ggplot(data = entire.dataset.tall, aes(x=Value, y=Variable)) + 
  geom_boxplot() +
  facet_wrap(~Variable) +
  ggtitle("BP1: Continuous Variables 1 through 9") 
# ggsave("BP1.png", plot = last_plot(), path = filepath, width = 50, height = 20,
#       units = "cm") +

# Categorical Variables

# HISTOGRAM DEVELOPMENT
# HISTOGRAM BY VARIABLE BY CLASS ####
ed.small <- entire.dataset %>% select(-c(1,11))
ed.small$Clump_Thickness <- as.integer(ed.small$Clump_Thickness)
ed.small$Uniformity_Cell_Size <- as.integer(ed.small$Uniformity_Cell_Size)
ed.small$Uniformity_Cell_Shape <- as.integer(ed.small$Uniformity_Cell_Shape)
ed.small$Marginal_Adhesion <- as.integer(ed.small$Marginal_Adhesion)
ed.small$Single_Epithelial_Cell_Size <- as.integer(ed.small$Single_Epithelial_Cell_Size)
ed.small$Bare_Nuclei <- as.integer(ed.small$Bare_Nuclei)
ed.small$Bland_Chromatin <- as.integer(ed.small$Bland_Chromatin)
ed.small$Normal_Nucleoli <- as.integer(ed.small$Normal_Nucleoli)
ed.small$Mitoses <- as.integer(ed.small$Mitoses)

# GATHER: FROM WIDE TO TALL ####
# https://stackoverflow.com/questions/14818529/plot-histograms-over-factor-variables
ed.small <- entire.dataset %>% select(-c(1,11))

ed.small$Clump_Thickness <- as.integer(ed.small$Clump_Thickness)
ed.small$Uniformity_Cell_Size <- as.integer(ed.small$Uniformity_Cell_Size)
ed.small$Uniformity_Cell_Shape <- as.integer(ed.small$Uniformity_Cell_Shape)
ed.small$Marginal_Adhesion <- as.integer(ed.small$Marginal_Adhesion)
ed.small$Single_Epithelial_Cell_Size <- as.integer(ed.small$Single_Epithelial_Cell_Size)
ed.small$Bare_Nuclei <- as.integer(ed.small$Bare_Nuclei)
ed.small$Bland_Chromatin <- as.integer(ed.small$Bland_Chromatin)
ed.small$Normal_Nucleoli <- as.integer(ed.small$Normal_Nucleoli)
ed.small$Mitoses <- as.integer(ed.small$Mitoses)


ed.tall <- ed.small %>% 
  gather(-10, key = "Variable", value = "Value") %>% 
  filter(!is.na(CancerState))

# ed.tall <- ed.tall %>% 
#   group_by(CancerState, Variable, Value) %>% 
#   summarise(Count = n()) %>%
#   arrange(Variable)


# CancerState-Variable-Value
ggplot(data = ed.tall, aes(x=Value)) +
  geom_histogram(bins=5) + 
  facet_wrap(Variable ~ CancerState, ncol = 9)


# BOXPLOT ####
ed.small$CancerState <- as.factor(ed.small$CancerState)

ed.tall <- ed.small %>% 
  gather(-10, key = "Variable", value = "Value") %>% 
  filter(!is.na(CancerState))

ggplot(ed.tall, aes(x=CancerState, y=Value, fill = CancerState)) + 
  geom_boxplot() +
  facet_wrap(~ Variable) +
  ggtitle("WDBC Boxplot ") + 
  scale_fill_manual(breaks = c("Benign", "Malignanat"), values = c("blue", "red")) +
  theme(legend.position="none")

# CORRPLOT ####
# CORRELATION PLOT
library(corrplot)

corr.data <- wdbc_data %>% select(-c(1)) 

corr.data$Clump_Thickness <- as.integer(corr.data$Clump_Thickness)
corr.data$Uniformity_Cell_Size <- as.integer(corr.data$Uniformity_Cell_Size)
corr.data$Uniformity_Cell_Shape <- as.integer(corr.data$Uniformity_Cell_Shape)
corr.data$Marginal_Adhesion <- as.integer(corr.data$Marginal_Adhesion)
corr.data$Single_Epithelial_Cell_Size <- as.integer(corr.data$Single_Epithelial_Cell_Size)
corr.data$Bare_Nuclei <- as.integer(corr.data$Bare_Nuclei)
corr.data$Bland_Chromatin <- as.integer(corr.data$Bland_Chromatin)
corr.data$Normal_Nucleoli <- as.integer(corr.data$Normal_Nucleoli)
corr.data$Mitoses <- as.integer(corr.data$Mitoses)
corr.data$Class <- as.integer(corr.data$Class)

corr.data <- corr.data %>%
  select(Clump = Clump_Thickness, Cell_Size = Uniformity_Cell_Size, 
       Cell_Shape = Uniformity_Cell_Shape, Adhesion = Marginal_Adhesion,
       Epithelial = Single_Epithelial_Cell_Size, Nuclei = Bare_Nuclei,
       Chromatin = Bland_Chromatin, Nucleoli = Normal_Nucleoli, everything())

corr <- cor(corr.data)
corrplot(corr, method ="number", type= "upper")

# IMPUTATION ####


wdbc.data %>% summary()

wdbc.data$Nuclei <- ifelse(is.na(wdbc.data$Nuclei),
                           median(wdbc.data$Nuclei, na.rm=TRUE), 
                           wdbc.data$Nuclei)

wdbc.data %>% summary()

#  proportion of classes 
table(wdbc.data$Class)

# Logistic Regression - ROC Curve ####
# https://www.youtube.com/watch?v=G_pvQYUm8Ik
# STEP: 
#   1) Divide data into 
#     1a) training set        => wbdc.train     => train
#     1b) test set            => wbdc.test      => test
#
#   2) Build a logistic model using training set
#     2a) logistic model      => wdbc.glm.fit   => mod_log 
#         wdbc.glm.fit <- glm(CancerState ~ ., data = wdbc.train, family = "binomial")
#         wdbc.glm.fit <- glm(CancerState ~ ., data = wdbc.train, family =  binomial(link = "logit"))
#
#   3) Predict the values, based off of training data set, using test data set
#     3a) predictions (probs) => glm.probs      => results_log
#         glm.probs <- predict(wdbc.glm.fit, wdbc.test, type = "response")
#
#   4) Can't use arbitrary value as the threshold, instead we use a performance threshold using the ROC
#     curve.  This is a blend of the accuracy and TP/TN vs TN/FN using library(ROCR)
#     4a) Prediction          => pred_log       => pred_log
          prediction(glm.probs, wdbc.test$Class) -> pred_log
#     4b) Performance (Performance metric: Accuracy vs Cutoff)
          performance(pred_log, "acc") -> acc
          plot(acc)
#         max acc is around 0.1, and when we build a confusion matrix using MAX ACCuracy of .09
#         table(wdbc.test$CancerState, glm.probs>0.9)
#                       FALSE TRUE
#           Benign      734   10
#           Malignant    58  316
#         Accuracy: (734+316)/(734+316+58+10) => 0.9391771
#         TP/FP Rates:
#           TP: 316 / (58+316)  => 0.8449198
#           FP: 10 / (10 + 734) => 0.01344086
#         To get a good balance of TP to FP:
            performance(pred_log, "tpr", "fpr") -> roc_curve
            plot(roc_curve)

wdbc.data.2 <-  wdbc.data %>% mutate(Class2 = case_when(Class == 2 ~ 0,
                                                        Class == 4 ~ 1)) %>%
  select(-Class) %>% select(Clump, Cell_Size, Cell_Shape, Adhesion, Epithelial, Nuclei,
                            Chromatin, Nucleoli, Mitoses, Class = Class2)

wdbc.data.2$Class <- as.factor(wdbc.data.2$Class)

# Model Selection Approaches
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/ozone2.csv", stringsAsFactors=F)
response_df <- inputData['ozone_reading']  # Y variable
predictors_df <- inputData[, !names(inputData) %in% "ozone_reading" ]  # X variables

lmMod <- lm(ozone_reading ~ . , data = inputData)
selectedMod <- step(lmMod)

all_vifs <- car::vif(selectedMod)
print(all_vifs)

# ---------------

wdbc.data.2 <- wdbc.data %>% select(-ID)

lmMod_2 <- lm(Class ~ . , data = wdbc.data.2)
selectedMod_2 <- step(lmMod_2, direction = "forward")

all_vifs_2 <- car::vif(selectedMod_2)
print(all_vifs_2)

forward <-data.frame(names(all_vifs_2))

# -----------------
ed.small <- wdbc.sub %>% select(-c(1,2))
corr <- cor(ed.small)
corrplot(corr, method ="number", type= "upper")
corrplot(corr, method ="circle", type= "upper")
rm(corr)
# ----------------- CORRELATION MAXTIX


# -----------------
glm_fits = glm(diagnosis ~.-ID_number,data=wdbc_sub,family="binomial")
car::vif(glm_fits)

# -----------------
par(mfrow = c(1, 2))
autoplot(glm_fits, which = 1:6, ncol = 3, label.size = 3, colour = 'diagnosis')
#--------
plot(hatvalues(glm_fits))
which.max(hatvalues(glm_fits))

#------------
wdbc.sub_Not153 <- wdbc.sub[-153,] 
