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
  scale_fill_manual(breaks = c("Benign", "Malignanat"), values = c("blue", "red"))
