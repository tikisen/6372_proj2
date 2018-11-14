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