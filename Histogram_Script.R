# Libraries

library(tidyverse)
library(ggrepel)

# Building dataset

dataset <- data.frame(Age, Gender, ContactType)

# Following was for the dataset originally used - filtering for direct patient contact
# types only:

direct_type <- c(
  "Case conference",
  "Face to face consultation",
  "Face to face consultation with relative/carer",
  "First attendance face to face",
  "Follow up attendance face to face",
  "Group consultation",
  "Multidisciplinary team meeting with patient",
  "Telephone consultation")

contacts <- dataset %>%
  filter(ContactType %in% direct_type)

# Average age by gender
Avg_Age_Gender <- contacts %>%
  group_by(Gender) %>%
  mutate(
    AvgAge = round(mean(Age), digits = 0)
  ) %>%
  select(Gender, AvgAge) %>%
  unique()

# The histogram - showing 5 year age groups and gender

ggplot(contacts, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, alpha = .5, position = "identity")+
  xlab("Age - Binwidth: 5") +
  ylab("Count") +
  geom_vline(data = Avg_Age_Gender, aes(xintercept = AvgAge, colour = Gender),
             linetype = "dashed", size = 1) +
  geom_label_repel(aes(label = AvgAge, x  =AvgAge, y =AvgAge), data = Avg_Age_Gender,
                   inherit.aes = FALSE, min.segment.length = 1) +
  theme(legend.position = "bottom") +
  labs(
    title = "Distribution of Activity by Age Band (5 Year Groupings)",
    subtitle = "Direct Contacts Only",
    caption = "Dashed Line: Average Age"
  ) +
  theme_minimal()
