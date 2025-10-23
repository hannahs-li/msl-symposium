# EDA + Corr Plot of trust and covid outcomes

# Load packages
library(ggplot2)
library(ggcorrplot)
library(readr)
library(skimr)
library(viridis)


# Load data 
load(here("data/global.rda"))

# build scatter plot 
global |> 
  filter(na.rm = TRUE) |> 
  ggplot(aes(x = government, y = total_deaths_per_million)) + 
  geom_point(aes(color =  political_regime)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs( 
    title = "Trust in Government vs. Total Covid Deaths Per Million",
    x = "Trust in Government",
    y = "Total Covid deaths per million")

cor(global$government, global$total_deaths_per_million, use = "complete.obs")




global |> 
  ggplot(aes(x = government, y = total_deaths_per_million)) +
  geom_point(aes(color = government), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_color_viridis(option = "C", direction = -1) +
  labs(
    title = "Trust in Government vs. COVID-19 Deaths per Million",
    x = "Trust in Government (%)",
    y = "Total Deaths per Million",
    color = "Trust Score"
  ) +
  theme_minimal()


# corr plot 
cor_data <- global |> 
  select(government,
         total_cases_per_million,
         total_deaths_per_million) |> 
         # vaccination_rates_per_million, 
         # compliance_score
         # covid_deaths_per_100k,
         # excess_deaths_per_100k) |> 
  drop_na()  # Remove rows with NA for correlation

# Compute correlation matrix
cor_matrix <- cor(cor_data)

# Create correlation heatmap
ggcorrplot(cor_matrix,
           method = "square",
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Plot: Trust and COVID-19 Metrics",
           colors = c("#6D9EC1", "white", "#E46767"),
           tl.cex = 10)


## corr matrix 
cor_data <- global |> 
  select(where(is.numeric)) |> 
  na.omit()

cor_matrix <- cor(cor_data)

## plot 
ggcorrplot::ggcorrplot(cor_matrix, 
                       lab = TRUE, 
                       title = "Correlation Plot: Trust and COVID-19 Metrics")

