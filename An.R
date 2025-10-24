library(tidyverse)
library(GGally)
library(ggplot2)
library(viridisLite)

df <- read_csv("ADW.csv")
df <- df |> 
  na.omit()
df <- df |> 
  mutate(year = as.factor(year))

df |> 
  summary()

ggpairs(
  df,
  aes(colour = year),
  columns = 1:18
  ) +
  labs(title = "ADWデータの散布図行列")

df_long <- df |> 
  pivot_longer(
    cols = -year,
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(df_long, aes(x = year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +
  labs(title = "変数ごとの推移", x = "Year", y = "Value") +
  scale_color_viridis_d()


vars_large <- c("revenue", "ord_inc", "net_inc", "total_ass", "net_ass", "ope_cf", "inv_cf", "fin_cf", "cash")
vars_medium <- c("BPS", "EPS", "price", "div")
vars_small <- c("equity_rat", "ROE", "PER")

# フィルタして描画例
df_long %>% filter(Variable %in% vars_large) %>% 
  ggplot(aes(x = year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +
  labs(title = "大スケール変数の推移", x = "Year", y = "Value")

df_long %>% filter(Variable %in% vars_medium) %>% 
  ggplot(aes(x = year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +
  labs(title = "中スケール変数の推移", x = "Year", y = "Value")

df_long %>% filter(Variable %in% vars_small) %>% 
  ggplot(aes(x = year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +
  labs(title = "小スケール変数の推移", x = "Year", y = "Value")
