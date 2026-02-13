library(dplyr)
library(tidyr)
library(haven)
library(plm)



df <- read_csv("R_panel_data.csv")
summary(df)

df_1 <- df %>% 
      arrange(Comp_name,year) %>%
      group_by(Comp_name) %>%
      mutate(group_id = cur_group_id()) %>%
      ungroup()

df_1 <- df_1 %>% select(-...1)

panel_data <- pdata.frame(df_1, index = c("group_id", "year"))
pdim(panel_data)


twfe <- plm(bank_br ~ did ,
            data = panel_data,
            model = "within",
            effect = "twoways")

summary(twfe)

coeftest(twfe, vcovHC)
coeftest(twfe, vcov = vcovHC(model, type="HC1", cluster="group"))

