# libraries ----
library(tidyverse)
library(janitor)
library(skimr)
# data ----
df <- read_csv("./train.csv",
               na = c("NA"),
               col_types = cols(.default = col_character()))
# munge ----
# clean_names
df <- df |>
    clean_names() 

df <- df |>
    # string_to_lower
    mutate(across(where(is.character), str_to_lower)) 
df <- df |> 
    # as.numeric
    mutate(across(c(lot_frontage, lot_area, mas_vnr_area, bsmt_fin_sf1,
                    bsmt_fin_sf2, bsmt_unf_sf, total_bsmt_sf, x1st_flr_sf,
                    x2nd_flr_sf, low_qual_fin_sf, gr_liv_area, garage_area, 
                    wood_deck_sf, open_porch_sf, sale_price), 
                  ~ as.numeric(.))) 
# missing values----
# # alley, pool_qc, fence ----
df <- df |> 
    mutate(across(c(alley, pool_qc, fence), 
                  ~ replace_na(., "none"))) 
# # lot_frontage ----
df <- df |> 
    mutate(lot_shape = as_factor(lot_shape)) |> 
    mutate(impute_lot_frontage = case_when(lot_shape == "ir1" ~ 76.1,
                                           lot_shape == "ir2" ~ 76.5,
                                           lot_shape == "ir3" ~ 138.0,
                                           lot_shape == "reg" ~ 67.0))
df<- df |> 
    mutate(lot_frontage = coalesce(df$lot_frontage, df$impute_lot_frontage)) |> 
    select(-impute_lot_frontage)
# # misc_feature ----
df <- df |> 
    mutate(across(misc_feature, 
                  ~ replace_na(., "none")))
# # mas_vnr_area and mas_vnr_type ----
df <- df |> 
    mutate(across(mas_vnr_area,
                  ~ replace_na(., 0))) |> 
    mutate(across(mas_vnr_type,
                  ~ replace_na(., "n/a")))
# # fireplace_qu ----
df <- df |> 
    mutate(across(fireplace_qu,
                  ~ replace_na(., "n/a")))
# # garage_ type, finish, qual, cond, yr_blt ----
df <- df |> 
    mutate(across(c(garage_type,
                    garage_finish,
                    garage_qual,
                    garage_cond,
                    garage_yr_blt),
                  ~ replace_na(., "n/a")))
# # electrical ----
# the missing value appears to be a mistake since the other houses in that neighborhood built after 1946 all have sbrkr
df <- df |> 
    mutate(across(electrical,
                  ~ replace_na(., "sbrkr")))
# # bsmt_ qual, cond, exposure, fin_type1, fin_type2 ----
df <- df |> 
    mutate(across(c(bsmt_qual, bsmt_cond, bsmt_fin_type1),
                  ~ replace_na(., "no_bsmt")))
df <- df |> 
    mutate(bsmt_fin_type2 =  
               case_when(id == 333 ~ "n/a", TRUE ~ replace_na(bsmt_fin_type2, "no_bsmt")))
df <- df |> 
    mutate(bsmt_exposure = 
               case_when(id == 949 ~ "no", TRUE ~ replace_na(bsmt_exposure, "no")))
# write_csv ----
write_csv(df, "./mutated_df.csv")
