# libraries ----
library(tidyverse)
library(janitor)
library(skimr)
# data ----
df <- read_csv("./train.csv",
               na = c("NA"))
# munge 
df <- df |>
    clean_names() # janitor

df <- df |> 
    mutate(
        across(everything(), as.character)) |>
    mutate(
        across(where(is.character), str_to_lower))
# numeric -----------------------------------------------------------------

df <- df |> 
    mutate(
        across(c(lot_frontage, lot_area, mas_vnr_area, bsmt_fin_sf1,
                 bsmt_fin_sf2, bsmt_unf_sf, total_bsmt_sf, 
                 x1st_flr_sf, x2nd_flr_sf, low_qual_fin_sf, gr_liv_area, 
                 garage_area, wood_deck_sf, open_porch_sf, enclosed_porch,
                 x3ssn_porch, screen_porch, pool_area, misc_val, sale_price,
                 year_built, year_remod_add), 
                  ~ as.numeric(.)))


# missing_values ----------------------------------------------------------

# # lot_frontage ----
# table for average values
# average_lot_frontage_by_lot_shape <- df |>
#     filter(!is.na(lot_frontage)) |>
#     group_by(lot_shape) |>
#     summarise(mean_lot_frontage = round(mean(lot_frontage),1))
df <- df |>  
    mutate(impute_lot_frontage = case_when(lot_shape == "ir1" ~ 76.1,
                                           lot_shape == "ir2" ~ 76.5,
                                           lot_shape == "ir3" ~ 138.4,
                                           lot_shape == "reg" ~ 67.0)) 
df <- df |> 
    mutate(lot_frontage = coalesce(df$lot_frontage, df$impute_lot_frontage)) |> 
    select(-impute_lot_frontage) 
# # mas_vnr_ area, type ----
df <- df |> 
    mutate(across(mas_vnr_type, ~ replace_na(., "unknown")))
# impute missing mas_vnr_area with mean of non-missing values
df <- df |> 
    mutate(mas_vnr_area = case_when(
        mas_vnr_type == "unknown" ~ mean(df$mas_vnr_area, na.rm = TRUE),
        TRUE ~ mas_vnr_area))

## bsmt ----
df <- df |> 
    mutate(bsmt_qual = case_when(
        total_bsmt_sf == 0 & is.na(bsmt_qual) ~ "no basement",
        TRUE ~ bsmt_qual
    )) |> 
    mutate(bsmt_cond = case_when(
        total_bsmt_sf == 0 & is.na(bsmt_cond) ~ "no basement",
        TRUE ~ bsmt_cond
    )) |> 
    mutate(bsmt_fin_type1 = case_when(
        total_bsmt_sf == 0 & is.na(bsmt_fin_type1) ~ "no basement",
        TRUE ~ bsmt_fin_type1
    )) |> 
    mutate(bsmt_exposure = case_when(
        total_bsmt_sf == 0 & is.na(bsmt_exposure) ~ "no basement",
        total_bsmt_sf > 0 & is.na(bsmt_exposure) ~ "unknown",
        TRUE ~ bsmt_exposure
    )) |> 
    mutate(bsmt_fin_type2 = case_when(
        total_bsmt_sf == 0 & is.na(bsmt_fin_type2) & !is.na(bsmt_fin_type1) ~ "no basement",
        total_bsmt_sf > 0 & bsmt_fin_sf2 > 0 ~ "unknown", # if there is no type 2 sf
        TRUE ~ bsmt_fin_type2
    )) 

# alley_fence_misc_feature ------------------------------------------------

# values where na is none
df <- df |> 
    mutate(across(c(alley, fence, misc_feature), 
                  ~ replace_na(., "none")))
# fireplace_qu ----
# x <- df |> 
#     filter(fireplaces == 0) |> 
#     select(fireplace_qu) |> 
#     distinct()
# after checking that fireplaces = 0, all fireplace_qu are na
df <- df |> 
    mutate(across(fireplace_qu, ~ replace_na(., "none")))
# electrical ----
# the missing value appears to be a mistake since the other houses in that neighborhood built after 1946 all have sbrkr
df <- df |> 
    mutate(across(electrical,
                  ~ replace_na(., "sbrkr")))
# utilities ----
# utilities has 4 levels, but all in df are allpub except one nosewa
df <- df |> 
    mutate(utilities = fct_lump_n(utilities, 
                                  n = 74, # one more than 5% of total
                                  other_level = "pub_missing")) |> 
    mutate(utilities = as.character(utilities))
# pool_qc ----
# df |> filter(pool_area == 0) |> count(pool_qc) # all na pool_qc have a pool_area = 0
df <- df |> 
    mutate(across(pool_qc,
                  ~ replace_na(., "no pool")))
# garage ----
df <- df |> 
    mutate(across(c(garage_type, garage_finish, garage_qual, garage_cond),
                  ~ replace_na(., "no garage"))) |> 
    mutate(has_garage = case_when(
        garage_area > 0 ~ "has_garage",
        garage_area == 0 ~ "no_garage")) 
# garage_yr_blt missing values
# new column determining the type of garage structure:
# original = garage was built the same year as the house
# add_on = garage was built after the house
# no_garage = no existing garage
df <- df |> 
    mutate(
        garage_str = 
               case_when(
                   year_built == garage_yr_blt ~ "original", 
                   year_built != garage_yr_blt ~ "add_on", 
                   is.na(garage_yr_blt) ~ "no_garage"
        ))
# unselect garage_yr_blt
df <- df |>
    select(-garage_yr_blt)

# neighborhood ------------------------------------------------------------


df <- df |> # combine two lowest rates 
    mutate(neighborhood = case_when(
        neighborhood == "Blueste" ~ "NPk_Blue",
        neighborhood == "NPkVill" ~ "NPk_Blue",
        TRUE ~ neighborhood
    ))

# has_remod ---------------------------------------------------------------

df <- df|> 
    mutate(has_remod = case_when(
        year_built - year_remod_add == 0 ~ "no",
        TRUE ~ replace_na("yes"))) |>
    select(-year_remod_add) 


# has_porch ---------------------------------------------------------------

df <- df |> 
    mutate(has_porch = if_else(
        (open_porch_sf > 0 | enclosed_porch > 0 | x3ssn_porch > 0 | screen_porch > 0),
        "yes", "no", missing = NULL
    ))
df <- df |> 
    select(-c(contains("porch")))

# has_deck ----------------------------------------------------------------

df <- df |> 
    mutate(has_deck = if_else(
        wood_deck_sf > 0, "yes", "no", missing = NULL
    ))
df <- df |> 
    select(-wood_deck_sf)

# has_pool ----------------------------------------------------------------

df <- df |> 
    mutate(has_pool = if_else(
        pool_area > 0, "yes", "no", missing = NULL)) |> 
    select(-pool_area)
