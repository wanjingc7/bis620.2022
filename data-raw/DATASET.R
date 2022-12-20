## code to prepare `DATASET` dataset goes here

ukb_accel <- readRDS("accel.rds")
usethis::use_data(ukb_accel, overwrite = TRUE)


library(dplyr)
library(purrr)
library(haven)
folfox_path <- file.path("AllProvidedFiles_265", "SAS dataset - 20050251")
ff_files <- dir(folfox_path)
ff_names <- gsub(".sas7bdat", "", ff_files)

dl <- map(file.path(folfox_path, ff_files), ~ read_sas(.x))
names(dl) <- ff_names

demo_df <- dl$demo %>%
  select(SUBJID, AGE, SEX, ATRT, B_ECOG, DIAGTYPE,
          DIAGSTAG, DSTATUS, HPV)

ae_df <- dl$ae %>%
  select(SUBJID, AETERM)

corevar_df <- dl$corevar %>%
  select(SUBJID, RACE)

death_df <- dl$death %>%
  select(SUBJID, DTHDY)

respeval_df <- dl$respeval %>%
  select(SUBJID, DOSREFDY, RSRESP, RSCONFYN)

medhist_df <- dl$medhist |>
  select(SUBJID, TRTFREQ)

full_data <- demo_df %>%
  left_join(ae_df, by = c("SUBJID" = "SUBJID")) %>%
  left_join(corevar_df, by = c("SUBJID" = "SUBJID")) %>%
  left_join(death_df, by = c("SUBJID" = "SUBJID")) %>%
  left_join(respeval_df, by = c("SUBJID" = "SUBJID")) %>%
  left_join(medhist_df, by = c("SUBJID" = "SUBJID")) %>%
  distinct() %>%
  mutate(DEATH_FLAG = ifelse(is.na(DTHDY), yes = "Alive", no = "Expired"))

usethis::use_data(full_data, overwrite = TRUE)
