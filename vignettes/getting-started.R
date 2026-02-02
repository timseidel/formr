## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# if (!requireNamespace("remotes")) install.packages("remotes")
# remotes::install_github("rubenarslan/formr")

## ----setup2, eval = FALSE-----------------------------------------------------
# library(formr)

## ----store_keys, eval = FALSE-------------------------------------------------
# # Store your credentials once
# # This saves them securely in your OS credential store
# formr_store_keys(
#   host = "https://api.formr.org",
#   client_id = "YOUR_CLIENT_ID",
#   client_secret = "YOUR_CLIENT_SECRET"
# )

## ----store_keys_classic, eval = FALSE-----------------------------------------
# # Store your email/password under a shorthand name (e.g. "main_account")
# formr_store_keys("main_account")

## ----auth_local, eval = FALSE-------------------------------------------------
# # Automatically finds your stored keys
# formr_api_authenticate(host = "https://api.formr.org") # or your custom URL!

## ----connect_classic, eval = FALSE--------------------------------------------
# # Connect using the stored credentials
# formr_connect("main_account")

## ----auth_run, eval = FALSE---------------------------------------------------
# # Inside a formr Run, simply call:
# formr_api_authenticate()

## ----push_pull, eval = FALSE--------------------------------------------------
# # Download a project (surveys and files) to your local folder
# formr_api_pull_project("daily_diary")
# 
# # Upload changes back to the server
# formr_api_push_project("daily_diary")

## ----results, eval = FALSE----------------------------------------------------
# # Fetch and process
# df <- formr_api_results("daily_diary")

## ----logout, eval = FALSE-----------------------------------------------------
# formr_api_logout()

