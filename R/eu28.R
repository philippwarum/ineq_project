## EU28 Main File
#################

# connect to server -------------------------------------------------------

source("./R/_connection.R")


# load data and store it locally ---------------------------------------------------------------

# Attention! Large data files (EU28). Only execute if necessary. This may take a while.
source("./R/eu28_download_data.R")


# prepare data (calculate income aggregates, ppp adjustment) ------------------------------------------------------------

source("./R/eu28_data_prep.R")


# calculate indicators ----------------------------------------------------

source("./R/eu28_indicators.R")

