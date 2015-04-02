#==============================================================================#
# Extract data for wp25_female_child

path_location <- "K:/whr/work/research/basic_data_extract/cps/"


# Preample ----------------------------------------------------------------

library(dplyr)


# Variables ---------------------------------------------------------------

vars <- c(
  "ph_seq",         # Household seq number
  "pppos",          # Record type and sequence indicator
  "precord",        # Person record
  "a_ernlwt",       # Earnings/not in labor force weight
  "marsupwt",       # March supplement final weight
  "h_hhtype",       # Household type
  "h_numper",       # Number of poeple in the 
  "a_age", 
  "a_dtocc",        # Detailed occupation recode
  "a_hga",          # Educational attainment
  "a_ftlf",         # Full-time civilian labor force status
  "a_maritl",       # Marital status
  "a_sex", 
  "ptotval",        # Total income from all sources
  "pearnval"       # Total earnings from all sources
)


# Connection ---------------------------------------------------------------

db_conn <- src_sqlite(path = paste0(path_location, "data/cps-asec.db"))


# Extract and Save ---------------------------------------------------------
years <- c(2014:1996, 1994:1992)
for(i in years){
  if(i >= 2005){ # No Replication weights before 2004
    cps <- tbl(db_conn, paste0("asec", i)) %>% 
      select(one_of(vars), pwwgt0:pwwgt160) %>% 
      collect()
  }else{
    if(i >= 1992){
      # 1992 no rep weights, but has the same variables as 2005 plus
      cps <- tbl(db_conn, paste0("asec", i)) %>% 
        select(one_of(vars)) %>% 
        collect()
    }else{
      # 1990 and before needs a different education variable: schl1
      cps <- tbl(db_conn, paste0("asec", i)) %>% 
        select(one_of(c(vars, "schl1"))) %>% 
        collect()
    }
  }
  save(cps, file = paste0("data/gather-data-cps-", i, ".RData"))
}


