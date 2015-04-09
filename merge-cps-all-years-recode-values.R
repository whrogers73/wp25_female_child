

# ID Number ---------------------------------------------------------------

cps <- transform(cps, id_person = paste(ph_seq, pppos, sep = "-"))


# Recode Household Type ---------------------------------------------------
# Recode the household to account for the marrital staus
# 0 = Other; 1 = never married and lives alone; 2 = married lives with spouse
# h_numper == 1, one person in the household
cps <- transform(
  cps, 
  hh_rcd = factor(
    ifelse(
      h_numper == 1 & a_maritl == 7, "alone never married", 
      ifelse(a_maritl %in% 1:2, "married", "other")
      )
    )
  )


# Recode Age --------------------------------------------------------------
# Recode the age category to match Anne's groupings
cps <- transform(
  cps, 
  age_rcd = factor(ifelse(
    a_age >= 18 & a_age < 25, "18-24",
    ifelse(
      a_age >= 25 & a_age < 35, "25-34", 
      ifelse(
        a_age >= 35 & a_age < 45, "35-44", 
        ifelse(
          a_age >= 45 & a_age < 55, "45-54",
          ifelse(
            a_age >= 55 & a_age < 65, "55-64",
            "65+"
          )
        )
      )
    )
  )
))


# Recode Education --------------------------------------------------------
# Adjust the definitions to match what I want.
if(year == 1990){
  cps <- transform(
    cps,
    edu_rcd = ifelse(
      schl1 == 6, "high school", 
      ifelse(
        schl1 == 7, "some college", 
        ifelse(
          schl1 == 8, "college", 
          ifelse(schl1 > 8, "college plus", "other")
        )
      )
    )
  ) 
}

if(year > 1991){
  cps <- transform(
    cps,
    edu_rcd = 
      factor(
        ifelse(
          a_hga == 39, "high school", 
          ifelse(
            a_hga > 39 & a_hga < 43, "some college", 
            ifelse(
              a_hga == 44, "college", 
              ifelse(
                a_hga > 44, "college plus", "other"
                )))), 
        levels = c("other", "high school", "some college", "college", "college plus")
        )
    ) 
}

