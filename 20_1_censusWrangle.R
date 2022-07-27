
## add in selected census table IDS

table_ids_list <- c("B01001e1" = "pop", # total unweighted population
                    "B01001e2" = "male", # male population 
                    "B01001e7" = "age7",  # below: male age 18 to 84 and older
                    "B01001e8" = "age8", 
                    "B01001e9" = "age9", 
                    "B01001e10" = "age10", 
                    "B01001e11" = "age11", 
                    "B01001e12" = "age12", 
                    "B01001e13" = "age13", 
                    "B01001e14" = "age14", 
                    "B01001e15" = "age15", 
                    "B01001e16" = "age16", 
                    "B01001e17" = "age17", 
                    "B01001e18" = "age18", 
                    "B01001e19" = "age19", 
                    "B01001e20" = "age20", 
                    "B01001e21" = "age21", 
                    "B01001e22" = "age22", 
                    "B01001e23" = "age23", 
                    "B01001e24" = "age24", 
                    "B01001e25" = "age25", 
                    "B01001e31" = "age31",  # below: female age 18 to 84 and older, NB: 26-30 are females under 18
                    "B01001e32" = "age32", 
                    "B01001e33" = "age33", 
                    "B01001e34" = "age34", 
                    "B01001e35" = "age35", 
                    "B01001e36" = "age36", 
                    "B01001e37" = "age37", 
                    "B01001e38" = "age38", 
                    "B01001e39" = "age39", 
                    "B01001e40" = "age40", 
                    "B01001e41" = "age41", 
                    "B01001e42" = "age42", 
                    "B01001e43" = "age43", 
                    "B01001e44" = "age44", 
                    "B01001e45" = "age45", 
                    "B01001e46" = "age46", 
                    "B01001e47" = "age47", 
                    "B01001e48" = "age48", 
                    "B01001e49" = "age49", 
                    "B02001e2" = "race_w", # white population
                    "B02001e3" = "race_b", # black population
                    "B02001e4" = "race_n", # native population
                    "B02001e5" = "race_a", # asian population
                    "B03002e12" = "race_h", # hispanic population
                    "B15003e1" = "pop_25", # population 25 years and over
                    "B15003e17" = "edu_highschool", # with high school diploma
                    "B15003e18" = "edu_ged", # with ged
                    "B15003e19" = "edu_college_1year", # some college
                    "B15003e20" = "edu_college_nodegree", # 1 or more year college, no degree
                    "B15003e21" = "edu_associate", # associate degree
                    "B15003e22" = "edu_bach", # with bachelor diploma
                    "B15003e23" = "edu_masters", # with masters diploma
                    "B15003e24" = "edu_professional", # with masters diploma
                    "B15003e25" = "edu_doctorate", # with masters diploma
                    # "B17011e1" = "poverty", # families with income in past 12 months below poverty level
                    "B11001e2" = "hh_family", # family households
                    "B11001e3" = "hh_married", # married-couple households
                    "B11001e5" = "hh_singleMale", 
                    "B11001e6" = "hh_singleFemale", 
                    "B11001e8" = "hh_livingAlone", 
                    "B19013e1" = "median_hh_inc", # median hh inc, 
                    "B19001e2" = "hh_inc_10", 
                    "B19001e3" = "hh_inc_15", 
                    "B19001e4" = "hh_inc_20", 
                    "B19001e5" = "hh_inc_25", 
                    "B19001e6" = "hh_inc_30", 
                    "B19001e7" = "hh_inc_35", 
                    "B19001e8" = "hh_inc_40", 
                    "B19001e9" = "hh_inc_45", 
                    "B19001e10" = "hh_inc_50", 
                    "B19001e11" = "hh_inc_60", 
                    "B19001e12" = "hh_inc_75", 
                    "B19001e13" = "hh_inc_100", 
                    "B19001e14" = "hh_inc_125", 
                    "B19001e15" = "hh_inc_150", 
                    "B19001e16" = "hh_inc_200", 
                    "B19001e17" = "hh_inc_200inf", 
                    "B19055e1" = "hhs", # total households
                    "B19055e2" = "socialSecurity", # households with social security
                    "B19057e2" = "publicAssistance", # households with public assistance
                    "B22010e2" = "foodStamps",  # hhs received food stamps / snap in past 12 months
                    "B23025e3" = "laborForce", # population 16 years and over
                    "B23025e5" = "unemployed", # population 16 years and over
                    "B23025e6" = "armedForces", # population 16 years and over
                    "B25003e1" = "housing_total", # total occupied housing units
                    "B25003e3" = "housing_renter",  # renter occupied housing units
                    "B27010e17" = "health0", # no health insurance, under 18
                    "B27010e33" = "health1", # no health insurance 18-34
                    "B27010e50" = "health2", # no health insurance, 34-64
                    "B27010e66" = "health3" # no health insurance, above 64
                    
)
# "B99051e5" = "foreignBorn", 
# "B99082e2" = "vehicle", 
# "B99084e5" = "workAtHome", 

# fetch data with description and id from list
table_ids <- data.table("description" = table_ids_list, id = names(table_ids_list))
table_ids[, subject := tolower(substr(id,1,3))] # some variables come from same "subject" table, classified by first 3 digits of code


count <- 1 # get and column bind census variables from SafeGraph Open Census Data
census <- foreach(i = unique(table_ids$subject), .combine = cbind) %do% {
  
  # get vars from same subject table
  vars_select <- c(table_ids[subject == i]$id)
  
  # get dir
  if (i == "b17") # naming quirk
    i <- "c17"
  dir <- file.path(dataIn, 'safegraph_open_census_data_2020', 'data', paste0('cbg_', i, '.csv'))
  
  if (count == 1) # only get cbg column once
    vars_select <- c("census_block_group", vars_select)
  
  # load data
  data <- fread(input = dir, select = vars_select)
  
  count <- count + 1 
  
  data # pass to foreach
  
}

# assign easy names from list based on id code
colnames(census)[2:length(colnames(census))] <- table_ids_list[colnames(census)[2:length(colnames(census))]] 



## construct variables

# education
census[, edu_highschool := edu_highschool + edu_ged + edu_college_1year + edu_college_nodegree + 
         edu_associate + edu_bach + edu_masters + edu_professional + edu_doctorate]

census[, edu_bach :=  edu_bach + edu_masters + edu_professional + edu_doctorate]

# age
census[, old_1824 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("age", c(7:10,31:34))] #sum over male and female age groups
census[, old_2564 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("age", c(11:19, 35:43))]
census[, old_65 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("age", c(20:25, 44:49))]

# income groups
census[, hh_inc_0_20 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("hh_inc_", seq(10,20,5))] # sum over hh inc groups
census[, hh_inc_20_45 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("hh_inc_", seq(25,45,5))] # sum over hh inc groups
census[, hh_inc_45_125 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("hh_inc_", c(50,60,75,100,125))] # sum over hh inc groups
census[, hh_inc_125_200 := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("hh_inc_", c(150,200))] # sum over hh inc groups
census[, hh_inc_200plus := hh_inc_200inf] # just reassigning this so columns are in the right order


# health insurance
census[, hlth_insur := rowSums(.SD, na.rm = TRUE), .SDcols = paste0("health", seq(0,3))] # sum over health insurance, all ages


## correct units

# divide by population: add variables here!
vars_pop <- c("male", "race_w", "race_b", "race_h", "race_a", "race_n", "old_1824", "old_2564", "old_65", "hlth_insur")
vars_hh <- c("socialSecurity", "publicAssistance", "foodStamps", "hh_married", "hh_singleMale", "hh_singleFemale",
             "hh_livingAlone", "hh_family", "hh_inc_0_20", "hh_inc_20_45", "hh_inc_45_125", "hh_inc_125_200", "hh_inc_200plus")
vars_edu <- c("edu_highschool", "edu_bach")

census <- census[, (vars_pop) := lapply(.SD, function(x) { x / pop}), .SDcols = vars_pop]
census <- census[, (vars_hh) := lapply(.SD, function(x) { x / hhs}), .SDcols = vars_hh]
census <- census[, (vars_edu) := lapply(.SD, function(x) { x / pop_25}), .SDcols = vars_edu]
census <- census[, unemployed := lapply(.SD, function(x) { x / laborForce}), .SDcols = "unemployed"]
census <- census[, housing_renter := lapply(.SD, function(x) { x / housing_total}), .SDcols = "housing_renter"]

# delete unnecessary variables
census[, c("edu_ged", "edu_college_1year", "edu_college_nodegree",
           "edu_associate","edu_masters", "edu_professional", "edu_doctorate",
           "laborForce", "housing_total", "pop_25") := NULL]
census[, (colnames(census)[grepl("age", colnames(census))]) := NULL ]
census[,(paste0("hh_inc_", c(seq(10,50,5), 60,75,100,125,150,200, "200inf"))) := NULL]

fwrite(census, file.path(dataOut, 'census_wrangle.csv.gz'))

