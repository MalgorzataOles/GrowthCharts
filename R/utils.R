# Helper functions for GrowthCharts Shiny App

#-----------------------------------------------------------------------------
# Read all sheets from the Excel file
read_excel_allsheets = function(file) {
  sheets = readxl::excel_sheets(file)
  x = lapply(sheets, function(x)
    readxl::read_excel(file, sheet=x))
  names(x) = sheets
  x
}

#-----------------------------------------------------------------------------
# Merge charts from one source
mergeCharts = function(lchart) {
  # find common columns
  clmns = Reduce(intersect, lapply(lchart, colnames))
  # annotate each chart and collapse the view
  df = do.call(rbind, lapply(names(lchart), function(x) {
    name = strsplit(x, "_")[[1]]
    tmp = lchart[[x]][,clmns]
    tmp$FEATURE = ifelse(name[1]=="BMI", name[1], tolower(name[1]))
    tmp$SEX = tolower(name[2])
    tmp
  }))
  # wide to long
  df %>%
    tidyr::pivot_longer(matches("^[0-9]{1,2}$"),
                        names_to="PERCENTILE", values_to="VALUE") %>%
    mutate(YEARS=MONTHS/12) %>%
    dplyr::select(MONTHS, YEARS, FEATURE, SEX, PERCENTILE, VALUE) %>%
    filter(!is.na(VALUE))
}

#-----------------------------------------------------------------------------
# Read in and format charts
loadChart = function(file) {
  tmp = read_excel_allsheets(file)
  mergeCharts(tmp)
}

#-----------------------------------------------------------------------------
# Merge input measurements
mergeMeasurements = function(lmeas) {
  bday = setNames(lmeas$General$BIRTHDAY, nm=lmeas$General$NAME)
  sex = setNames(lmeas$General$SEX, nm=lmeas$General$NAME)
  people = names(lmeas)[names(lmeas) %in% names(bday)]
  data = lmeas[people]
  
  # check if columns are correctly named
  correctColnames = sapply(data, function(x)
    names(x)==c("DATE", "HEIGHT", "WEIGHT", "HEAD"))
  if(!all(correctColnames)) cat("Wrong data headers")
  
  # add age at measurement and sex
  do.call(rbind, lapply(names(data), function(nx) {
    x = data[[nx]] %>%
      mutate(AGE=time_length(interval(bday[nx],DATE), "years"))
    if(is.na(sex[nx]) | !sex[nx] %in% c("male", "female")) {
      x = rbind(x %>% mutate(SEX="female"), x %>% mutate(SEX="male"))
    } else {
      x = x %>% mutate(SEX=sex[nx])
    }
    x %>% select(-DATE) %>% mutate(NAME=nx)
  }))
}

#-----------------------------------------------------------------------------
# Read in and format measurements
loadMeasurements = function(file) {
  tmp = read_excel_allsheets(file$datapath[1])
  mergeMeasurements(tmp)
}

# if sex is not specified, it will be both male and female
# only names in General tab will be plotted (if they have data)
# two people with the same name can't coexist in General table
# All required columns must be present (their names), even if no data is given
#-----------------------------------------------------------------------------
# definitions
colDef = c("mediumorchid1", "orangered", "dodgerblue", "darkturquoise", "green3",
           "hotpink1", "gold", "lightsalmon", "seagreen1", "grey")
lcolDef = length(colDef)
#-----------------------------------------------------------------------------