# Helper functions for GrowthCharts Shiny App

#-----------------------------------------------------------------------------
# Read all sheets from the Excel file
read_excel_allsheets = function(file) {
  sheets = readxl::excel_sheets(file)
  x = lapply(sheets, function(x)
    readxl::read_excel(file, sheet=x, col_types="numeric"))
  names(x) = sheets
  x
}

#-----------------------------------------------------------------------------
# Merge charts from one source.
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