#===========================================================================
# LOADING OF PRESAVED GROWTH STANDARDS
#===========================================================================

# definitions
sexType = c("Female", "Male")
featureType = c("Height", "Weight", "BMI", "Head")

# search for csv files with three dots in a name
curvesFiles = list.files("data", pattern="^([^\\.]*\\.){3}[^\\.]*csv$")
fileDF = data.frame(do.call(rbind, strsplit(curvesFiles, split="\\.")),
                    row.names=curvesFiles)
colnames(fileDF) = c("Name", "Sex", "Feature")

# find properly named input data
idx = fileDF$Sex %in% sexType & fileDF$Feature %in% featureType
fileDF = fileDF[idx,]

# change columns to factors to control order
fileDF$Name = factor(fileDF$Name, levels=sort(unique(fileDF$Name),
                                              decreasing=TRUE))
fileDF$Sex = factor(fileDF$Sex, levels=sexType)
fileDF$Feature = factor(fileDF$Feature, levels=featureType)

# import data and put it in a proper structure
growth_data = tapply(1:nrow(fileDF), fileDF$Name, function(i) {
  x = fileDF[i,]
  tapply(1:nrow(x), x$Sex, function(j) {
    y = x[j,]
    tapply(1:nrow(y), y$Feature, function(k) {
      read.csv2(file.path("data", rownames(y)[k]), dec=".")
    }, simplify=FALSE)
  })
})

# function to remove NA elements, keeping only data.frames
clean_list = function(x) {
  lapply(x, function(y) {
    lapply(y, function(z) {
      z[sapply(z, is.data.frame)]
    })
  })
}

# clean up unused levels
growth_data = clean_list(growth_data)