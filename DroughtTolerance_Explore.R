# Doing some summaries of the Drought tolerance designation info
library(ggplot2)
# designations <- googlesheets4::read_sheet(ss="1RwwCrJ-A24XdyHCUbPs9A12xqd6XA-bL", sheet="Sheet1", range="A1:J160")
# path.dat <- "~/Google Drive/Shared drives/Urban Ecological Drought/Drought Tolerance Designations/Drought Tolerance Designations.xlsx"

designations <- readxl::read_xlsx("~/Google Drive/Shared drives/Urban Ecological Drought/Drought Tolerance Designations/Drought Tolerance Designations.xlsx", sheet="Sheet1", range="A1:K160")

designations$`Grant Designation-Final` <- as.factor(designations$`Grant Designation-Final`)
summary(designations)

ggplot(data=designations) +
  geom_boxplot(aes(x=USDA, y=`PsiP0 Summer`))
