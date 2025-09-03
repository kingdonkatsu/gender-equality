## Statistical significance check

install.packages("car")
install.packages("data.table")
library(data.table)
lawsuit.dt = fread("data/lawsuit.csv")

# Convert categorical variables to factor
cat_vars = c("Dept", "Rank", "Gender", "Clin", "Cert")
lawsuit.dt[, (cat_vars) := lapply(.SD, as.factor), .SDcols = cat_vars]

# First regression plot
lin94 = lm(Sal94 ~ Dept + Gender + Clin + Cert + Prate + Exper + Rank, data = lawsuit.dt)
summary(lin94)

# Checking for nonlinear relationships by plotting partial residual plots
library(car)
crPlots(lin94)

# Do the same for sal95
lin95 = lm(Sal95 ~ Dept + Gender + Clin + Cert + Prate + Exper + Rank, data = lawsuit.dt)
summary(lin95)

# Checking for nonlinear relationships
library(car)
crPlots(lin95)

# Checking for correlation
vif(lin94)
vif(lin95)

# Prate is highly correlated, check for dependency
lin_prate = lm(Prate ~ Dept + Clin + Cert + Exper + Rank, data = lawsuit.dt)
summary(lin_prate)

# From reg results, prate is affected by dept, clin and rank
# We can therefore drop prate as a factor
lin94 = lm(Sal94 ~ Dept + Gender + Clin + Cert + Exper + Rank, data = lawsuit.dt)
summary(lin94)
lin95 = lm(Sal95 ~ Dept + Gender + Clin + Cert + Exper + Rank, data = lawsuit.dt)
summary(lin95)

