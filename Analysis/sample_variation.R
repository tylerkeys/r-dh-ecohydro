# population distro for true replicants 
# benthics only, no fish had reps
# Showing variance based on NT_Total
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_true-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
hist(data$delta_pct);
plot(data$rep1, data$rep0);

# population distro for fish
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_1mon-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
hist(data$delta_pct);
plot(data$nt_0, data$nt_1);

# population distro for fish where initial val from 5 to 10
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_1mon-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, nt_0 > 5 & nt_0 <= 10)
hist(datasub$delta_pct);

# population distro for fish where initial val >= 10
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_1mon-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, nt_0 >= 10)
hist(datasub$delta_pct);

# population distro for fish where initial val <= 5 
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_1mon-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, nt_0 <= 5)
hist(datasub$delta_pct);

uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_all-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, nt_0 > 5 & numdays > 90)
hist(datasub$delta_pct);

uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_all-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, nt_0 > 5 & numdays < 365)
hist(datasub$delta_pct);
quantile(data$abs_delta_pct);
quantile(datasub$abs_delta_pct);
plot(datasub$nt_0, datasub$nt_1);
summary(fit <- lm(datasub$nt_1 ~ datasub$nt_0)) 
abline(fit) # add regression line to the plot

uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_all-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, numdays < 365)
hist(datasub$delta_pct);
quantile(data$abs_delta_pct);
quantile(datasub$abs_delta_pct);
plot(datasub$nt_0,datasub$nt_1);

# do a single case study
uri <- 'http://deq1.bse.vt.edu/files/fe/vahydro_edas_all-reps.csv'
data <- read.csv(uri, header = TRUE, sep = ",");
datasub <- subset(data, featureid == 198211)
hist(datasub$delta_pct);
quantile(data$abs_delta_pct);
quantile(datasub$abs_delta_pct);
plot(datasub$nt_0,datasub$nt_1);


