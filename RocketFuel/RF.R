library(dplyr)
library(ggplot2)

setwd("C:/Users/manish.grewal/emdp/R/RocketFuel")
dat <- read.csv("RocketFuel.csv")

## Convert to factors
dat$ftest <- factor(dat$test, labels = c("control", "test"))
dat$fconverted <- factor(dat$converted, labels = c("not-converted", "converted"))

dat1 <- dat[dat$tot_impr <= 100,] 
dat2 <- dat[dat$tot_impr > 100 & dat$tot_impr <= 200,] 
dat3 <- dat[dat$tot_impr > 200 & dat$tot_impr <= 300,] 
dat4 <- dat[dat$tot_impr > 300 & dat$tot_impr <= 500,]
dat5 <- dat[dat$tot_impr > 500 & dat$tot_impr <= 1000,] 
dat6 <- dat[dat$tot_impr > 1000,] 


every_nth = function(n) {return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})}

x <- rbind(dat1, dat2, dat3)
x <- dat4
x <- dat1
x$tot_impr_bin_w <- cut(x$tot_impr, breaks = c(0, unique(quantile(x$tot_impr, probs = seq(0, 1, .05)))), dig.lab = 5)
x$tot_impr_bin_w <- cut(x$tot_impr, breaks = seq(min(x$tot_impr) - min(x$tot_impr) %% 10, max(x$tot_impr) + 10, 10), dig.lab = 5)

summ <- x %>%
  group_by(tot_impr_bin_w, ftest) %>%
  summarise(conversion_rate = mean(converted) * 100, count = n())

p <- ggplot(summ, aes(tot_impr_bin_w, conversion_rate, fill = ftest)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

s <- unique(quantile(dat1$tot_impr, probs = seq(0, 1, .05)))
s <- c(0, unique(quantile(dat1$tot_impr, probs = seq(0, 1, .05))))
dat1$tot_impr_cat <- cut(dat1$tot_impr, breaks = seq(0, 100, 5))
dat1$tot_impr_cat <- cut(dat1$tot_impr, breaks = s)

ggplot(dat1, aes(tot_impr_cat, converted, fill = ftest)) +
  geom_bar(stat = "identity") +
  #scale_x_discrete(breaks = x$tot_impr_bin_w[seq(1, nrow(x), 10)]) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


my_func <- function (x) {
  #unique(quantile(x$tot_impr, probs = seq(0, 1, .1)))
  x$tot_impr_bin_w <- cut(x$tot_impr, breaks = c(0, unique(quantile(x$tot_impr, probs = seq(0, 1, .1)))), dig.lab = 5)

  #x$tot_impr_bin_w <- cut(x$tot_impr, breaks = seq(0,100,10), dig.lab = 5)

  summ <- group_by(x, tot_impr_bin_w, ftest, fconverted)
  ggplot(x, aes(tot_impr_bin_w, converted, fill = fconverted)) +
    geom_histogram(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

p <- my_func(dat1)
p
p <- my_func(dat2)
p
p <- my_func(dat3)
p
p <- my_func(dat4)
p



library(scales)
show_col(hue_pal()(2))



From looking at the quantiles, we see that 99.5% of that records have tot_impr less than or equal to 276. So we will split the dataset into four parts for ease of analysis.

```{r}
dat1 <- dat[dat$tot_impr <= 100,] 
dat2 <- dat[dat$tot_impr > 100 & dat$tot_impr < 200,] 
dat3 <- dat[dat$tot_impr > 200 & dat$tot_impr < 500,] 
dat4 <- dat[dat$tot_impr > 500,] 
```

```{r fig.show="hold", out.width="40%"}
quantile(dat1$tot_impr, probs = seq(0.05, 1, .05))
dat1$tot_impr_bin_f <- cut(dat1$tot_impr, breaks = unique(quantile(dat1$tot_impr, probs = seq(0.05, 1, .05))), dig.lab = 5)

summary(dat1$tot_impr_bin_f)

ggplot(dat1, aes(tot_impr_bin_f, fill = fconverted)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(dat1, aes(tot_impr_bin_f, fill = fconverted)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

summary(dat1$tot_impr)

```


#### Equal width binning
```{r q_3a_31}
dat$tot_impr_bin_w <- cut(dat$tot_impr, breaks = seq(0L, 2250L, 50L), dig.lab = 5)

ggplot(dat, aes(tot_impr_bin_w)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

#### Equal frequency binning

```{r}
quantile(dat$tot_impr, probs = seq(0, 1, .2))
dat$tot_impr_bin_f <- cut(dat$tot_impr, breaks = c(0, unique(quantile(dat$tot_impr, probs = seq(0, 1, .005)))), dig.lab = 5)

ggplot(dat, aes(tot_impr_bin_f)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

```




x <- dat #[dat$test == 1,]

x$tot_impr_bin_f <- cut(x$tot_impr, breaks = c(0, unique(quantile(x$tot_impr, probs = seq(0, 1, .01)))), dig.lab = 5)

summ_f <- x %>%
  group_by(tot_impr_bin_f, test, converted) %>%
  summarise(num_converted = n(), sum_tot_impr = sum(tot_impr)) %>%
  mutate(prop = num_converted * 100 / sum(num_converted), sum = sum(num_converted), sum_tot_impr = sum(sum_tot_impr))
  
#summ_f0 <- filter(summ_f, converted == 0)
#summ_f1 <- filter(summ_f, converted == 1)
  
summ_f_t <- summ_f %>% filter(test == 1, converted == 1, tot_impr_bin_f != '(11,12]'
                              )
summ_f_c <- summ_f %>% filter(test == 0, converted == 1)

CPM <- 9
summ_f_t$lift <- summ_f_t$prop - summ_f_c$prop
summ_f_t <- mutate(summ_f_t, 
                   num_exposed = sum - num_converted,
                   addl_profit = lift * num_exposed * 40 / 100,
                   cost_campaign = sum_tot_impr * CPM / 1000,
                   ROI = (addl_profit - cost_campaign) / cost_campaign)


every_nth = function(n) {return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})}

ggplot(summ_f_t, aes(x = tot_impr_bin_f)) +
  geom_bar(aes(y = addl_profit), stat = "identity", position = "stack", fill = "#cc000099") +
#  geom_bar(aes(y = cost_campaign), stat = "identity", position = "stack", fill = "#0000cc99") +
  #geom_bar(aes()) +
  scale_x_discrete(breaks = every_nth(n=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summ_f_t, aes(x = tot_impr_bin_f)) +
  #geom_bar(aes(y = addl_profit), stat = "identity", position = "stack", fill = "#cc000099") +
  geom_bar(aes(y = cost_campaign), stat = "identity", position = "stack", fill = "#0000cc99") +
  scale_x_discrete(breaks = every_nth(n=2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summ_f_t, aes(x = tot_impr_bin_f)) +
  #geom_bar(aes(y = addl_profit), stat = "identity", position = "stack", fill = "#cc000099") +
  geom_bar(aes(y = ROI), stat = "identity", position = "stack", fill = "#0000cc99") +
  scale_x_discrete(breaks = every_nth(n=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))













### 3.a.2 Plot conversion rates for those who were in the control group and for those who were exposed to the ad. 

#### Control Group

##### Equal frequency binning

```{r fig.width=10, fig.height=5}
#quantile(x$tot_impr, probs = seq(0, 1, .002))
x <- dat[dat$test == 0,]

x$tot_impr_bin_f <- cut(x$tot_impr, breaks = c(0, unique(quantile(x$tot_impr, probs = seq(0, 1, .005)))), dig.lab = 5)

summ_f <- x %>%
  group_by(tot_impr_bin_f, ftest) %>%
  summarise(conversion_rate = mean(converted) * 100, count = n())

ggplot(summ_f, aes(tot_impr_bin_f, conversion_rate)) +
  geom_bar(stat = "identity", fill="#F8766D") +
  scale_x_discrete(breaks = every_nth(n=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Conversion rates by total impressions - Control Group")
```

We can see the conversion rate steadily increases and reaches a peak at (116,128] total impressions. Further increase in total impressions leads to a declining conversion rate.  

#### Number of users by total impressions - Control Group - Equal frequency binning

```{r fig.width=10,fig.height=5}
ggplot(summ_f, aes(tot_impr_bin_f, count)) +
  geom_bar(stat = "identity", fill="#F8766D") +
  scale_x_discrete(breaks = every_nth(n=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of users by total impressions - Control Group")

```

#### Conversion rates by total impressions - Test Group - Equal frequency binning

```{r fig.width=10, fig.height=5}
#quantile(x$tot_impr, probs = seq(0, 1, .002))
x <- dat[dat$test == 1,]

x$tot_impr_bin_f <- cut(x$tot_impr, breaks = c(0, unique(quantile(x$tot_impr, probs = seq(0, 1, .005)))), dig.lab = 5)

summ_f <- x %>%
  group_by(tot_impr_bin_f, ftest) %>%
  summarise(conversion_rate = mean(converted) * 100, count = n())

ggplot(summ_f, aes(tot_impr_bin_f, conversion_rate)) +
  geom_bar(stat = "identity", fill = "#00bfc4") +
  scale_x_discrete(breaks = every_nth(n=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Conversion rates by total impressions - Test Group")
```

We can see the conversion rate steadily increases and reaches a peak at (116,128] total impressions. Further increase in total impressions leads to a declining conversion rate.  

#### Number of users by total impressions - Test Group - Equal frequency binning

```{r fig.width=10,fig.height=5}
ggplot(summ_f, aes(tot_impr_bin_f, count)) +
  geom_bar(stat = "identity", fill = "#00bfc4") +
  scale_x_discrete(breaks = every_nth(n=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of users by total impressions - Test Group")

```

