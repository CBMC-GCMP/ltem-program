

library(fst)


sst <- read_fst("../tropicalization_in_the_GOC/data/full_oisst_pacific_data.fst") %>% 
  filter(between(lon, -112, -109)) %>% 
  filter(between(lat, 22.0, 23.0)) %>% 
  group_by(t) %>% 
  summarise(temp = mean(temp))


sst.ts <- ts(data = sst$temp, 
             start = c(1982,1), 
             frequency = 365)

dec <- decompose(sst.ts)
plot(dec)

sst$trend <- as.data.frame(dec$trend) %>% 
  rename(trend = x) %>% 
  pull(trend) %>% 
  as.numeric(.)


sst %>% 
  ggplot(aes(x = t, y = trend)) +
  geom_point(aes(x = t, y = temp), col = "gray90", pch = 21) +
  geom_line() +
  geom_hline(yintercept = mean(sst$temp), col = "blue") +
  geom_vline(xintercept = as.Date("1982-01-01"), col = "red") +
  geom_vline(xintercept = as.Date("1997-01-01"), col = "red") +
  geom_vline(xintercept = as.Date("2006-01-01"), col = "red") +
  geom_vline(xintercept = as.Date("2009-01-01"), col = "red") +
  geom_vline(xintercept = as.Date("2014-01-01"), col = "red") +
  theme_bw()+
  labs(x="Time", y= "Sea Surface Temperature °C") +
  scale_x_date(breaks = "3 year", labels = date_format("%Y"))

ggsave("report/figs/sst_fig.png", dpi = 600)

library(lubridate)
sst_tomod <- sst %>% 
  mutate(Year = year(t), Month = month(t)) %>% 
  group_by(Year, Month) %>% 
  summarise(temp = mean(temp)) %>% 
  mutate(Date = as.Date(paste(Year, Month, "15", sep = "-")), Time = as.numeric(Date)/1000)


m <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time), data = sst_tomod)

appraise(m$gam)
draw(m$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")
layout(1)


ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

## AR(1)
m1 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 10),
           data = sst_tomod, correlation = corARMA(form = ~ 1|Year, p = 1))

## AR(2)
m2 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 10),
           data = sst_tomod, correlation = corARMA(form = ~ 1|Year, p = 2),
           control = ctrl)

## AR(3)
m3 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 10),
           data = sst_tomod, correlation = corARMA(form = ~ 1|Year, p = 3),
           control = ctrl)


draw(m$gam)
summary(m1$gam)

anova(m$lme, m1$lme, m2$lme, m3$lme)

layout(matrix(1:2, ncol = 2))
plot(m1$gam, scale = 0)
layout(1)

layout(matrix(1:2, ncol = 2))
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)


want <- seq(1, nrow(sst_tomod), length.out = 200)
pdat <- with(sst_tomod,
             data.frame(Time = Time[want], Date = Date[want],
                        Month = Month[want]))

## predict trend contributions
p  <- predict(m$gam,  newdata = pdat, se.fit = TRUE)
p1 <- predict(m1$gam, newdata = pdat,  se.fit = TRUE)


sst_tomod$fit <- p1$fit

pdat$fit1 <- p1$fit
pdat$se1 <- p1$se.fit


(p1 <- pdat %>% 
    ggplot(aes(x=Date, y=fit1)) +
    geom_line() +
    geom_ribbon(data = new_data,
                aes(x = Year,
                    y = predicted, 
                    ymin = predicted - se.fit, 
                    ymax = predicted + se.fit), 
                alpha = .2) +
    geom_line(data = new_data,
              aes(x=Year, y=predicted)) +
    labs(x="Year", y=bquote("Average biomass (ton/ha)")) +
    scale_x_continuous(breaks = seq(2009,2021, by=2)) +
    scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
    scale_color_manual(values = c("red", "blue", "darkgreen", "orange")) +
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank()))