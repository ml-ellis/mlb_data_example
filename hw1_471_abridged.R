### -- load packages --###
library(tidyverse)  # tidyverse
library(ggrepel)    # for scatter plot point labels
library(kableExtra) # for printing tables
library(cowplot) 

### -- Import data -- ###
mlb_raw <- read.csv("../../data/MLPayData_Total.csv")
mlb_raw

### -- Forming df for aggreagte date and yearly data -- ###

#forming aggregate df
team = mlb_raw %>% pull(Team.name.2014)
payroll_aggregate = mlb_raw %>% pull(payroll)
pct_wins_aggregate = mlb_raw %>% pull(avgwin)
mlb_aggregate = tibble(team = team,
                       payroll_aggregate = payroll_aggregate,
                       pct_wins_aggregate = pct_wins_aggregate)


#forming yearly df

mlb_yearly = select(mlb_raw, -avgwin, -payroll) #delete first two columns

mlb_yearly = mlb_yearly %>% #pivot longer
  pivot_longer(!Team.name.2014, names_to = "year", values_to = "value")

mlb_yearly = mlb_yearly %>% #separate date 
  separate(year, into = c("type", "year"), sep = 1) %>%
    separate(year, into = c("year", "gross"), sep = 4)

#combine the two columns which correspond to type
mlb_yearly$type = str_c(mlb_yearly$type, mlb_yearly$gross)

mlb_yearly = select(mlb_yearly, -gross) #delete the unnecessary column

mlb_yearly = mlb_yearly %>% #pivot wider
  pivot_wider(names_from = "type", values_from = "value")

mlb_yearly = mlb_yearly %>% #rename columns
  rename(team = Team.name.2014, payroll = p, number_wins = X, pct_wins = X.pct)

### -- Compute a aggregate df from the yearly data 
### & compare w/original aggregate df --


#use aggregate function to sum the columns based off their team
df = aggregate(mlb_yearly, by = list(mlb_yearly$team), FUN = mean)

#create a tibble using df
mlb_aggregate_computed = tibble(
  team = df$Group.1,
  payroll_aggregate_computed = (17 * df$payroll)
  /1000,
  pct_wins_aggregate_computed = df$pct_wins
)


#scatter plot comparing both mlb_aggregate and mlb_aggregate_computed
conjoined_aggregate = left_join(mlb_aggregate, mlb_aggregate_computed)

p1 = conjoined_aggregate %>%
  ggplot(aes(x = payroll_aggregate, y = payroll_aggregate_computed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(y= "Payroll Aggregate Computed", x = "Payroll Aggregate")

p2 = conjoined_aggregate %>%
  ggplot(aes(x = pct_wins_aggregate, y = pct_wins_aggregate_computed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(y= "Percent Wins Aggregate Computed", x = "Percent Wins Aggregate")

plot_grid(p1, p2, labels = "auto")

### -- Plot payroll as function of year for all 30 teams --

mean.data = tibble ( #create a df to plot mean value lines
  team = df$Group.1,
  mean = df$payroll)

mlb_yearly %>% #creating ggplot
  ggplot(aes( x = year, y = payroll)) +
  geom_point() +
  facet_wrap(~ team, nrow = 10) +
  geom_hline(data = mean.data, aes(yintercept = mean), color = "red") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y= "Payroll", x = "Year")



### -- IDENTIFY THREE TEAMS WITH THE LARGEST PAYROLL_AGGREGATE_COMPUTED

descending_order = arrange(mlb_aggregate_computed, 
                           desc(by_group = payroll_aggregate_computed))

max_3 = tibble(
  team = descending_order$team[1:3],
  payroll_aggregate_computed = 
    descending_order$payroll_aggregate_computed[1:3]
)

max_3 %>%
  kbl(caption = "Teams with Largest Total Payroll", col.names = c("Team", 
                                                                  "Payroll Aggregate Computed")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


### -- CREATE SCATTER PLOT OF PCT_WINS vs. PAYROLL BASED ON AGGREGATED DATA

mlb_aggregate %>%
  ggplot(aes(x = pct_wins_aggregate, y = payroll_aggregate)) +
  geom_point() +
  geom_text_repel(aes(label = team)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y= "Payroll Aggregate", x = "Percent Wins Aggregate")

### -- IDENTIFY THE 3 TEAMS WITH THE GREATEST EFFICIENCY

team_efficiency = mlb_aggregate %>%
  mutate(efficiency = pct_wins_aggregate / payroll_aggregate)

team_efficiency = team_efficiency %>%
  arrange(desc(by_group = efficiency))

team_efficiency = team_efficiency %>%
  filter(efficiency > .64)

team_efficiency %>%
  kbl(caption = "Teams with Greatest Efficiency", col.names = c("Team", 
                                                                "Payroll Aggregate", "% Wins Aggregate", "Efficiency")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


### -- RUN A LINEAR REGRESSION OF PCT_WINS_AGGREGATE ON PAYROLL_AGGREGATE 
lm_fit_1 = lm(pct_wins_aggregate ~ payroll_aggregate, data = mlb_aggregate)
summary(lm_fit_1)

