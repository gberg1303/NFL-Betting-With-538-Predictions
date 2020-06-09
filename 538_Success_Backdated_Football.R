library(tidyverse)
library(nflfastR)
library(janitor)
library(lubridate)

### Load Team Names
nflfastR::teams_colors_logos

### Load Data from 538 NFL Regular Season Predictions
NFL_Predictions_FiveThirtyEight <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/checking-our-work-data/master/nfl_games.csv")

### Load Past Odds
NFL_Odds <- readxl::read_xlsx("/Users/jonathangoldberg/Google Drive/Random/Sports/Betting/538 Formula/nfl.xlsx") %>%
  clean_names() %>%
  filter(!is.na(date)) %>%
  merge(y = teams_colors_logos %>% select(team_name, team_nick), by.x = "home_team", by.y = "team_name", all.x = TRUE, all.y = FALSE) %>%
  mutate(home_odds_close = ifelse(is.na(home_odds_close) == TRUE, home_odds_open, home_odds_close),
         away_odds_close = ifelse(is.na(away_odds_close) == TRUE, away_odds_open, away_odds_close))

### Merge the Two
Predictions_Odds <- merge(x = NFL_Odds %>% select(date, team_nick, home_odds_close, away_odds_close), 
                          y = NFL_Predictions_FiveThirtyEight, 
                          by.x = c("date", "team_nick"),
                          by.y = c("date", "team1"),
                          all.x = FALSE,
                          all.y = TRUE) %>%
  unique() %>% # Remove Duplicates
  mutate(home_odds_close_probability = 1/home_odds_close,
         away_odds_close_probability = 1/away_odds_close) # Turn Decimal Into Probabilities


### Select Cases where 538 Prediction finds an optimal bet to be made 
Bets <- Predictions_Odds %>%
  mutate(Home_Bet = ifelse(prob1 > home_odds_close_probability, 1, 0),
         Away_Bet = ifelse(prob2 > away_odds_close_probability, 1, 0)) %>%
  filter(Home_Bet == 1 | Away_Bet == 1) %>%
  mutate(date = as.Date(date)) %>%
  mutate(Win_Prob_Difference = prob1 - home_odds_close_probability) %>%
  # This part is tricky. To ensure that everything flowed well, I simply assigned away probabilities and attributes to the home columns. 
  mutate(Win_Prob_Difference = ifelse(Away_Bet == 1, prob2 - away_odds_close_probability, Win_Prob_Difference),
         home_odds_close = ifelse(Away_Bet == 1, away_odds_close, home_odds_close),
         home_odds_close_probability = ifelse(Away_Bet == 1, away_odds_close_probability, home_odds_close_probability),
         prob1_outcome = ifelse(Away_Bet == 1, prob2_outcome, prob1_outcome),
         prob1 = ifelse(Away_Bet == 1, prob2, prob1))

### Lets first try to see if simple $10 bets are profitable before going any more complex
Bets <- Bets %>%
  filter(Win_Prob_Difference > .05) %>% # Ensure that there is a pretty decent margin between odds and the outcome
  mutate(Money_Made = ifelse(prob1_outcome == 1, 10*home_odds_close-10, -10))
sum(Bets$Money_Made)

### It turns out to be pretty profitable. Now lets try with the Kelly Criterion
Kelly_Bets <- Bets %>%
  arrange(date) %>%
  group_by(date) %>% 
  mutate(Week_Difference_Rank = rank(desc(Win_Prob_Difference))) %>%
  filter(Week_Difference_Rank == 1) %>%
  mutate(Bankroll_Before = 100,
         Bet_Size = prob1 - ((1-prob1)/(home_odds_close-1)), # Remember Bet Size with Kelly Formula is as a percentage of the Bankroll
         Bet = Bankroll_Before*Bet_Size,
         Money_Made = ifelse(prob1_outcome == 1, Bet*home_odds_close-Bet, -Bet),
         Bankroll_After = Bankroll_Before+Money_Made,
         Stash_Before = 0,
         Stash_After = 0,
         Total_Cash = Bankroll_Before) 

# Run a For Loop to Update Each Row with Bet Result
for(x in 2:nrow(Kelly_Bets)){
  Kelly_Bets$Stash_Before[x] <- Kelly_Bets$Stash_After[x-1]
  Kelly_Bets$Bankroll_Before[x] <- Kelly_Bets$Bankroll_After[(x-1)]
  Kelly_Bets$Bet[x] <- Kelly_Bets$Bankroll_Before[x]*Kelly_Bets$Bet_Size[x]
  Kelly_Bets$Money_Made[x] <- ifelse(Kelly_Bets$prob1_outcome[x] == 1, Kelly_Bets$Bet[x]*Kelly_Bets$home_odds_close[x]-Kelly_Bets$Bet[x], -Kelly_Bets$Bet[x])
  Kelly_Bets$Bankroll_After[x] = Kelly_Bets$Bankroll_Before[x]+Kelly_Bets$Money_Made[x]
  Kelly_Bets$Stash_After[x] <- Kelly_Bets$Stash_Before[x]
  if(Kelly_Bets$Bankroll_After[x] > 200){
  Kelly_Bets$Bankroll_After[x] <- Kelly_Bets$Bankroll_After[x]-50
  Kelly_Bets$Stash_After[x] <- Kelly_Bets$Stash_After[x]+50 
  }
  if(Kelly_Bets$Bankroll_After[x] < 50 & Kelly_Bets$Stash_Before[x] >= 25){
    Kelly_Bets$Bankroll_After[x] <- Kelly_Bets$Bankroll_After[x]+25
   Kelly_Bets$Stash_After[x] <- Kelly_Bets$Stash_After[x]-25
  }
  #if(Kelly_Bets$Stash_After[x] > 10*Kelly_Bets$Bankroll_After[x]){
  #  Kelly_Bets$Bankroll_After[x] <- Kelly_Bets$Bankroll_After[x]+(.25*Kelly_Bets$Stash_After[x])
  #  Kelly_Bets$Stash_After[x] <- Kelly_Bets$Stash_After[x]-(.25*Kelly_Bets$Stash_After[x])
  #}
  Kelly_Bets$Total_Cash[x] = Kelly_Bets$Stash_After[x]+Kelly_Bets$Bankroll_After[x]
}

### Complete the Bet Sheet
Kelly_Bets <- merge(Bets %>% select(-Win_Prob_Difference, Money_Made), 
                    Kelly_Bets %>% select(date, team_nick, Bankroll_Before, Bet_Size, Bet, Bankroll_After, Stash_Before, Stash_After, Total_Cash),
                    by = c("date", "team_nick"),
                    all.x = FALSE,
                    all.y = TRUE)
                    

### Graph Total Cash Over Time
Kelly_Bets %>%
  ggplot(aes(x = date, y = Total_Cash)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "NFL Betting: Using 538 Predictions and the Kelly Criterion to Generate Gains Over Time",
    subtitle = "One Bet per Week | Margin Between Odds and Probability of Bet Winning > 5%",
    y = "Total Cash",
    x = "Date",
    caption = "@gberg1303 | Data from @FiveThirtyEight | Kelly Criterion was modified to stash cash when bankroll doubled the principal and to feed the bankflow when it was short on cash. The modification makes hte formula more resiliant to disasterous runs."
  )
