### Now lets try the Kelly bets again with random sampling
Kelly_Bets_Random <- Bets %>%
  arrange(date) %>%
  mutate(Win_Prob_Difference = prob1 - home_odds_close_probability) %>%
  filter(Win_Prob_Difference > .05) %>%
  group_by(date) %>%
  nest() %>%             
  ungroup() %>%
  mutate(samp = map2(data, 1, sample_n)) %>%
  select(-data) %>%
  unnest(samp) %>%
  mutate(Bankroll_Before = 100,
         Bet_Size = (prob1*home_odds_close-1)/(home_odds_close-1), # Remember Bet Size with Kelly Formula is as a precentage of the Bankroll
         Bet = Bankroll_Before*Bet_Size,
         Money_Made = ifelse(prob1_outcome == 1, Bet*home_odds_close-Bet, -Bet),
         Bankroll_After = Bankroll_Before+Money_Made) 

# Run a For Loop to Update Everything
for(x in 2:nrow(Kelly_Bets_Random)){
  Kelly_Bets_Random$Bankroll_Before[x] <- Kelly_Bets_Random$Bankroll_After[(x-1)]
  Kelly_Bets_Random$Bet[x] <- Kelly_Bets_Random$Bankroll_Before[x]*Kelly_Bets_Random$Bet_Size[x]
  Kelly_Bets_Random$Money_Made[x] <- ifelse(Kelly_Bets_Random$prob1_outcome[x] == 1, Kelly_Bets_Random$Bet[x]*Kelly_Bets_Random$home_odds_close[x]-Kelly_Bets_Random$Bet[x], -Kelly_Bets_Random$Bet[x])
  Kelly_Bets_Random$Bankroll_After[x] = Kelly_Bets_Random$Bankroll_Before[x]+Kelly_Bets_Random$Money_Made[x]
}
rm(x)

### Turns out Random Sampling is not very consistent. Let's try with second greatest favorable Prediction - odds
Kelly_Bets_Second_Place <- Bets %>%
  arrange(date) %>%
  mutate(Win_Prob_Difference = prob1 - home_odds_close_probability) %>%
  group_by(date) %>%
  mutate(rank_in_date = rank(desc(Win_Prob_Difference))) %>%
  filter(rank_in_date == 4) %>%
  mutate(Bankroll_Before = 100,
         Bet_Size = (prob1*home_odds_close-1)/(home_odds_close-1), # Remember Bet Size with Kelly Formula is as a precentage of the Bankroll
         Bet = Bankroll_Before*Bet_Size,
         Money_Made = ifelse(prob1_outcome == 1, Bet*home_odds_close-Bet, -Bet),
         Bankroll_After = Bankroll_Before+Money_Made) 

# Run a For Loop to Update Everything
for(x in 2:nrow(Kelly_Bets_Second_Place)){
  Kelly_Bets_Second_Place$Bankroll_Before[x] <- Kelly_Bets_Second_Place$Bankroll_After[(x-1)]
  Kelly_Bets_Second_Place$Bet[x] <- Kelly_Bets_Second_Place$Bankroll_Before[x]*Kelly_Bets_Second_Place$Bet_Size[x]
  Kelly_Bets_Second_Place$Money_Made[x] <- ifelse(Kelly_Bets_Second_Place$prob1_outcome[x] == 1, Kelly_Bets_Second_Place$Bet[x]*Kelly_Bets_Second_Place$home_odds_close[x]-Kelly_Bets_Second_Place$Bet[x], -Kelly_Bets_Second_Place$Bet[x])
  Kelly_Bets_Second_Place$Bankroll_After[x] = Kelly_Bets_Second_Place$Bankroll_Before[x]+Kelly_Bets_Second_Place$Money_Made[x]
}