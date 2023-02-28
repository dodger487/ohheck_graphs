# Chris Riederer
# 2022-09-02

# Make a graph showing what to do in your last move of the Oh Hell card game.

library(gt)
library(tidyverse)


################################################################################
## Set up data.


# Create initial data frame of card suits and values.
suits <- tibble(suit = c('HCD', 'S'))
# suits <- tibble(suit = c('♥️♣️♦️', '♠️'))
values <- tibble(value = 2:14)
# card_values <- fct_relevel(c(2,3,4,5,6,7,8,9,10,'J', 'Q','K', 'A'),
#                            levels = c(2,3,4,5,6,7,8,9,10,'J', 'Q','K', 'A'))
# values <- tibble(value = values)
df <- crossing(suits, values) %>% arrange(suit, value)
df <- df %>% mutate(is_spade = suit == 'S')
df %>% head
df <- df %>% mutate(card_value = ifelse(
  value == 11, 'J',
  ifelse(value == 12, 'Q',
         ifelse(value == 13, 'K',
                ifelse(value == 14, 'A',
                       value))))
)
df <- df %>% mutate(card_value = fct_relevel(card_value, levels = c(2,3,4,5,6,7,8,9,10,'J', 'Q','K', 'A')))
df %>% head(14)


# Add column showing number of cards better if you're leading or following.
df <- df %>%
  mutate(
    num_cards_better =
      if_else(
        suit == 'S',
        15 - value - 1,  # If spade, only beaten by other spades.
        # If leading and not a spade, beaten by all spades and higher cards in your suit.
        13 + 15 - value - 1,
      )
  )

# Make new rows for different numbers of players.
num_players_tibble <- tibble(num_players = c(2:8))
df <- crossing(df, num_players_tibble)

# Make new rows for if you're leading or following.
lead_tibble <- tibble(is_lead = as.factor(c("lead", "follow")))
df <- crossing(df, lead_tibble)

# Arrange it to make it looks nice.
df <- df %>% arrange(is_lead, num_players, suit, -value)

# Take a peek.
View(df)


################################################################################
## Compute probability of winning.

prob_no_card_better <- function(num_cards_worse, num_available_cards, players) {
  if(players == 0) { return(1)}
  return( 
    (num_cards_worse / num_available_cards) 
    * prob_no_card_better(num_cards_worse - 1, num_available_cards - 1, players - 1)
  )
}

win_probability_lead <- function(num_cards_better, num_players) {
  prob_no_card_better(
    52 - num_cards_better - 1,
    51,
    num_players - 1)
}

win_probability_follow <- function(num_cards_better, num_players, is_spade, value) {
  if (is_spade) {
    return(win_probability_lead(num_cards_better, num_players))
  }
  
  num_cards_worse_suit <- value - 2
  prob_worse_card_in_suit_lead <- num_cards_worse_suit / 51
  
  prob_no_other_card_beats_yours <- prob_no_card_better(
    num_cards_worse_suit - 1, # -1 because one was already played, the lead.
    52 - 2, # All cards -1 for your card -1 for the lead
    num_players - 2
  )
  return(prob_worse_card_in_suit_lead * prob_no_other_card_beats_yours)
}


win_probability <- function(num_cards_better, num_players, is_lead, is_spade, value) {
  if(is_lead == "lead" || is_spade) {
    return(win_probability_lead(num_cards_better, num_players))
  }
  
  # Only handling non-spade follow  
  return(win_probability_follow(num_cards_better, num_players, is_spade, value))
}


df <- df %>% 
  rowwise %>%
  mutate(win_prob = win_probability(num_cards_better, num_players, is_lead, is_spade, value)) %>% #, suit, value)) %>%
  ungroup()
View(df)


################################################################################
## Graph it.

final_plot <- df %>%
  mutate(suit = ifelse(suit == 'S', '♠️', '♥️♣️♦️')) %>%
  # mutate(card_value = fct_relevel(card_value, levels = c(2,3,4,5,6,7,8,9,10,'J', 'Q','🤴', 'A'))) %>%
  mutate(card_name = paste0(suit, str_pad(value, 2))) %>%
  mutate(binary_win = win_prob > 0.5) %>%
  ggplot(aes(x = as.factor(num_players), y = as.factor(card_value), fill = binary_win)) +
  geom_tile(color = 'black') +
  geom_text(aes(label = scales::label_percent()(win_prob)), size = 1.8) +
  # facet_grid(rows = vars(suit), scales = "free_y") +
  facet_grid(suit ~ is_lead, scales = "free_y", switch = "y") +
  scale_fill_manual(values = c('#E06F60', '#61E0A0')) +
  xlab("Number Players") +
  ylab("") +
  theme_minimal() +
  # theme_bw() +
  theme(
    legend.position="none",
    strip.placement = "outside", # Move suits to left of card values on y axis
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 5.84 / 3.84
  ) +
  NULL
final_plot
ggsave("ohheck_postcard.png", width = 3.84, height = 5.84, units = "in")


###############################################################################
## Send postcard

library(ggirl)
contact_email <- "blair.waldorf@example.com"

send_address_1 <- address(name = "YOUR NAME HERE",
                          address_line_1 = "1136 Fifth Avenue",
                          city = "New York",
                          state = "New York",
                          postal_code = "10128",
                          country = "US")

message_1 <- "Now you know how to bid in Oh, Heck!"


ggpostcard_preview(final_plot)


ggpostcard(final_plot,
           contact_email,
           messages = message_1,
           send_addresses = send_address_1)
