# Explicit list of words (all lowercase, as requested)
words <- c(
  "dot", "gap", "soup", "pack", "kit", "beck", "get", "tech", "tape", "cake",
  "ship", "got", "bet", "pox", "tap", "peat", "poop", "tot", "sat", "bat",
  "seat", "tip", "dupe", "food", "chat", "pat", "jock", "cap", "shot", "fit",
  "set", "tat", "feet", "take", "boot", "cape", "toot", "date", "deep", "gate",
  "coot", "beat", "kook", "pit", "top", "debt", "seek", "safe", "peck", "deck",
  "sock", "pet", "pick", "teeth", "dock", "fake", "pep", "cop", "peak", "fate",
  "tooth", "vat", "vet", "sit", "sheet", "tick", "cheat", "cat", "chip", "fat",
  "sip", "keep", "dip", "shape", "bit", "bait", "jet", "kate", "suit", "shoot",
  "zoot", "pot", "cot", "beep"
)

# Randomize the order explicitly
set.seed(NULL)  # Different every time you run it
randomized_words <- sample(words)

# Add explicit carrier phrases
carrier_phrases <- paste("The word is", randomized_words)

# Combine explicitly into a single string for easy copy-pasting
final_output <- paste(carrier_phrases, collapse = ". ")

# Add the final period explicitly
final_output <- paste0(final_output, ".")

# Print explicitly for easy copy-and-paste
cat(final_output)
