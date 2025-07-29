library(stringr)

# 1. Corrected word list
wordlist <- c(
  "peat", "seat", "feet", "deep", "beat", "seek", "teeth", "peak", "sheets", "keep", "beep", "cheek", "kit", "shipped", "tip", "fit", "pit", "pick", "tick", "sit", "chip", "sip", "bit", "dip", "taped", "cake", "cape", "date", "gate", "take", "safe", "fake", "fate", "shape", "bait", "sake", "dead", "get", "beg", "bet", "debt", "peck", "deck", "pet", "set", "vet", "jets", "bed","gap", "pack", "smacked", "sat", "bat", "pat", "cap", "chat", "cat", "fat", "zap", "tax","dot", "got", "top", "tots", "jock", "shot", "socks", "dock", "cop", "pot", "lot", "box", "soup", "duke", "dupe", "food", "boots", "loop", "kook", "tube", "tooth", "suit", "shoot", "goop", "caught", "bought", "taught", "thought", "fought", "sought", "cough", "boss", "talked", "sauce", "toss", "chalk"
)

# 2. Read clipboard content on Mac (robust method)
passage <- tolower(paste(readLines(pipe("pbpaste")), collapse = " "))

# 3. Tokenize and count
tokens <- str_extract_all(passage, "\\b[a-z]+\\b")[[1]]
counts <- table(factor(tokens, levels = wordlist))

# 4. Check for missing or duplicated words
missing <- names(counts[counts == 0])
duplicates <- names(counts[counts > 1])

# 5. Output results
if (length(missing) == 0 && length(duplicates) == 0) {
  cat("✅ All", length(wordlist), "words are used exactly once.\n")
} else {
  if (length(missing) > 0) {
    cat("❌ Missing words:\n")
    print(missing)
  }
  if (length(duplicates) > 0) {
    cat("⚠️ Duplicated words:\n")
    print(duplicates)
  }
}
