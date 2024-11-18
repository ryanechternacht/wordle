# Why?

Just playing around with solving wordle's algorithmically

## Current Approach

The current approach takes all possible words, generates a letter
count in for each index, then scores each word based on how many 
letters from the word list match the index. 

Example: if 48 words have an "a" at index 0, and 22 words have a 
letter "b" at index 1, then "aback" will have a score of 48 + 22 + ...

## Improvements

The current system is very willing to double up on letters (it's first guess is always "sores"). I should probably penalize this. 

I've considered building out a test harness to run 1000+ words and
give an average guess count, to build better algorithms. I _think_ 
an optimal approach is probably graph-based. I.e. we would build a 
graph of words that connect to other words (green clues or yellow clues would generate different weights), and we would guess the word
with the highest weight towards available words. I don't know if there's a clear optimal weighting approach, or just tinker with those. 