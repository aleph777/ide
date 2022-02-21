#!/usr/bin/python3

import sys
import copy
import random

freq = {'a': 8.34,
        'b': 1.54,
        'c': 2.73,
        'd': 4.14,
        'e': 12.60,
        'f': 2.03,
        'g': 1.92,
        'h': 6.11,
        'i': 6.71,
        'j': 0.23,
        'k': 0.87,
        'l': 4.24,
        'm': 2.53,
        'n': 6.80,
        'o': 7.70,
        'p': 1.66,
        'q': 0.09,
        'r': 5.68,
        's': 6.11,
        't': 9.37,
        'u': 2.85,
        'v': 1.06,
        'w': 2.34,
        'x': 0.20,
        'y': 2.04,
        'z': 0.06}

wfreq = {}

def read_words(fname):
    file = open(fname, "r")

    if file.readable():
       text = []

       for item in file:
          text.append(item.rstrip ('\n'))
       file.close()
       return text
    else:
        sys.exit("Unable to open " + fname)

def tokenize_word(word):
    tdict  = {}
    f      = 0.0
    tokens = [char for char in word]

    tdict["letters"] = tokens

    for t in tokens:
        if t not in tdict:
           tdict[t] = 1
           f += freq[t]
        else:
           tdict[t] += 1

    wfreq[word] = f

    return copy.deepcopy(tdict)

def tokenize_words(words):
    tokenizedWords = {}

    for word in words:
        tokenizedWords[word] = tokenize_word(word)

    return tokenizedWords

def filter_black(letter, count, tokenizedWords):
    copy_tokenizedWords = copy.deepcopy(tokenizedWords)

    for word in tokenizedWords:
        if letter in tokenizedWords[word]:
           if tokenizedWords[word][letter] >= count:
              del copy_tokenizedWords[word]

    return copy_tokenizedWords

def filter_green(letter, index, tokenizedWords):
    copy_tokenizedWords = copy.deepcopy(tokenizedWords)

    for word in tokenizedWords:
        if tokenizedWords[word]["letters"][index] != letter:
           del copy_tokenizedWords[word]

    return copy_tokenizedWords

def filter_yellow(letter, index, tokenizedWords):
    copy_tokenizedWords = copy.deepcopy(tokenizedWords)

    for word in tokenizedWords:
        if tokenizedWords[word]["letters"][index] == letter:
           del copy_tokenizedWords[word]
        elif letter not in tokenizedWords[word]["letters"]:
           del copy_tokenizedWords[word]

    return copy_tokenizedWords

def filter_words(wordle_text, tokenizedWords):
    copy_tokenizedWords = copy.deepcopy(tokenizedWords)

    ldict = {}
    index = 0

    for w in wordle_text:
        text   = [char for char in w]
        letter = text[0]
        color  = text[1]

        if letter not in ldict:
           ldict[letter] = 1
        else:
           ldict[letter] += 1

    for w in wordle_text:
        text   = [char for char in w]
        letter = text[0]
        color  = text[1]

        if color == 'b':
            copy_tokenizedWords = filter_black( letter, ldict[letter], copy_tokenizedWords)
            ldict[letter] -= 1
        elif color == 'g':
            copy_tokenizedWords = filter_green( letter, index,         copy_tokenizedWords)
        elif color == 'y':
            copy_tokenizedWords = filter_yellow(letter, index,         copy_tokenizedWords)
        else:
            sys.exit("Invalid color")

        index += 1

    return copy_tokenizedWords

def process_input_text(text):
    words = text.split(" ")

    if len(words) == 1:
       sys.exit()

    if len(words) != 5:
       sys.exit("Wrong number of input tokens")

    for n in range(4):
        wdict = {}

        if len(words[n]) != 2:
            sys.exit("Wrong word length: " + words[n])

    return words

words = read_words(sys.argv[1])

tokenizedWords = tokenize_words(words)

inp = "text"

while True:
    wordle_text = process_input_text(input("Enter wordle text: ").rstrip())

    tokenizedWords = filter_words(wordle_text, tokenizedWords)

    n = len(tokenizedWords)

    print(n)

    wf = {}

    for item in list(tokenizedWords.keys()):
        wf[item] = wfreq[item]

    for w in sorted(wf.items(), key = lambda x: (-x[1], x[0]))[0: min(n, 9)]:
        print(w[0])
