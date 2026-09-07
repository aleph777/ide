#!/usr/bin/python3

import bcrypt

# 1. Hashing
password = b"password"
hashed = bcrypt.hashpw(password, bcrypt.gensalt(rounds=12))

# 2. Verifying
if bcrypt.checkpw(password, hashed):
    print("Match")
    print(hashed)
