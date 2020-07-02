#!/usr/bin/env python

import betacode.conv

betacoded = ""

with open("dados.csv", "r") as dados:
    for line in dados:
        betacoded += betacode.conv.uni_to_beta(line)

with open("dados2.csv", "w") as novo:
    novo.write(betacoded)

print("Cabou")

