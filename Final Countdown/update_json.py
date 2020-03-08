import os
import nltk
import sys


def update_json():
    list = []
    file = open("RuleML.json", "r")
    i=0
    for line in file:
        i = i+1

    file.close()

    file = open("RuleML.json", "r")
    j=0
    for line in file:
        j=j+1
        if j == i-4:
           list.append('}')
        else:
           list.append(line)
    file.close()
    file = open("RuleML.json", "w")
    for a in list:
        file.write(a)
    file.close()



update_json()





