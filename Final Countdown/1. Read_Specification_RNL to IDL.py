#-------------------------------------------------------------#
# RNL/CNL: Restricted/Controlled Natural Language             #
# IDL: Intermediate form of Description Logic representation  #
# ""RNL/CNL to RuleML Json""                                  #
# Implemented using pyswip                                    #
# Author: Bayzid Ashik Hossain                                #
#-------------------------------------------------------------#


import os
import nltk
import sys
from pyswip import Prolog

if os.path.exists("lexicon.pl"):
    os.remove("lexicon.pl") 

def read_specification():
    prolog = Prolog()
    prolog.consult("9. main.pl")

    if os.path.exists("RuleML.json"):
        os.remove("RuleML.json")

    s0 = 'start()'
    for y in list(prolog.query(s0)):
        print('start')        

    str3 =""
    file = open("specification.txt", "r") 
    for line in file:
        str1 = "readt([['"+line+"']])"
        print(str1)
        for y in list(prolog.query(str1)):
            print('yes')
        str1=""
        str2=""
        str3=""

    s1 = 'end()'
    for y in list(prolog.query(s1)):
        print('end')

    if os.path.exists("lexicon.pl"):
        os.remove("lexicon.pl")    

    s2 = 'store_lexicon(lexicon)'
    for y in list(prolog.query(s2)):
        print(y) 
    file.close()
    

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


read_specification()
update_json()

































            
