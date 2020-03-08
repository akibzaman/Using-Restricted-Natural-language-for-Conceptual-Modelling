import json
import os

file = open("RuleML.json", "r")
rj = json.load(file)

b = []
r = []
c = []

for a in rj['RuleML']['Assert'][0:]:
  if 'Atom' in a.keys():
    b.append(a)
  if 'And' in a.keys():
    r.append(a)
  if 'Forall' in a.keys():
    c.append(a)

file.close()

def print_list(list):
  for a in list:
    print(a)


def get_entities():
  ent = []
  for a in b:
    x = a.get('Atom')
    if type(x)== dict:
      if x.get('Rel') == 'entity':
        ent.append(x.get('Ind'))
  return ent


def get_attributes():
  att = []
  for a in b:
      x = a.get('Atom')
      if type(x)== dict:
        if x.get('Rel') == 'attribute':
          att.append(x.get('Ind'))
  return att

 
def get_relations():
  rel = []
  for a in r:
    x = a.get('And')
    for b in x.get('Atom'):
      if b.get('Rel') == 'relation':
        rel.append(b.get('Ind'))
  return rel    

    
def print_all():
  print(get_entities())
  print()
  print(get_attributes())
  print()
  print(get_relations())


#----------------------
# Listing the relations
# under "And" in RuleML
#----------------------

def get_json_and():
  json_and = []
  i = 0
  j = len(r)

  start_text = '"And":['
  end_text = ']'

  json_and.append(start_text)

  for a in r:
    x = a.get('And')
    x_ = str(x)  
    if i<j-1:
      x_ = x_+','    
    json_and.append(x_.replace("'",'"'))
    i = i+1
  json_and.append(end_text)

  return json_and


#-------------------------
# Listing the constraints
# under "Forall" in RuleML
#-------------------------

def get_json_forall():
  json_forall = []
  i = 0
  j = len(c)

  start_text = '"Forall":['
  end_text = ']'

  json_forall.append(start_text)

  for a in c:
    x = a.get('Forall')
    x_ = str(x)  
    if i<j-1:
      x_ = x_+','    
    json_forall.append(x_.replace("'",'"'))
    i = i+1
  json_forall.append(end_text)

  return json_forall

#-------------------------
# Listing the entities and
# attributes under "Atom"
# in RuleML
#-------------------------

def get_json_atom():
  json_atom = []
  i = 0
  j = len(b)

  start_text = '"Atom":['
  end_text = ']'

  json_atom.append(start_text)

  for a in b:
    x = a.get('Atom')
    x_ = str(x)  
    if i<j-1:
      x_ = x_+','    
    json_atom.append(x_.replace("'",'"'))
    i = i+1
  json_atom.append(end_text)

  return json_atom


#-------------------------
# Generating a formatted 
# json file for the CM Sys
#-------------------------

def gen_Fjson():
  if os.path.exists("Formatted_RuleML.json"):
      os.remove("Formatted_RuleML.json") 

  start_text = '{\n "RuleML": {\n "Assert": {\n'
  end_text = '},\n "_style": "ERD" \n} \n}'

  file1 = open("Formatted_RuleML.json", "w")
  file1.write(start_text)

  str1 = get_json_atom()
  for a in str1:
    file1.write('\t\t\t')
    file1.write(a)
    file1.write('\n')
  file1.write(',')
  file1.write('\n')
  
  str2 = get_json_and()
  for a in str2:
    file1.write('\t\t\t')
    file1.write(a)
    file1.write('\n')
  file1.write(',')
  file1.write('\n')
  
  str3 = get_json_forall()
  for a in str3:
    file1.write('\t\t\t')
    file1.write(a)
    file1.write('\n')
  file1.write(end_text)
  file1.close()


#gen_Fjson()



print(get_entities())
print()
print(get_attributes())
print()
print(get_relations())






















