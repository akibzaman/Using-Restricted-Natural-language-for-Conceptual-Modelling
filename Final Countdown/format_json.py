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


def get_constraints():
  cons = []
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('Forall'):
      x = a.get('Forall')
      opdomain = x['Implies']['head']['Atom']['Ind']
      #op = x['Implies']['body']['Exists']['And']['Atom'][0]['Ind']
      oprange = x['Implies']['body']['Exists']['And']['Atom'][3]['Ind']
      maxcard = x['Implies']['body']['Exists']['And']['Atom'][2]['Data']['__text']
      mincard = x['Implies']['body']['Exists']['And']['Atom'][1]['Data']['__text']  
      #print(opdomain, op, oprange, mincard, maxcard)
      cons.append((opdomain, oprange, mincard, maxcard))
  return cons
    
def print_all():
  print(get_entities())
  print()
  print(get_attributes())
  print()
  print(get_relations())
  print()
  print(get_constraints())

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









entities = []
ob_entities = []
attributes = []
constraints = []


def get_data_Fjson():
  dataprol = []
  datapro = []
  subset = []
  exclusion = []
  rel = []
  constraint = []
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('Forall'):
      x = a.get('Forall')
      y = x['Implies']['body']
      if isinstance(y, list):
        subset.append(x)
      elif y.get('Exists'):
        z = y.get('Exists')
        if isinstance(z, list):
          dataprol.append(x)
        elif z['And']['Atom'][0]['Ind'] == 'own':
          datapro.append(x)
      elif y.get('Neg'):
        exclusion.append(x)
      else:
        constraint.append(x)


##  print('Entities')
##  print('--------------------')
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('Atom'):
      x = a.get('Atom')
      if not isinstance(x,list):
        y = x.get('Rel')
        if y == 'entity' :
          entities.append(x['Ind'])
          #print(x['Ind'])
      else:
        if x[1]['Ind'] == 'objectifies':
          y = x[0]['Ind']
          rel_domain = x[1]['Reify']['And']['Atom'][0]['Ind']
          relation = x[1]['Reify']['And']['Atom'][1]['Ind']
          rel_range = x[1]['Reify']['And']['Atom'][2]['Ind']
          #print('Objectified Entity-',y, '--||--', rel_domain, relation, rel_range)
          ob_entities.append((y, rel_domain, relation, rel_range))

##  print()
##  print('Attributes')
##  print('--------------------')
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('Atom'):
      x = a.get('Atom')
      if not isinstance(x,list):
        y = x.get('Rel')
        if y == 'attribute' :
          #print(x['Ind'],'--||--', x['Var']['_type'])
          attributes.append((x['Ind'], x['Var']['_type']))

##  print()
##  print('Relations')
##  print('--------------------')
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('And'):
      x = a.get('And')
      #print(x['Atom'][0]['Ind'],'--', x['Atom'][1]['Ind'], '--', x['Atom'][2]['Ind'])

        
##  print()
##  print('Constraints')
##  print('-----------------')
##  print()
##  print('Subset Constraint')
##  print('-----------------')
  for a in subset:
    b = a.get('Implies')
    rel_domain = b['head']['Atom']['Ind']
    rel_one = b['body'][0]['Exists']['And']['Atom'][0]['Ind']
    rel_two = b['body'][1]['Exists']['And']['Atom'][0]['Ind']
    rel_range = b['body'][0]['Exists']['And']['Atom'][3]['Ind']
    #print(rel_domain, rel_one, rel_two, rel_range)
    constraints.append(('Subset',rel_domain, rel_one, rel_two, rel_range))
    print()


##  print()      
##  print('Exclusion Constraint')
##  print('--------------------')
  for a in exclusion:
    b = a.get('Implies')
    c = b.get('body')
    d = c.get('Neg')
    if d.get('Atom'):
      entity_one = b['head']['Atom']['Ind']
      entity_two = b['body']['Neg']['Atom']['Ind']
      #print(entity_one, '-|not', entity_two)
      constraints.append(('Exclusion','Entity', entity_one, entity_two))
    elif d.get('And'):
      rel_domain = b['head']['Atom']['Ind']
      rel_one = b['body']['Neg']['And'][0]['Atom'][0]['Ind']
      rel_two = b['body']['Neg']['And'][1]['Atom'][0]['Ind']
      rel_range = b['body']['Neg']['And'][0]['Atom'][1]['Ind']
      #print(rel_domain, '--|', rel_one, '-|not|--', rel_two, '|--', rel_range)
      print()
      constraints.append(('Exclusion','Fact', rel_domain, rel_one, rel_two, rel_range))


##  print()
##  print('Constraint on Relations')
##  print('--------------------------')
  for a in rj['RuleML']['Assert'][0:]:
    if a.get('Forall'):
      x = a.get('Forall')
      y = x['Implies']['body']
      if not isinstance(y, list):
        if y.get('Exists'):
          z = y.get('Exists')
          if not isinstance(z, list) and z['And']['Atom'][0]['Ind'] != 'own':
            rel.append(x)

  for a in rel:
    rel_domain = a['Implies']['head']['Atom']['Ind']
    relation = a['Implies']['body']['Exists']['And']['Atom'][0]['Ind']
    rel_range = a['Implies']['body']['Exists']['And']['Atom'][3]['Ind']
    min_cardinality = a['Implies']['body']['Exists']['And']['Atom'][1]['Data']['__text']
    max_cardinality = a['Implies']['body']['Exists']['And']['Atom'][2]['Data']['__text']
    #print(rel_domain, '--', relation, '--', rel_range, '--', min_cardinality, '--', max_cardinality)
    constraints.append(('Relation', rel_domain, relation, rel_range, min_cardinality, max_cardinality))
        

##  print()
##  print('Constraint on data properties/Attributes')
##  print('----------------------------------------')
  for a in dataprol:
    data_domain = a['Implies']['head']['Atom']['Ind']
    data_range1 = a['Implies']['body']['Exists'][0]['And']['Atom'][3]['Ind']
    min_cardinality1 = a['Implies']['body']['Exists'][0]['And']['Atom'][1]['Data']['__text']
    max_cardinality1 = a['Implies']['body']['Exists'][0]['And']['Atom'][2]['Data']['__text']
    data_range2 = a['Implies']['body']['Exists'][1]['And']['Atom'][3]['Ind']
    min_cardinality2 = a['Implies']['body']['Exists'][1]['And']['Atom'][1]['Data']['__text']
    max_cardinality2 = a['Implies']['body']['Exists'][1]['And']['Atom'][2]['Data']['__text']
    #print(data_domain, '--', data_range1, '--', min_cardinality1, '--', max_cardinality1)
    #print(data_domain, '--', data_range2, '--', min_cardinality2, '--', max_cardinality2)
    constraints.append(('Data', data_domain, data_range1, min_cardinality1, max_cardinality1))
    constraints.append(('Data', data_domain, data_range2, min_cardinality2, max_cardinality2))

  for a in datapro:
    data_domain = a['Implies']['head']['Atom']['Ind']
    data_range1 = a['Implies']['body']['Exists']['And']['Atom'][3]['Ind']
    min_cardinality1 = a['Implies']['body']['Exists']['And']['Atom'][1]['Data']['__text']
    max_cardinality1 = a['Implies']['body']['Exists']['And']['Atom'][2]['Data']['__text']
    #print(data_domain, '--', data_range1, '--', min_cardinality1, '--', max_cardinality2)
    constraints.append(('Data', data_domain, data_range1, min_cardinality1, max_cardinality1))


##  print()
##  print('Subclass constraints')
##  print('--------------------')

  for a in constraint:
    b = a.get('Implies')
    c = b.get('body')
    d = c.get('Or')
    if d:
      print()
      #print('super class constraint')
      #print('----------------------')
      sup_class = a['Implies']['head']['Atom']['Ind']
      for x in a['Implies']['body']['Or']['Atom']:
        #print(sup_class, '--', x['Ind'])
        constraints.append(('Superclass', sup_class, x['Ind']))
    else:
      sub_class = a['Implies']['head']['Atom']['Ind']
      sup_class = a['Implies']['body']['Ind']
      #print(sub_class, '--',sup_class)
      constraints.append(('Subclass', sub_class, sup_class))
      

gen_Fjson()
get_data_Fjson()

for a in constraints:
  print(a)
print('\n\n')
for a in attributes:
  print(a)
print('\n\n')
for a in entities:
  print(a)






























  
