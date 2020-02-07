import os
import re
import ast
import json
from collections import Counter

os.chdir(r'day19/data')
f = open('donald_speech.txt', encoding='utf-8')
count = 0
txt = f.read()
for file_content in txt:
    if file_content == '\n':
        count += 1
if txt[-1] != '\n':
    count += 1
print('There are %d lines in the text.' % (count))
content = re.split('[ |\n|,|.|-]', txt)
word_list = list(filter(None, content))
print('There are %d words in the text.' % (len(word_list)))
content = re.split('[ |\n|,|.|-]', txt)
word_list = list(filter(None, content))
word_dict = {}
for word in word_list:
    if word in word_dict:
        word_dict[word] += 1
    else:
        word_dict[word] = 1
count = Counter(word_dict)
print('The ten most frequent words used in Trump\'s speech are:')
print(count.most_common()[:10])
f.close()


f = open('countries_data.json', 'r', encoding='utf-8')
mydict = json.loads(f.read())
def most_populated_countries(dict):
    return sorted(dict, key=lambda k: k['population'], reverse=True)
newdict = most_populated_countries(mydict)
sliced_dict = newdict[0:10]
for i in range(0, 10):
    print(sliced_dict[i].get('name'),
          sliced_dict[i].get('population'), end='\n')


f = open('romeo_and_juliet.txt', 'r', encoding='utf-8')
txt = f.read()
content = re.split('[ ~!@#$%^&*(),./\?:;"\'-_\n]', txt)
word_list = list(filter(None, content))
word_dict = {}
for word in word_list:
    if word in word_dict:
        word_dict[word] += 1
    else:
        word_dict[word] = 1
count = Counter(word_dict)
print('The ten most frequent words used in  speech are:')
print(count.most_common()[:10])
f.close()
