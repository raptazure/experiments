import re
from collections import Counter

paragraph = '''I love teaching.
If you do not love teaching what else can you love.
I love Python if you do not love something which can give you all the capabilities to develop an application what else can you love.'''
txt = re.sub('[.|\n]', ' ', paragraph)
word_dict = {}
word_list = txt.split()
for word in word_list:
    if word in word_dict:
        word_dict[word] += 1
    else:
        word_dict[word] = 1
count = Counter(word_dict)
print(count.most_common()[:1])

points = ['-1', '2', '-4', '-3', '-1', '0', '4', '8']
sorted_points = sorted(map(int, points))
distance = sorted_points[len(sorted_points)-1] - sorted_points[0]
print('distance = ' + str(distance))

value = input('Please input a value:')
if not(value[0].isalpha() or value[0] == '_'):
    print('False')
else:
    for i in value[1:]:
        if not(i.isalnum() or i == '_'):
            print('False')
            break
    else:
        print('True')

sentence = '''%I $am@% a %tea@cher%, &and& I lo%#ve %tea@ching%;.
There $is nothing; &as& mo@re rewarding as educa@ting &and& @emp%o@wering peo@ple.
;I found tea@ching m%o@re interesting tha@n any other %jo@bs. %Do@es thi%s mo@tivate yo@u to be a tea@cher!?'''
clean_text = re.sub('[%$@&,#;?!]', '', sentence)
print(clean_text)
txt = re.sub('[.|\n]', ' ', clean_text)
word_dict = {}
word_list = txt.split()
for word in word_list:
    if word in word_dict:
        word_dict[word] += 1
    else:
        word_dict[word] = 1
count = Counter(word_dict)
print(count.most_common()[:3])
