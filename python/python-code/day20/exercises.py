import requests
from collections import Counter

url = 'http://www.gutenberg.org/files/1112/1112.txt'
response = requests.get(url)
txt = response.text
content = re.split('[ ~!@#$%^&*(),./\?:;"\'-_\n]', txt)
word_list = list(filter(None, content))
word_dict = {}
for word in word_list:
    if word in word_dict:
        word_dict[word] += 1
    else:
        word_dict[word] = 1
count = Counter(word_dict)
print('The ten most frequent words used in the text are:')
print(count.most_common()[:10])

url = 'https://restcountries.eu/rest/v2/all'
response = requests.get(url)
print(response)
print(response.status_code)
countries = response.json()
print(countries[:10])
