import json
import requests
from bs4 import BeautifulSoup

jsonlist = []
url = 'http://mlr.cs.umass.edu/ml/datasets.html'
response = requests.get(url)
print(response.status_code)
content = response.content
soup = BeautifulSoup(content, 'html.parser')
print(soup.title)
print(soup.title.get_text())
print(soup.body)
tables = soup.find_all('table', {'cellpadding': '3'})
table = tables[0]
for td in table.find('tr').find_all('td'):
    jsonlist.append(td.text)
with open('day22/exer1.json', 'w', encoding='utf-8') as f:
    json.dump(jsonlist, f, ensure_ascii=False, indent=4)

jsonlist = []
url = 'https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States'
response = requests.get(url)
print(response.status_code)
content = response.content
soup = BeautifulSoup(content, 'html.parser')
print(soup.title)
print(soup.title.get_text())
print(soup.body)
tables = soup.find_all('table')
table = tables[0]
for td in table.find('tr').find_all('td'):
    jsonlist.append(td.text)
with open('day22/exer2.json', 'w', encoding='utf-8') as f:
    json.dump(jsonlist, f, ensure_ascii=False, indent=4)
