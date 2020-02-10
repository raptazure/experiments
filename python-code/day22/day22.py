import requests
from bs4 import BeautifulSoup

'''Python Web Scraping'''
url = 'http://mlr.cs.umass.edu/ml/datasets.html'
response = requests.get(url)
print(response.status_code)  # 200 means the fetching was successful
content = response.content  # we get all the content from the website
soup = BeautifulSoup(content, 'html.parser')
print(soup.title)  # <title>UCI Machine Learning Repository: Data Sets</title>
print(soup.title.get_text())  # UCI Machine Learning Repository: Data Sets
print(soup.body)  # gives the whole page on the website
tables = soup.find_all('table', {'cellpadding': '3'})
table = tables[0]  # the result is list, we are taking out from the list
for td in table.find('tr').find_all('td'):
    print(td.text)
