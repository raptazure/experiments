import os
from flask import Flask, render_template
from bson.objectid import ObjectId

'''Python with MongoDB'''
# Creating a database and collection
MONGODB_URI = 'mongodb+srv://asabeneh:your_password_goes_here@30daysofpython-twxkr.mongodb.net/test?retryWrites=true&w=majority'    # Your Connection String(MongoDB URI)
client = pymongo.MongoClient(MONGODB_URI)

db = client.thirty_days_of_python   # Creating database
db.students.insert_one(
    {'name': 'Asabeneh', 'country': 'Finland', 'city': 'Helsinki', 'age': 250})    # Creating collection and inserting a document
print(client.list_database_names())
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Inserting many documents to collection
db = client['thirty_days_of_python']  # accessing the database
students = [
    {'name': 'David', 'country': 'UK', 'city': 'London', 'age': 34},
    {'name': 'John', 'country': 'Sweden', 'city': 'Stockholm', 'age': 28},
    {'name': 'Sami', 'country': 'Finland', 'city': 'Helsinki', 'age': 25},
]
for student in students:
    db.students.insert_one(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# MongoDB Find
db = client['thirty_days_of_python']
student = db.students.find_one({'_id': ObjectId('5df68a23f106fe2d315bbc8c')})
print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

db = client['thirty_days_of_python']
students = db.students.find({}, {"_id": 0,  "name": 1, "country": 1})    # 0 means not include and 1 means include
for student in students:
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Find with Query
db = client['thirty_days_of_python']
students = db.students.find(query)
for student in students:
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Find query with modifier
db = client['thirty_days_of_python']
query = {"age": {"$gt": 30}}
students = db.students.find(query)
for student in students:
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Limiting documents
db = client['thirty_days_of_python']
db.students.find().limit(3)

# Find with sort
db = client['thirty_days_of_python']
students = db.students.find().sort('name')
for student in students:
    print(student)
students = db.students.find().sort('name', -1)
for student in students:
    print(student)
students = db.students.find().sort('age')
for student in students:
    print(student)
students = db.students.find().sort('age', -1)
for student in students:
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Update with query
db = client['thirty_days_of_python']
query = {'age': 250}
new_value = {'$set': {'age': 38}}
db.students.update_one(query, new_value)
for student in db.students.find():
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Delete Document
db = client['thirty_days_of_python']
query = {'name': 'John'}
db.students.delete_one(query)
for student in db.students.find():
    print(student)
app = Flask(__name__)
if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Drop a collection
db = client['thirty_days_of_python']
db.students.drop()
