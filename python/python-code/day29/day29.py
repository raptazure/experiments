from flask import Flask,  Response
from bson.objectid import ObjectId
from bson.json_util import dumps
from datetime import datetime
import json
import pymongo


'''Building API'''
# Retrieving data using get
app = Flask(__name__)
@app.route('/api/v1.0/students', methods=['GET'])
def students():
    student_list = [
        {
            'name': 'Asabeneh',
            'country': 'Finland',
            'city': 'Helsinki',
            'skills': ['HTML', 'CSS', 'JavaScript', 'Python']
        },
        {
            'name': 'David',
            'country': 'UK',
            'city': 'London',
            'skills': ['Python', 'MongoDB']
        },
        {
            'name': 'John',
            'country': 'Sweden',
            'city': 'Stockholm',
            'skills': ['Java', 'C#']
        }
    ]
    return Response(json.dumps(student_list), mimetype='application/json')

if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Getting a document by id
app = Flask(__name__)
MONGODB_URI = 'mongodb+srv://asabeneh:your_password@30daysofpython-twxkr.mongodb.net/test?retryWrites=true&w=majority'
client = pymongo.MongoClient(MONGODB_URI)
db = client['thirty_days_of_python']  # accessing the database

@app.route('/api/v1.0/students', methods=['GET'])
def students():
    return Response(json.dumps(student), mimetype='application/json')

@app.route('/api/v1.0/students/<id>', methods=['GET'])
def single_student(id):
    student = db.students.find({'_id': ObjectId(id)})
    return Response(dumps(student), mimetype='application/json')

if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)

# Creating data using POST
@app.route('/api/v1.0/students', methods=['GET'])
def students():
    return Response(json.dumps(student), mimetype='application/json')

@app.route('/api/v1.0/students/<id>', methods=['GET'])
def single_student(id):
    student = db.students.find({'_id': ObjectId(id)})
    return Response(dumps(student), mimetype='application/json')

@app.route('/api/v1.0/students', methods=['POST'])
def create_student():
    name = request.form['name']
    country = request.form['country']
    city = request.form['city']
    skills = request.form['skills'].split(', ')
    bio = request.form['bio']
    birthyear = request.form['birthyear']
    created_at = datetime.now()
    student = {
        'name': name,
        'country': country,
        'city': city,
        'birthyear': birthyear,
        'skills': skills,
        'bio': bio,
        'created_at': created_at
    }
    db.students.insert_one(student)
    return
def update_student(id):
    if __name__ == '__main__':
        port = int(os.environ.get("PORT", 5000))
        app.run(debug=True, host='0.0.0.0', port=port)

# Updating using PUT (POST before PUT)
@app.route('/api/v1.0/students/<id>', methods=['PUT'])  # this decorator create the home route
def update_student(id):
    query = {"_id": ObjectId(id)}
    name = request.form['name']
    country = request.form['country']
    city = request.form['city']
    skills = request.form['skills'].split(', ')
    bio = request.form['bio']
    birthyear = request.form['birthyear']
    created_at = datetime.now()
    student = {
        'name': name,
        'country': country,
        'city': city,
        'birthyear': birthyear,
        'skills': skills,
        'bio': bio,
        'created_at': created_at
    }
    db.students.update_one(query, student)
    return

def update_student(id):
    if __name__ == '__main__':
        port = int(os.environ.get("PORT", 5000))
        app.run(debug=True, host='0.0.0.0', port=port)

# Deleting a document using Delete
app = Flask(__name__)
MONGODB_URI = 'mongodb+srv://asabeneh:your_password@30daysofpython-twxkr.mongodb.net/test?retryWrites=true&w=majority'
client = pymongo.MongoClient(MONGODB_URI)
db = client['thirty_days_of_python']  # accessing the database
@app.route('/api/v1.0/students', methods=['GET'])
def students():
    return Response(json.dumps(student), mimetype='application/json')

@app.route('/api/v1.0/students/<id>', methods=['GET'])
def single_student(id):
    student = db.students.find({'_id': ObjectId(id)})
    return Response(dumps(student), mimetype='application/json')

@app.route('/api/v1.0/students', methods=['POST'])
def create_student():
    name = request.form['name']
    country = request.form['country']
    city = request.form['city']
    skills = request.form['skills'].split(', ')
    bio = request.form['bio']
    birthyear = request.form['birthyear']
    created_at = datetime.now()
    student = {
        'name': name,
        'country': country,
        'city': city,
        'birthyear': birthyear,
        'skills': skills,
        'bio': bio,
        'created_at': created_at
    }
    db.students.insert_one(student)
    return

# this decorator create the home route
@app.route('/api/v1.0/students/<id>', methods=['PUT'])
def update_student(id):
    query = {"_id": ObjectId(id)}
    name = request.form['name']
    country = request.form['country']
    city = request.form['city']
    skills = request.form['skills'].split(', ')
    bio = request.form['bio']
    birthyear = request.form['birthyear']
    created_at = datetime.now()
    student = {
        'name': name,
        'country': country,
        'city': city,
        'birthyear': birthyear,
        'skills': skills,
        'bio': bio,
        'created_at': created_at
    }
    db.students.update_one(query, student)
    return

# this decorator create the home route
@app.route('/api/v1.0/students/<id>', methods=['PUT'])
def update_student(id):
    query = {"_id": ObjectId(id)}
    name = request.form['name']
    country = request.form['country']
    city = request.form['city']
    skills = request.form['skills'].split(', ')
    bio = request.form['bio']
    birthyear = request.form['birthyear']
    created_at = datetime.now()
    student = {
        'name': name,
        'country': country,
        'city': city,
        'birthyear': birthyear,
        'skills': skills,
        'bio': bio,
        'created_at': created_at
    }
    db.students.update_one(query, student)
    return

@app.route('/api/v1.0/students/<id>', methods=['DELETE'])
def delete_student(id):
    db.students.delete_one({"_id": ObjectId(id)})
    return

if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    app.run(debug=True, host='0.0.0.0', port=port)
