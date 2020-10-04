## API (Application Programming Interface)

- Application Programming Interface(API). The kind of API will cover in this section is going to be Web APIS. Web APIs are the defined interfaces through which interactions happen between an enterprise and applications that use its assets, which also is a Service Level Agreement (SLA) to specify the functional provider and expose the service path or URL for its API users. In the context of web development, an API is defined as a set of specifications, such as Hypertext Transfer Protocol (HTTP) request messages, along with a definition of the structure of response messages, usually in an XML or a JavaScript Object Notation (JSON) format. Web API has been moving away from Simple Object Access Protocol (SOAP) based web services and service-oriented architecture (SOA) towards more direct representational state transfer (REST) style web resources. Social media services, web APIs have allowed web communities to share content and data between communities and different platforms. Using API, content that is created in one place dynamically can be posted and updated to multiple locations on the web. For example, Twitter's REST API allows developers to access core Twitter data and the Search API provides methods for developers to interact with Twitter Search and trends data. Many applications provide API end points.
- RESTful API is an application programming interface (API) that uses HTTP requests to GET, PUT, POST and DELETE data. Every application which has CRUD(Create, Read, Update, Delete) operation has an API to create data, to get data, to update data or to delete data from database.

## HTTP (Hypertext Transfer Protocol)

### What is HTTP?

- HTTP is an established communication protocol between a client and a server. A client in this case is a browser and server is the place where you access data. HTTP is a network protocol used to deliver resources which could be files on the World Wide Web, whether they're HTML files, image files, query results, scripts, or other file types.
- A browser is an HTTP client because it sends requests to an HTTP server (Web server), which then sends responses back to the client.

### Structure of HTTP

- HTTP uses client-server model. An HTTP client opens a connection and sends a request message to an HTTP server and the HTTP server returns message which is the requested resources. When the request response cycle completes, the server closes the connection.
- The format of the request and response messages are similar. Both kinds of messages have:
  - An initial line
  - Zero or more header lines
  - A blank line (i.e. a CRLF by itself)
  - An optional message body (e.g. a file, or query data, or query output)

### Initial Request Line (Status Line)

- The initial request line is different from the response. A request line has three parts, separated by spaces:
  - Method name(GET, POST, HEAD)
  - Path of the requested resource
  - The version of HTTP being used.
  - e.g. GET/HTTP/1.1
- GET is the most common HTTP to get or read resource and POST a common request method to create resource.

### Initial Response Line (Status Line)

- The initial response line also has three parts separated by spaces:
  - HTTP version
  - Response status code that gives the result of the request
  - A reason which describes the status code
  - e.g. HTTP/1.0 200 OK, HTTP/1.0 404 Not Found
- The most common status codes are: 200 OK: The request succeeded, and the resulting resource (e.g. file or script output) is returned in the message body.
- A complete list of HTTP status code can be found [here](https://httpstatuses.com/).

### Header Fields

- Header lines provide information about the request or response, or about the object sent in the message body.

### The message body

- An HTTP message may have a body of data sent after the header lines. In a response, this is where the requested resource is returned to the client (the most common use of the message body), or perhaps explanatory text if there's an error. In a request, this is where user-entered data or uploaded files are sent to the server.
- If an HTTP message includes a body, there are usually header lines in the message that describe the body.
- In particular, the Content-Type: header gives the MIME-type of the data in the body(text/html, application/json, text/plain, text/css, image/gif). The Content-Length: header gives the number of bytes in the body.

### Request Methods

- The GET, POST, PUT and DELETE are the HTTP request methods which we are going to implement an API or a CRUD operation application.
  - GET: GET method is used to retrieve and get information from the given server using a given URI. Requests using GET should only retrieve data and should have no other effect on the data.
  - POST: POST request is used to create data and send data to the server, for example, creating a new post, file upload, etc. using HTML forms.
  - PUT: Replaces all current representations of the target resource with the uploaded content and we use it modify or update data.
  - DELETE: Removes data
