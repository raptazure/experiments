# oatpp-blog-api [![Build Status](https://dev.azure.com/lganzzzo/lganzzzo/_apis/build/status/oatpp.example-crud?branchName=master)](https://dev.azure.com/lganzzzo/lganzzzo/_build?definitionId=9?branchName=master)


See more:

- [Oat++ Website](https://oatpp.io/)
- [Oat++ Github Repository](https://github.com/oatpp/oatpp)
- [Get Started](https://oatpp.io/docs/start)

## Overview

This project is using [oatpp](https://github.com/oatpp/oatpp) and [oatpp-swagger](https://github.com/oatpp/oatpp-swagger) modules.

### Project layout

```
|- CMakeLists.txt                        // projects CMakeLists.txt
|- src/
|   |
|   |- controller/                       // Folder containing ArticleController where all endpoints are declared
|   |- db/                               // Folder with database mock
|   |- dto/                              // DTOs are declared here
|   |- SwaggerComponent.hpp              // Swagger-UI config
|   |- AppComponent.hpp                  // Service config
|   |- App.cpp                           // main() is here
|
|- test/                                 // test folder
|- utility/install-oatpp-modules.sh      // utility script to install required oatpp-modules.
```

---

### Build and Run

#### Using CMake

**Requires**

- `oatpp` and `oatpp-swagger` modules installed. You may run `utility/install-oatpp-modules.sh` 
script to install required oatpp modules.

```
$ mkdir build && cd build
$ cmake ..
$ make 
$ ./crud-exe        # - run application.
```

#### In Docker

```
$ docker build -t example-crud .
$ docker run -p 8000:8000 -t example-crud
```

---

### Endpoints declaration

#### Create Article

```c++
ENDPOINT_INFO(createArticle) {
  info->summary = "Create new Article";
  info->addConsumes<Object<ArticleDto>>("application/json");
  info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
}
ENDPOINT("POST", "api/articles", createArticle,
         BODY_DTO(Object<ArticleDto>, articleDto)) {
  return createDtoResponse(Status::CODE_200, m_database->createArticle(articleDto));
}
```

#### Update Article

```c++
ENDPOINT_INFO(putArticle) {
  info->summary = "Update Article by id";
  info->addConsumes<Object<ArticleDto>>("application/json");
  info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
  info->addResponse<String>(Status::CODE_404, "text/plain");
}
ENDPOINT("PUT", "api/articles/{id}", putArticle,
         PATH(Int32, id),
         BODY_DTO(Object<ArticleDto>, articleDto)) {
  articleDto->id = id;
  return createDtoResponse(Status::CODE_200, m_database->updateArticle(articleDto));
}
```

#### Get one Article

```c++
ENDPOINT_INFO(getArticleById) {
  info->summary = "Get one Article by id";
  info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
  info->addResponse<String>(Status::CODE_404, "text/plain");
}
ENDPOINT("GET", "api/articles/{id}", getArticleById,
         PATH(Int32, id)) {
  auto article = m_database->getArticleById(id);
  OATPP_ASSERT_HTTP(article, Status::CODE_404, "Article not found");
  return createDtoResponse(Status::CODE_200, article);
}
```

#### Get list of articles

```c++
ENDPOINT_INFO(getArticles) {
  info->summary = "get all stored articles";
  info->addResponse<List<Object<ArticleDto>>>(Status::CODE_200, "application/json");
}
ENDPOINT("GET", "api/articles", getArticles) {
  return createDtoResponse(Status::CODE_200, m_database->getArticles());
}
```

#### Delete Article
```c++
ENDPOINT_INFO(deleteArticle) {
  info->summary = "Delete Article by id";
  info->addResponse<String>(Status::CODE_200, "text/plain");
  info->addResponse<String>(Status::CODE_404, "text/plain");
}
ENDPOINT("DELETE", "api/articles/{id}", deleteArticle,
         PATH(Int32, id)) {
  bool success = m_database->deleteArticle(id);
  OATPP_ASSERT_HTTP(success, Status::CODE_417, "Article not deleted. Perhaps no such Article in the Database");
  return createResponse(Status::CODE_200, "Article successfully deleted");
}  
```
