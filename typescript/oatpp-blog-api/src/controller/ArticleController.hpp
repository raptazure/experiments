
#ifndef ArticleController_hpp
#define ArticleController_hpp

#include "../db/Database.hpp"

#include "oatpp/web/server/api/ApiController.hpp"
#include "oatpp/parser/json/mapping/ObjectMapper.hpp"
#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/macro/component.hpp"

/**
 *  EXAMPLE ApiController
 *  Basic examples of howto create ENDPOINTs
 *  More details on oatpp.io
 */
class ArticleController : public oatpp::web::server::api::ApiController
{
public:
  ArticleController(const std::shared_ptr<ObjectMapper> &objectMapper)
      : oatpp::web::server::api::ApiController(objectMapper)
  {
  }

private:
  /**
   *  Inject Database component
   */
  OATPP_COMPONENT(std::shared_ptr<Database>, m_database);

public:
  /**
   *  Inject @objectMapper component here as default parameter
   *  Do not return bare Controllable* object! use shared_ptr!
   */
  static std::shared_ptr<ArticleController> createShared(OATPP_COMPONENT(std::shared_ptr<ObjectMapper>,
                                                                         objectMapper))
  {
    return std::make_shared<ArticleController>(objectMapper);
  }

  /**
   *  Begin ENDPOINTs generation ('ApiController' codegen)
   */
#include OATPP_CODEGEN_BEGIN(ApiController)

  /**
   *  Insert Your endpoints here !!!
   */

  ENDPOINT_INFO(root)
  {
    info->summary = "Index.html page";
    info->addResponse<String>(Status::CODE_200, "text/html");
  }
  ADD_CORS(root)
  ENDPOINT("GET", "/", root)
  {
    const char *html =
        "<html lang='en'>"
        "<head>"
        "<meta charset=utf-8/>"
        "</head>"
        "<body>"
        "<a href='swagger/ui'>Checkout Swagger-UI page</a>"
        "</body>"
        "</html>";
    auto response = createResponse(Status::CODE_200, html);
    response->putHeader(Header::CONTENT_TYPE, "text/html");
    return response;
  }

  ENDPOINT_INFO(createArticle)
  {
    info->summary = "Create new Article";
    info->addConsumes<Object<ArticleDto>>("application/json");
    info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
  }

  ADD_CORS(createArticle)
  ENDPOINT("POST", "api/articles", createArticle,
           BODY_DTO(Object<ArticleDto>, articleDto))
  {
    return createDtoResponse(Status::CODE_200, m_database->createArticle(articleDto));
  }

  ENDPOINT_INFO(putArticle)
  {
    // general
    info->summary = "Update Article by id";
    info->addConsumes<Object<ArticleDto>>("application/json");
    info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
    info->addResponse<String>(Status::CODE_404, "text/plain");
    // params specific
    info->pathParams["id"].description = "Article Identifier";
  }

  ADD_CORS(putArticle, "*", "GET, POST, PUT, DELETE")
  ENDPOINT("PUT", "api/articles/{id}", putArticle,
           PATH(Int32, id),
           BODY_DTO(Object<ArticleDto>, articleDto))
  {
    articleDto->id = id;
    return createDtoResponse(Status::CODE_200, m_database->updateArticle(articleDto));
  }

  ENDPOINT_INFO(getArticleById)
  {
    // general
    info->summary = "Get one Article by id";
    info->addResponse<Object<ArticleDto>>(Status::CODE_200, "application/json");
    info->addResponse<String>(Status::CODE_404, "text/plain");
    // params specific
    info->pathParams["id"].description = "Article Identifier";
  }
  ADD_CORS(getArticleById)
  ENDPOINT("GET", "api/articles/{id}", getArticleById,
           PATH(Int32, id))
  {
    auto article = m_database->getArticleById(id);
    OATPP_ASSERT_HTTP(article, Status::CODE_404, "Article not found");
    return createDtoResponse(Status::CODE_200, article);
  }

  ENDPOINT_INFO(getArticles)
  {
    info->summary = "get all stored articles";
    info->addResponse<List<Object<ArticleDto>>>(Status::CODE_200, "application/json");
  }
  ADD_CORS(getArticles)
  ENDPOINT("GET", "api/articles", getArticles)
  {
    return createDtoResponse(Status::CODE_200, m_database->getArticles());
  }

  ENDPOINT_INFO(deleteArticle)
  {
    // general
    info->summary = "Delete Article by id";
    info->addResponse<String>(Status::CODE_200, "text/plain");
    info->addResponse<String>(Status::CODE_404, "text/plain");
    // params specific
    info->pathParams["id"].description = "Article Identifier";
  }
  ADD_CORS(deleteArticle, "*", "GET, POST, PUT, DELET")
  ENDPOINT("DELETE", "api/articles/{id}", deleteArticle,
           PATH(Int32, id))
  {
    bool success = m_database->deleteArticle(id);
    OATPP_ASSERT_HTTP(success, Status::CODE_417, "Article not deleted. Perhaps no such Article in the Database");
    return createResponse(Status::CODE_200, "Article successfully deleted");
  }

  /**
   *  Finish ENDPOINTs generation ('ApiController' codegen)
   */
#include OATPP_CODEGEN_END(ApiController)
};

#endif /* ArticleController_hpp */
