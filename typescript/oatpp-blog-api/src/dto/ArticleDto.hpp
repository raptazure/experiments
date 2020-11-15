#ifndef ArticleDto_hpp
#define ArticleDto_hpp

#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/Types.hpp"

#include OATPP_CODEGEN_BEGIN(DTO)

/**
 *  Data Transfer Object. Object containing fields only.
 *  Used in API for serialization/deserialization and validation
 */
class ArticleDto : public oatpp::DTO
{

  DTO_INIT(ArticleDto, DTO)

  DTO_FIELD(Int32, id);
  DTO_FIELD(String, title, "title");
  DTO_FIELD(String, body, "body");
};

#include OATPP_CODEGEN_END(DTO)

#endif /* ArticleDto_hpp */
