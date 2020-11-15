
#ifndef Database_hpp
#define Database_hpp

#include "../dto/ArticleDto.hpp"
#include "model/Article.hpp"

#include "oatpp/core/concurrency/SpinLock.hpp"
#include <unordered_map>

/**
 *  Trivial in-memory Database based on unordered_map container.
 */
class Database
{
private:
  oatpp::concurrency::SpinLock m_lock;
  v_int32 m_idCounter;                                 ///< counter to generate articleIds
  std::unordered_map<v_int32, Article> m_articlesById; ///< Map id to Article
private:
  Article serializeFromDto(const oatpp::Object<ArticleDto> &articleDto);
  oatpp::Object<ArticleDto> deserializeToDto(const Article &article);

public:
  Database()
      : m_idCounter(0)
  {
  }

  oatpp::Object<ArticleDto> createArticle(const oatpp::Object<ArticleDto> &articleDto);
  oatpp::Object<ArticleDto> updateArticle(const oatpp::Object<ArticleDto> &articleDto);
  oatpp::Object<ArticleDto> getArticleById(v_int32 id);
  oatpp::List<oatpp::Object<ArticleDto>> getArticles();
  bool deleteArticle(v_int32 id);
};

#endif /* Database_hpp */
