
#include "Database.hpp"

Article Database::serializeFromDto(const oatpp::Object<ArticleDto> &articleDto)
{
  Article article;
  if (articleDto->id)
  {
    article.id = *articleDto->id;
  }
  article.title = articleDto->title;
  article.body = articleDto->body;
  return article;
}

oatpp::Object<ArticleDto> Database::deserializeToDto(const Article &article)
{
  auto dto = ArticleDto::createShared();
  dto->id = article.id;
  dto->title = article.title;
  dto->body = article.body;

  return dto;
}

oatpp::Object<ArticleDto> Database::createArticle(const oatpp::Object<ArticleDto> &articleDto)
{
  std::lock_guard<oatpp::concurrency::SpinLock> lock(m_lock);
  auto article = serializeFromDto(articleDto);
  article.id = m_idCounter++;
  m_articlesById[article.id] = article;
  return deserializeToDto(article);
}

oatpp::Object<ArticleDto> Database::updateArticle(const oatpp::Object<ArticleDto> &articleDto)
{
  std::lock_guard<oatpp::concurrency::SpinLock> lock(m_lock);
  auto article = serializeFromDto(articleDto);
  if (article.id < 0)
  {
    throw std::runtime_error("Article Id cannot be less than 0");
  }
  auto it = m_articlesById.find(article.id);
  if (it != m_articlesById.end())
  {
    m_articlesById[article.id] = article;
  }
  else
  {
    throw new std::runtime_error("Such article not found");
  }
  return deserializeToDto(article);
}

oatpp::Object<ArticleDto> Database::getArticleById(v_int32 id)
{
  std::lock_guard<oatpp::concurrency::SpinLock> lock(m_lock);
  auto it = m_articlesById.find(id);
  if (it == m_articlesById.end())
  {
    return nullptr;
  }
  return deserializeToDto(it->second);
}

oatpp::List<oatpp::Object<ArticleDto>> Database::getArticles()
{
  std::lock_guard<oatpp::concurrency::SpinLock> lock(m_lock);
  oatpp::List<oatpp::Object<ArticleDto>> result({});
  auto it = m_articlesById.begin();
  while (it != m_articlesById.end())
  {
    result->push_back(deserializeToDto(it->second));
    it++;
  }
  return result;
}

bool Database::deleteArticle(v_int32 id)
{
  std::lock_guard<oatpp::concurrency::SpinLock> lock(m_lock);
  auto it = m_articlesById.find(id);
  if (it == m_articlesById.end())
  {
    return false;
  }
  m_articlesById.erase(it);
  return true;
}
