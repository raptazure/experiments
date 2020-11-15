
#ifndef db_Article_hpp
#define db_Article_hpp

#include "oatpp/core/Types.hpp"

#include <string>
#include <list>

/**
 *  Object of Article stored in the Demo-Database
 */
class Article
{
public:
  v_int32 id;
  oatpp::String title;
  oatpp::String body;
};

#endif /* db_Article_hpp */
