module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data PostsController
  = PostsAction
  | NewPostAction
  | ShowPostAction {postId :: !(Id Post)}
  | CreatePostAction
  | EditPostAction {postId :: !(Id Post)}
  | UpdatePostAction {postId :: !(Id Post)}
  | DeletePostAction {postId :: !(Id Post)}
  deriving (Eq, Show, Data)

data CommentsController
  = CommentsAction
  | NewCommentAction {postId :: !(Id Post)}
  | ShowCommentAction {commentId :: !(Id Comment)}
  | CreateCommentAction
  | EditCommentAction {commentId :: !(Id Comment)}
  | UpdateCommentAction {commentId :: !(Id Comment)}
  | DeleteCommentAction {commentId :: !(Id Comment)}
  deriving (Eq, Show, Data)
