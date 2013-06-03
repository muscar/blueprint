+!foo [context(async)] <- !bar.

+!social_plan[context(promise)]
<- facebook.get_wall_posts(WallPosts);
   window.alert(WallPosts);
   twitter.get_tweets(Tweets);
   window.alert(Tweets);
   !correlate(WallPosts, Tweets).