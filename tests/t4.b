+!foo [context(async)] <- !bar.

+!social_plan[context(promise)]
<- do facebook.get_wall_posts(WallPosts);
   window.alert(WallPosts);
   do twitter.get_tweets(Tweets);
   window.alert(Tweets);
   !correlate(WallPosts, Tweets).