today(friday).

+!foo([]) <- +done.
+!foo([X|Xs])
<- !bar(X);
   !foo(Xs).

+!p(X) [context(promise)]
<- !q(X).