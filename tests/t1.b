today(friday).

+!foo([]) <- +done.
+!foo([X|Xs])
<- !bar(X);
   !foo(Xs).

+!bar(X, Y) : X & Y <- !baz(X, Y).

+!p(X) [context(promise)]
<- !q(pair(X, 1)).

+!test <- !testaux(pair(1, 2)).