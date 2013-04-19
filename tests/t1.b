today(friday).

+!foo([]) <- +done.
+!foo([X|Xs])
<- !bar(X);
   !foo(Xs).

+!bar(X) : X & Y <- !baz(X).

+!p(X) [context(promise)]
<- !q(X).