+!social_plan[context(promise)]
<- do facebook.statuses(Statuses);
   !collect(Statuses, [], Words);
   !count_words(Words).

+!collect([], Acc, Acc).
+!collect([Status|Statuses], Acc, R)
<- blueprint.lang.str.split(Status, Words);
   blueprint.lang.list.concat(Acc, Words, Acc1);
   !collect(Statuses, Acc1, R).

+!count([], D)
<- window.makeCloud("#cloud", D, _).
+!count([W|Ws], D)
<- blueprint.lang.dict.find_default(D, W, 0, C);
   blueprint.lang.dict.add(D, W, C + 1, _);
   !count(Ws, D).

+!count_words(Ws)
<- blueprint.lang.dict.make(D);
   !count(Ws, D).
