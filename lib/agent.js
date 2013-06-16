function agent() {
    var $self = this;;
    this.social_plan = function () {
        var Statuses = new $variable('Statuses');
        var Words = new $variable('Words');
        Statuses.unbind();
        Words.unbind();
        if (true) {
            promise.bind(facebook.statuses(Statuses), function () {
                $self.collect(Statuses, $nil, Words);
                $self.count_words(Words);
            })
        } else {
            $error("plan social_plan failed")
        }
    };
    this.foo = function () {;
        if (true) {
            $self.bar()
        } else {
            $error("plan foo failed")
        }
    };
    this.count_words = function (param0) {
        var D = new $variable('D');
        var Ws = new $variable('Ws');
        D.unbind();
        Ws.unbind();
        if ($unify(param0, Ws) && true) {
            $unify(blueprint.lang.dict.make(), D);
            $self.count(Ws, D)
        } else {
            $error("plan count_words failed")
        }
    };
    this.count = function (param0, param1) {
        var C = new $variable('C');
        var D = new $variable('D');
        var W = new $variable('W');
        var Ws = new $variable('Ws');
        C.unbind();
        D.unbind();
        W.unbind();
        Ws.unbind();
        if ($unify(param0, $nil) && $unify(param1, D) && true) {
            window.makeCloud("#cloud", D.getValue())
        } else {
            C.unbind();
            D.unbind();
            W.unbind();
            Ws.unbind();
            if ($unify(param0, new $cons(W, Ws)) && $unify(param1, D) && true) {
                $unify(blueprint.lang.dict.find_default(D.getValue(), W.getValue(), 0), C);
                blueprint.lang.dict.add(D.getValue(), W.getValue(), C.getValue() + 1);
                $self.count(Ws, D)
            } else {
                $error("plan count failed")
            }
        }
    };
    this.collect = function (param0, param1, param2) {
        var Acc = new $variable('Acc');
        var Acc1 = new $variable('Acc1');
        var R = new $variable('R');
        var Status = new $variable('Status');
        var Statuses = new $variable('Statuses');
        var Words = new $variable('Words');
        Acc.unbind();
        Acc1.unbind();
        R.unbind();
        Status.unbind();
        Statuses.unbind();
        Words.unbind();
        if ($unify(param0, $nil) && $unify(param1, Acc) && $unify(param2, Acc) && true) {} else {
            Acc.unbind();
            Acc1.unbind();
            R.unbind();
            Status.unbind();
            Statuses.unbind();
            Words.unbind();
            if ($unify(param0, new $cons(Status, Statuses)) && $unify(param1, Acc) && $unify(param2, R) && true) {
                $unify(blueprint.lang.str.split(Status.getValue()), Words);
                $unify(blueprint.lang.list.concat(Acc.getValue(), Words.getValue()), Acc1);
                $self.collect(Statuses, Acc1, R)
            } else {
                $error("plan collect failed")
            }
        }
    };
}
