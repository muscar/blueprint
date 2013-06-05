var idx = 0;

function $term(tag) {
	this.tag = tag;
	this.idx = idx++;
	for (var i = 1; i < arguments.length; i++)
	{
		this["item" + i] = arguments[i]
	}
}

$term.prototype.toString = function() {
	if (this.item1 == undefined) {
		return "_G" + this.idx;
	}
	var val = this.item1;
	while (val instanceof $term && val.tag == -1) {
		val = val.item1;
	}
	return val.toString();
}

function $unbind(term) {
	term.item1 = undefined;
}

function $unify(left, right) {
	if (left.tag == -1) {
		if (left.item1 == undefined) {
			left.item1 = right;
			return true;
		} else {
			return $unify(left.item1, right);
		}
	} else if (right.tag == -1) {
		if (right.item1 == undefined) {
			right.item1 = left;
			return true;
		} else {
			return $unify(left, right.item1);
		}
	} else if (typeof(left) === typeof(right)) {
		return left === right;
	}
	return false;
}

// Promise lib

function promiseBuilder() {
	this.unit = function (x) {
		var promise = jQuery.Deferred();
		promise.resolve(x);
		return promise;
	}

	this.bind = function (m, f) {
		return m.then(f);
	}
}

var promise = new promiseBuilder();

// FB

function FB() {
	this.get_wall_posts = function (v) {
		var promise = jQuery.Deferred();
		setTimeout(function () {
			$unify(v, "facebook posts");
			promise.resolve();
		}, 1000);
		return promise;
	}
}

// Twitter

function Twitter() {
	this.get_tweets = function (v) {
		var promise = jQuery.Deferred();
		setTimeout(function () {
			$unify(v, "twitter tweets");
			promise.resolve();
		}, 1000);
		return promise;
	}
}

var facebook = new FB();
var twitter = new Twitter();
