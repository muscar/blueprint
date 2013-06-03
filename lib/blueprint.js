function $term(name, tag) {
	this.tag = tag;
	for (var i = 1; i < arguments.length; i++)
	{
		this["item" + i] = arguments[i]
	}
}

$term.prototype.toString = function()
{
    return "term";
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
