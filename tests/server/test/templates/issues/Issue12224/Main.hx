function main() {
	var arr = [1];
	arr.remove(1);
	log(arr);
	if (arr.length > 0)
		throw "no remove";

	final ints:Array<Int> = [];
	ints.push(0);
	for (value in ints) {
		log(value);
		ints.remove(value);
	}
	if (ints.length > 0)
		throw "no remove";
}

@:pure(false)
function log(v:Any):Void {}
