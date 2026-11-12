abstract RO(Array<Int>) {
	public function new() this = [1, 2, 3];
	@:arrayAccess inline function get(i:Int):Int return this[i];
}

function main() {
	var a = new RO();
	a[0] = 42;
	a[1] += 10;
}
