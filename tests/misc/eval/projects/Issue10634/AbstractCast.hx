abstract A(Int) {
	public function new() this = 0;
}

function take(f:()->Void) {}

function main() {
	take(() -> {
		var a:A = "nope";
	});
}
