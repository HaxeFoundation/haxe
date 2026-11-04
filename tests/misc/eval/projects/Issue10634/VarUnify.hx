function take(f:()->Void) {}

function main() {
	take(() -> {
		var i:Int = "s";
	});
}
