function take(f:()->Void) {}

function main() {
	take(() -> {
		var c = 5;
		var x = c[0];
	});
}
