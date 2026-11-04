function take(?n:Int, ?f:Void->Int) {}

function main() {
	take(() -> {
		({ a: 1 }).noField;
	});
}
