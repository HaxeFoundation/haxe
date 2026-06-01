function take(?cb:()->Void, ?n:Int) {}

function main() {
	take(() -> {
		var i:Int = "s";
	});
}
