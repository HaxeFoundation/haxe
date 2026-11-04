function take(?s:String, ?cb:()->Void) {}

function main() {
	take(() -> {
		var i:Int = "s";
	});
}
