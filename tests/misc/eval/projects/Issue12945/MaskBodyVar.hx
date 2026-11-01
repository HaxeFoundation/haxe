function take(?n:Int, ?f:String->Void) {}

function main() {
	take(a -> {
		var i:Int = a;
	});
}
