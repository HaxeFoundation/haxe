function needsInt(i:Int) {}

function take(?n:Int, ?f:String->Void) {}

function main() {
	take(a -> {
		needsInt(a);
	});
}
