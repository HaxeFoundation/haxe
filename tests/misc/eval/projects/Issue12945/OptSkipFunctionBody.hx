function doStuff( ?onStart : Void -> Bool, ?onEnd : Void -> Void ) {}

function main() {
	// `() -> {}` returns Void, so it must NOT bind to onStart (Void -> Bool);
	// the optional onStart is skipped and the lambda binds to onEnd (Void -> Void).
	doStuff(() -> {});
}
