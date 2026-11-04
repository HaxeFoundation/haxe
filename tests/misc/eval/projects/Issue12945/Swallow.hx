function take(?s:String, ?n:Int) {}

function main() {
	// `Dup.make()` skips ?s:String and binds to ?n:Int, but loading `Dup`
	// produces a real error that must still be reported.
	take(Dup.make());
}
