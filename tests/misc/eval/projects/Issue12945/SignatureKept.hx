function take(?f:String->Void) {}

function main() {
	// A function-vs-function signature mismatch (Int vs String parameter) is a real
	// error and must still be reported alongside the in-place body error, even though
	// the body errors too — unlike a function passed to a non-function parameter.
	take((x:Int) -> aaa);
}
