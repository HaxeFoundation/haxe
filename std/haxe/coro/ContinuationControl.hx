package haxe.coro;

@:using(ContinuationControl.ContinuationControlTools)
enum abstract ContinuationControl(Int) {
	final Pending;
	final Returned;
	final Thrown;
}

class ContinuationControlTools {
	static public function toString(c:ContinuationControl) {
		return switch (c) {
			case Pending: "Pending";
			case Returned: "Returned";
			case Thrown: "Thrown";
		}
	}
}