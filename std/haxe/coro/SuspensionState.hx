package haxe.coro;

@:using(SuspensionState.SuspensionStateTools)
enum abstract SuspensionState(Int) {
	final Pending;
	final Returned;
	final Thrown;
}

class SuspensionStateTools {
	static public function toString(c:SuspensionState) {
		return switch (c) {
			case Pending: "Pending";
			case Returned: "Returned";
			case Thrown: "Thrown";
		}
	}
}