package unit.issues;

private enum JobIdentifierImpl {
	ID(id:Int);
}

@:forward
private abstract JobIdentifier(JobIdentifierImpl) from JobIdentifierImpl {
	@:from static inline function fromID(id:Int):JobIdentifier {
		return ID(id);
	}
}

class Issue11694 extends unit.Test {
	static var callbackHandlers:Map<JobIdentifier, String> = new Map();

	function test() {
		callbackHandlers.set(5, "x");
		eq("x", callbackHandlers.get(5));
		var m = new Map<JobIdentifier, String>();
		m.set(1, "y");
		eq("y", m.get(1));
		f(m.exists(2));
	}
}
