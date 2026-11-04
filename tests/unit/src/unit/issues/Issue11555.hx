package unit.issues;

private class Promise<T> {
	public function new() {}

	public function getResult():PromiseResult<T> {
		return VALUE(null, 12.5, this);
	}
}

private enum PromiseResult<T> {
	VALUE(result:T, time:Float, promise:Promise<T>);
	FAILURE(ex:Dynamic, time:Float, promise:Promise<T>);
	PENDING(promise:Promise<T>);
}

class Issue11555 extends unit.Test {
	function test() {
		var p = new Promise<Void>();
		var s = switch (p.getResult()) {
			case VALUE(_, time, promise):
				t(promise == p);
				'value $time';
			case FAILURE(_, time, _):
				'failure $time';
			case PENDING(_):
				'pending';
		}
		eq("value 12.5", s);
	}
}
