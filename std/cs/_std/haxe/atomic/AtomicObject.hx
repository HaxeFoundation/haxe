package haxe.atomic;

import cs.system.threading.Interlocked.*;

private class ObjectWrapper<T:{}> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}
}

extern abstract AtomicObject<T:{}>(ObjectWrapper<T>) {
	public inline function new(value:T) {
		this = new ObjectWrapper(value);
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		return cs.Syntax.code("System.Threading.Interlocked.CompareExchange(ref ({0}).value, {1}, {2})", this, replacement, expected);
	}

	public inline function exchange(value:T):T {
		return cs.Syntax.code("System.Threading.Interlocked.Exchange(ref ({0}).value, {1})", this, value);
	}

	public inline function load():T {
		return this.value; // according to the CLI spec reads and writes are atomic
	}

	public inline function store(value:T):T {
		return this.value = value; // according to the CLI spec reads and writes are atomic
	}
}