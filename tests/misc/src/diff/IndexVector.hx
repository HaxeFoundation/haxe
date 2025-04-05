package diff;

import haxe.ds.Vector;

@:structInit
private class IV {
	public final v:Vector<Int>;
	public var index:Int;
}

abstract IndexVector(IV) {
	public inline function new(v:Vector<Int>, index:Int) {
		this = {
			v: v,
			index: index
		}
	}

	@:op(A += B)
	inline function addAssign(rhs:Int) {
		this.index += rhs;
	}

	@:op(A + B)
	inline function add(delta:Int) {
		return new IndexVector(this.v, this.index + delta);
	}

	@:op([])
	inline function read(delta:Int) {
		return this.v[this.index + delta];
	}

	@:op([])
	inline function write(delta:Int, v:Int) {
		return this.v[this.index + delta] = v;
	}

	public inline function toString() {
		return '[IV +${this.index}]';
	}
}
