package unit.issues;

import utest.Assert;

private var count = 0;

private abstract MyInt(Int) from Int to Int {
	@:op(A += B)
	public inline function addEq(x:Int):MyInt {
		count++;
		return this;
	}

	@:op(A++)
	public inline function increment():MyInt {
		count++;
		return this;
	}
}

class Issue10845 extends Test {
	final field:MyInt = 1;

	function test():Void {
		final local:MyInt = 1;

		field++;
		eq(1, count);
		local++;
		eq(2, count);

		field += 1;
		eq(3, count);
		local += 1;
		eq(4, count);

		field.addEq(1);
		eq(5, count);
		local.addEq(1);
		eq(6, count);

		field.increment();
		eq(7, count);
		local.increment();
		eq(8, count);
	}
}
