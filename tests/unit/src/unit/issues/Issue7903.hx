package unit.issues;

import utest.Assert;

class Issue7903 extends unit.Test {
	static var tmp:String;
	function test() {
		//Test recursive objects
		var o = {rec:(null:Dynamic)};
		o.rec = o;
		try {
			tmp = Std.string(o);
			Assert.pass();
		} catch(e:Dynamic) {
			Assert.fail('Failed to stringify an object with recursive reference: $e');
		}

		//Test recursive enums
		var o = {rec:(null:RecursiveEnum)};
		var e = Ctor(o);
		o.rec = e;
		try {
			tmp = Std.string(e);
			Assert.pass();
		} catch(e:Dynamic) {
			Assert.fail('Failed to stringify an enum instance with recursive reference: $e');
		}

		//Test recursive arrays
		var a:Array<Dynamic> = [];
		a.push(a);
		try {
			tmp = Std.string(a);
			Assert.pass();
		} catch(e:Dynamic) {
			Assert.fail('Failed to stringify a recursive array: $e');
		}
	}
}

private enum RecursiveEnum {
	Ctor(o:{rec:Null<RecursiveEnum>});
}