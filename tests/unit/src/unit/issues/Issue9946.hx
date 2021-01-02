#if cs
package unit.issues;

import cs.system.Attribute;
import haxe.test.MyAttrAttribute;

class Issue9946 extends unit.Test {
	function test() {
		eq(hasNullArg(untyped __cs__('typeof(haxe.test.AttrWithNullType)')), true);
		eq(hasNullArg(untyped __cs__('typeof(haxe.test.AttrWithNonNullType)')), false);
	}

	static function hasNullArg(cls:cs.system.Type):Null<Bool> {
		var attributes = @:privateAccess new Array(Attribute.GetCustomAttributes(cls));

		for (attr in attributes) {
			if (Std.isOfType(attr, MyAttrAttribute)) {
				var a:MyAttrAttribute = cast attr;
				return a.check;
			}
		}

		return null;
	}
}
#end
