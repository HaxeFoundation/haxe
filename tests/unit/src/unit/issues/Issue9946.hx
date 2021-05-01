package unit.issues;

#if cs
import cs.system.Attribute;
import haxe.test.AttrWithNullType;
import haxe.test.AttrWithNonNullType;
import haxe.test.MyAttrAttribute;
#end

class Issue9946 extends unit.Test {
	#if cs
	function test() {
		eq(hasNullArg(cs.Lib.toNativeType(AttrWithNullType)), true);
		eq(hasNullArg(cs.Lib.toNativeType(AttrWithNonNullType)), false);
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
	#end
}
