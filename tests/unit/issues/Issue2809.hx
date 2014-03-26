package unit.issues;
import unit.Test;

private enum MyEnum {
	SomeValue;
	DifferentValue;
}

class Issue2809 extends Test {
	function test() {
		var val:Dynamic = MyEnum.DifferentValue;
		var x = "foo";
		switch(val) {
			case MyEnum.SomeValue:
				x = "bar";
		}
		eq("foo", x);
	}
}