package unit.issues;

import utest.Assert;

#if cpp
private enum MyEnum {
	EnumTest(p:cpp.Pointer<Int>);
}
#end

class Issue10831 extends Test {
	#if cpp
	function test() {
		switch (get()) {
			case EnumTest(p):
				trace(p);
		}
		Assert.pass();
	}

	function get() {
		return EnumTest(null);
	}
	#end
}
