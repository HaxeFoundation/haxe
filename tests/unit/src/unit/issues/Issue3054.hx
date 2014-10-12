package unit.issues;

private enum MyEnum {
	A;
	B;
}

private enum MyOtherEnum {
	A(e:Null<MyEnum>);
	B;
}

@:analyzer(no_check_has_effect)
class Issue3054 extends Test {
	function test() {
		var myValue:Null<MyEnum> = A;
		switch(myValue) {
			case A:
			case B:
		}

		var myOtherValue:Null<MyOtherEnum> = B;
		t(unit.TestType.typeError(switch(myOtherValue) {
			case A(A):
			case A(B):
			case B:
		}));
	}
}