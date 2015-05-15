package unit.issues;

private enum MyEnum2 {
    MyEnumValue(p:String);
}

class Issue2988 extends Test {
	function test() {
        var a : Dynamic = MyEnumValue("foo");
		var s = "";
        if( (a is MyEnum2) ){
            switch( a ){
                case MyEnumValue(s1): s = s1;
            }
        }
		eq("foo", s);
	}
}