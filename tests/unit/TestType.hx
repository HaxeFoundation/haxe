package unit;

class TestType extends Test {

	public function testType() {
		eq( Type.resolveClass("unit.MyClass"), unit.MyClass );
		eq( Type.getClassName(unit.MyClass), "unit.MyClass" );
		eq( Type.getClassFields(unit.MyClass).length , 0 );
	}


	public function testFields() {
		var sfields = Type.getClassFields(unit.MySubClass);
		eq( sfields.length , 1 );
		eq( sfields[0], "XXX" );

		var fields = ["add","get","intValue","ref","set","stringValue","val"];
		var fl = Type.getInstanceFields(unit.MyClass);
		fl.sort(Reflect.compare);
		eq( fl.join("|"), fields.join("|") );
		var fl = Type.getInstanceFields(unit.MySubClass);
		fl.sort(Reflect.compare);
		eq( fl.join("|"), fields.join("|") );
	}

}