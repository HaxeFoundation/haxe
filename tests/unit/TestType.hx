package unit;
import unit.MyEnum;

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

	public function testEnumEq() {
		t( Type.enumEq(null,null) );
		f( Type.enumEq(A,null) );
		f( Type.enumEq(null,D(A)) );

		t( Type.enumEq(A,A) );
		t( Type.enumEq(B,B) );
		f( Type.enumEq(A,B) );

		t( Type.enumEq(C(1,"hello"),C(1,"hello")) );
		f( Type.enumEq(C(1,"hello"),C(1,"hellox")) );

		t( Type.enumEq(D(A),D(A)) );
		f( Type.enumEq(D(A),D(B)) );

	}

}