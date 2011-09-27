package unit;
import unit.MyEnum;

class TestType extends Test {

	static inline function u( s : String ) : String {
		#if flash
		return untyped __unprotect__(s);
		#else
		return s;
		#end
	}

	public function testType() {
		var name = u("unit")+"."+u("MyClass");
		eq( Type.resolveClass(name), unit.MyClass );
		eq( Type.getClassName(unit.MyClass), name );
		eq( Type.getClassFields(unit.MyClass).length , 0 );
	}

	public function testFields() {
		var sfields = Type.getClassFields(unit.MySubClass);
		eq( sfields.length , 1 );
		eq( sfields[0], u("XXX") );

		var fields = [u("add"),u("get"),u("intValue"),u("ref"),u("set"),u("stringValue"),u("val")];
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
	
	function testPossibleBug() {
		var c = Type.getEnumConstructs(MyEnum);
		var old = c[0];
		c[0] = "modified";
		eq( Type.getEnumConstructs(MyEnum)[0], old );
		
		var i = Type.getInstanceFields(TestType);
		var old = i[0];
		i[0] = "modified";
		eq( Type.getInstanceFields(TestType)[0], old );
		
		var i = Type.getClassFields(TestType);
		var old = i[0];
		i[0] = "modified";
		eq( Type.getClassFields(TestType)[0], old );
		
		// we don't check for Type.enumParameters modifications :
		// we want it to be as fast as possible even if it references
		// the current enum - since it's not cachable
	}
	
	function testAllField() {
		eq( Type.allEnums(MyEnum).join("#"), "A#B" );
	}
	
}