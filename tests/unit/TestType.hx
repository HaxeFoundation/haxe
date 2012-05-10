package unit;
import unit.MyEnum;
import unit.MyClass;

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
	
	function testWiderVisibility() {
		var c = new MyClass.MyChild1();
		eq(12, c.a());
	}
	
	function testUnifyMin() {
		#if !macro

		// array
		
		var ti1:Array<I1>;
		var tbase:Array<Base>;
		var tpbase:Array<PClassBase<Float>>;
		#if (flash9 || cpp)
		var tnullbool:Array<Null<Bool>>;
		var tnullbase:Array<Null<Base>>;
		#else
		var tnullbool:Array<Bool>;
		var tnullbase:Array<Base>;
		#end
		var tchild1:Array<Child1>;
		var ts:Array<{s:String}>;
		
		Test.typedAs([new Child1(), new Child2()], tbase);
		Test.typedAs([new Child1(), new Child2(), new Base()], tbase);
		Test.typedAs([new Child1(), new Child2_1(), new Base()], tbase);	
		Test.typedAs([new Child2(), new Unrelated()], ti1);
		Test.typedAs([new Child2_1(), new Unrelated()], ti1);

		Test.typedAs([new ClassI2(), new Child2()], ti1);
		Test.typedAs([new CI1(), new CI2()], tbase);
		Test.typedAs([new CII1(), new CII2()], tbase);
		
		Test.typedAs([new PClass1(), new PClass2(2.0)], tpbase);
		
		Test.typedAs([null, false], tnullbool);
		Test.typedAs([false, null], tnullbool);
		Test.typedAs([null, new Base()], tnullbase);
		//Test.typedAs([new Base(), null], tnullbase); // TODO: this fails on flash9 and cpp
		Test.typedAs([new Base()], tbase);
		Test.typedAs([new Base(), new Child1()], tbase);
		Test.typedAs([new Child1(), new Base()], tbase);
		Test.typedAs([new Child1(), new Child1()], tchild1);
		Test.typedAs([ { s:"foo" }, new Unrelated()], ts);
		Test.typedAs([new Unrelated(), { s:"foo" } ], ts);

		// if
		
		var tbase:Base;
		var ti1:I1;
		#if (flash9 || cpp)
		var tnullbool:Null<Bool>;
		#else
		var tnullbool:Bool;
		#end
		var ts: { s:String };
		
		Test.typedAs(if (false) new Child1(); else new Child2(), tbase);
		Test.typedAs(
			if (false) new Child1();
			else if (true) new Child2();
			else new Base(), tbase);
		Test.typedAs(
			if (false) new Child1();
			else if (true) new Child2_1();
			else new Base(), tbase);
		Test.typedAs(if (false) new Child2(); else new Unrelated(), ti1);
		Test.typedAs(if (false) new Child2_1(); else new Unrelated(), ti1);
		
		Test.typedAs(if (false) null; else false, tnullbool);
		Test.typedAs(if (false) true; else null, tnullbool);
		Test.typedAs(if (false) new Unrelated(); else {s:"foo"}, ts);
		Test.typedAs(if (false) { s:"foo" }; else new Unrelated(), ts);
		
		//switch
		
		Test.typedAs(switch(false) { case true: new Child1(); case false: new Child2(); }, tbase);
		Test.typedAs(switch(1) { case 0: new Child1(); case 1: new Child2(); case 2: new Base(); }, tbase);
		Test.typedAs(switch(1) { case 0: new Child1(); case 1: new Child2_1(); default: new Base(); }, tbase);
		Test.typedAs(switch(false) { case true: new Child2(); case false: new Unrelated(); }, ti1);
		Test.typedAs(switch(false) { case true: new Child2_1(); case false: new Unrelated(); }, ti1);
		
		Test.typedAs(switch(false) { case true: null; default: false; }, tnullbool);
		Test.typedAs(switch(false) { case true: true; default: null; }, tnullbool);
		Test.typedAs(switch(false) { case true: new Unrelated(); default: {s:"foo"}; }, ts);
		Test.typedAs(switch(false) { case true: { s:"foo" }; default: new Unrelated(); }, ts);
		
		// return
		
		Test.typedAs(function() { return new Child1(); return new Child2(); } (), tbase);
		Test.typedAs(function() { return new Child1(); return new Child2(); return new Base(); } (), tbase);
		Test.typedAs(function() { return new Child1(); return new Child2_1(); return new Base(); } (), tbase);
		Test.typedAs(function() { return new Child2(); return new Unrelated(); } (), ti1);
		Test.typedAs(function() { return new Child2_1(); return new Unrelated(); } (), ti1);
		
		Test.typedAs(function() { return null; return false; } (), tnullbool);
		Test.typedAs(function() { return true; return null; } (), tnullbool);
		Test.typedAs(function() { return new Unrelated(); return {s:"foo"}; } (), ts);
		Test.typedAs(function() { return {s:"foo"}; return new Unrelated(); } (), ts);
		#end
		
	}
}