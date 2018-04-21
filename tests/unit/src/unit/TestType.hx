package unit;

import haxe.ds.List;
import unit.MyEnum;
import unit.MyClass;
import unit.HelperMacros.*;

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

		// AS3 generator will create native properties
		#if !as3

		// x should not be listed since it's not a variable
		var fl = Type.getInstanceFields(VarProps);
		var fields = ["get_x","get_y","set_x","set_y","set_z","y","z"];
		fl.sort(Reflect.compare);
		eq( fl.join("|"), fields.join("|"));

		// same for statics
		var fl = Type.getClassFields(VarProps);
		var fields = ["SY", "get_SX", "get_SY", "set_SX", "set_SY"];
		fl.sort(Reflect.compare);
		eq( fl.join("|"), fields.join("|"));

		#end
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

		// TODO: this is also a problem
		#if !as3
		var mc2 = new MyChild2();
		eq(21, mc2.test1(new MyChild1()));
		#end
	}

	function testUnifyMin() {
		// array

		var ti1:Array<I1>;
		var tbase:Array<Base>;
		var tpbase:Array<PClassBase<Float>>;
		var tnullbool:Array<Null<Bool>>;
		var tnullbase:Array<Null<Base>>;
		var tchild1:Array<Child1>;
		var ts:Array<{s:String}>;

		typedAs([new Child1(), new Child2()], tbase);
		typedAs([new Child1(), new Child2(), new Base()], tbase);
		typedAs([new Child1(), new Child2_1(), new Base()], tbase);
		typedAs([new Child2(), new Unrelated()], ti1);
		typedAs([new Child2_1(), new Unrelated()], ti1);

		typedAs([new ClassI2(), new Child2()], ti1);
		typedAs([new CI1(), new CI2()], tbase);
		typedAs([new CII1(), new CII2()], tbase);

		typedAs([new PClass1(), new PClass2(2.0)], tpbase);

		typedAs([null, false], tnullbool);
		typedAs([false, null], tnullbool);
		typedAs([null, new Base()], tnullbase);
		typedAs([new Base(), null], tnullbase);
		typedAs([new Base()], tbase);
		typedAs([new Base(), new Child1()], tbase);
		typedAs([new Child1(), new Base()], tbase);
		typedAs([new Child1(), new Child1()], tchild1);
		typedAs([ { s:"foo" }, new Unrelated()], ts);
		typedAs([new Unrelated(), { s:"foo" } ], ts);

		// if

		var tbase:Base;
		var ti1:I1;
		var tnullbool:Null<Bool>;
		var ts: { s:String };

		typedAs(if (false) new Child1(); else new Child2(), tbase);
		typedAs(
			if (false) new Child1();
			else if (true) new Child2();
			else new Base(), tbase);
		typedAs(
			if (false) new Child1();
			else if (true) new Child2_1();
			else new Base(), tbase);
		typedAs(if (false) new Child2(); else new Unrelated(), ti1);
		typedAs(if (false) new Child2_1(); else new Unrelated(), ti1);

		typedAs(if (false) null; else false, tnullbool);
		typedAs(if (false) true; else null, tnullbool);
		typedAs(if (false) new Unrelated(); else {s:"foo"}, ts);
		typedAs(if (false) { s:"foo" }; else new Unrelated(), ts);

		//switch

		typedAs(switch(false) { case true: new Child1(); case false: new Child2(); }, tbase);
		typedAs(switch(1) { case 0: new Child1(); case 1: new Child2_1(); default: new Base(); }, tbase);
		typedAs(switch(false) { case true: new Child2(); case false: new Unrelated(); }, ti1);
		typedAs(switch(false) { case true: new Child2_1(); case false: new Unrelated(); }, ti1);

		typedAs(switch(false) { case true: null; default: false; }, tnullbool);
		typedAs(switch(false) { case true: true; default: null; }, tnullbool);
		typedAs(switch(false) { case true: new Unrelated(); default: {s:"foo"}; }, ts);
		typedAs(switch(false) { case true: { s:"foo" }; default: new Unrelated(); }, ts);

		typedAs([ { x : new Child1() }, { x : new Child2() } ], [{ x: new Base() }]);

		#if flash
		typedAs(function() { return 0; var v:UInt = 0; return v; } (), 1);
		#end
	}

	function testCallback()
	{
		var func = function(a:Int, b:String, c:Float) return a;

		var tstringfloat = function(b:String, c:Float) return 0;
		var tfloat = function(c:Float) return 0;
		var tvoid = function() return 0;
		var tintstring = function(a:Int, b:String) return 0;
		var tintfloat = function(a:Int, c:Float) return 0;
		var tint = function(a:Int) return 0;
		var tstring = function(b:String) return 0;

		// all missing

		typedAs(func.bind(), func);
		typedAs(func.bind(_), func);
		typedAs(func.bind(_, _), func);
		typedAs(func.bind(_, _, _), func);

		// all given

		typedAs(func.bind(22, "2", 13), tvoid);

		// last missing

		typedAs(func.bind(22, "2"), tfloat);
		typedAs(func.bind(22, "2", _), tfloat);

		// first given

		typedAs(func.bind(22), tstringfloat);
		typedAs(func.bind(22, _), tstringfloat);
		typedAs(func.bind(22, _, _), tstringfloat);

		// mixed

		typedAs(func.bind(_, _, 12), tintstring);
		typedAs(func.bind(_, "22", _), tintfloat);
		typedAs(func.bind(_, "22", 12), tint);
		typedAs(func.bind(12, _, 12), tstring);

		// values

		eq(1, func.bind()(1, "2", 3));
		eq(2, func.bind(2)("2", 3));
		eq(2, func.bind(2, "3")(3));
		eq(2, func.bind(2, "3", 4)());

		eq(1, func.bind(_, "2", 3)(1));
		eq(1, func.bind(_, "2")(1, 3));
		eq(1, func.bind(_)(1, "2", 3));

		eq(1, func.bind(_, "2", _)(1, 2));

		eq(1, func.bind().bind(_, "2", 3)(1));
		eq(1, func.bind(1).bind("2", 3)());
		eq(1, func.bind(1, _).bind("2")(3));
		eq(1, func.bind(_, "2").bind(1)(3));

		var a = 5;
		var b = "foo";
		var cb = func.bind(a);
		a = 6;
		func = function(a,b,c):Int return throw "error";
		eq(5, cb(b, 0));

		var optfunc = function(a:Int, b:Int, ?c:Int = 2) return a + b + c;
		eq(6, optfunc.bind(1)(3));
		eq(6, optfunc.bind(1, 3)());

		eq(7, optfunc.bind(_, _, _)(1, 2, 4));
		eq(7, optfunc.bind(_, 2, _)(1, 4));

		var foo = function ( x : Int, ?p : haxe.PosInfos ) { return "foo" + x; }
		var f : Void -> String = foo.bind(0);
 		eq("foo0", f());

		var foo = function(bar = 2) { return bar; };
		#if (flash || hl) // Cannot skip not-nullable argument
		t(typeError(foo.bind(_)));
		#else
		var l = foo.bind(_);
		eq(2, l());
		#end

		// note that this does not
		var foo = function(bar:Null<Int> = 2) { return bar; };
		var l = foo.bind(_);
		eq(2, l());
	}

	function testConstantAnonCovariance()
	{
		var func = function (str:String, ?str1: { x:Float, y:Int }, ?str2: { w:Float, h:Int } ) { };
		var a: { v:Float };
		var b:Dynamic = "bar";
		f(typeError(a = { v:0.2 } ));
		f(typeError(a = { v:0 } ));
		typedAs(a = { v: 0 }, a);
		typedAs(a = { v: 0.2 }, a);
		t(typeError(a = { v: "foo" } ));
		f(typeError(a = { v: untyped "foo" } ));
		f(typeError(a = { v: b } ));
		f(typeError( { var b: { v:Dynamic } = { v: "foo" };} ));
		t(typeError( { var b: { v:Int } = { v: 1.2 }; } ));
		t(typeError( { var b: { v:Int } = { v:0, w:"foo" }; }));
		t(typeError( { var b: { v:Int } = { v:0, v:2 }; } ));
		t(typeError( { var b: { v:Int, w:String } = { v:0 }; } ));
		typedAs({ v: 0.2, " foo":2 }, a);
		// this is allowed now (https://github.com/HaxeFoundation/haxe/issues/3547)
		//t(typeError(a = { v:0, " foo":2 } ));
		f(typeError(func("foo", { x:1.2, y:2 } )));
		f(typeError(func("foo", { w:1.2, h:2 } )));
	}

	function testCovariantReturn()
	{
		var b:Base = null;
		var c1:Child1 = null;
		var c2_1:Child2_1 = null;

		var c = new Cov2();
		typedAs(c.covariant(), c1);
		t(Std.is(c.covariant(), Child1));
		t(Std.is(cast(c, Cov1).covariant(), Child1));

		// base class reference
		var br:Cov1 = c;
		typedAs(br.covariant(), b);
		t((br.covariant() is Child1));

		// interface reference
		var ir:CovI = c;
		typedAs(ir.covariant(), b);
		t((ir.covariant() is Child1));

		// dynamic
		var dr:Dynamic = c;
		t((dr.covariant() is Child1));

		// interface covariance
		var c3 = new Cov3();
		typedAs(c3.covariant(), c2_1);
		t((c3.covariant() is Child2_1));
	}

	function testContravariantArgs()
	{
		var b = function(arg:Base):Void { };
		var c1 = function(arg:Child1):Void { };

		var c = new Ctrv2();
		typedAs(c.contravariant, b);
		typedAs(cast (c, Ctrv1).contravariant, c1);
	}

	function testInlineCast() {
		var s = new InlineCastB().test().quote();
		eq(s, "I am the greatest.");
	}

	function testInitFields()
	{
		var c = new InitBase();
		eq(c.i, 2);
		eq(c.s, "foo");
		eq(c.b, true);
		eq(c.t, String);

		var c = new InitChild();
		eq(c.i, 2);
		eq(c.s, "foo");
		eq(c.b, true);
		eq(c.t, String);

		var c = new InitChildWithCtor(null);
		eq(c.i, 2);
		eq(c.s, "foo");
		eq(c.b, true);
		eq(c.t, String);

		var c = Type.createInstance(InitWithoutCtor, []);
		eq(c.i, 2);

		var c = new InitProperties();
		eq(c.accNull, 3);
		eq(c.accDefault, 3);
		eq(c.accFunc, 3);
		eq(c.accNever, 3);
		eq(c.accDynamic, 3);
		exc(function() c.accFunc = 4);
	}

	function testReturnFlow()
	{
		var l = function():String
		{
			while (true)
			{
				return "foo";
			}
			// some platforms may have to add an implicit return null here
		}
		eq(l(), "foo");
	}

	function testOptionalParamsSkip() {
		function foo( a : MyEnum, ?b : Bool, ?c : MyEnum ) {
			return "";
		}
		typedAs(foo(A), "");
		typedAs(foo(A, true), "");
		typedAs(foo(A, A), "");
		t(typeError(foo(A, A, false)));
	}

	function testParamConstraints()
	{
		var pcc = new ParamConstraintsClass();
		var b = new Base();
		var c1 = new Child1();
		var u = new Unrelated();
		var ci1 = new CI1();

		eq(ParamConstraintsClass.staticSingle(b), b);
		eq(ParamConstraintsClass.staticSingle(c1), c1);
		// TODO: these should fail (param is constrained to Base)
		//ParamConstraintsClass.staticSingle(u);
		//ParamConstraintsClass.staticSingle(1);
		//ParamConstraintsClass.staticSingle("foo");

		eq(pcc.memberSingle(b), b);
		eq(pcc.memberSingle(c1), c1);
		//typeError(pcc.memberSingle(u));

		eq(pcc.memberMultiple(ci1), ci1);
		//typeError(pcc.memberMultiple(b));
		//typeError(pcc.memberMultiple(c1));

		var l = new List();
		l.push(ci1);
		var lmono = new List();
		eq(pcc.memberComplex(ci1, l), l);
		//eq(pcc.memberComplex(ci1, lmono), lmono);
		//typeError(pcc.memberComplex(ci1, [ci1]));

		eq(pcc.memberBasic("foo", ["bar"]), "bar");

		eq(pcc.memberAnon( { x : 1, y : 3. } ), 4);
		//typeError(pcc.memberAnon( { x : 1 } ));
		//typeError(pcc.memberAnon( { y : 3. } ));

		#if !(java || cs)
		// pcc.memberOverload("foo", "bar");
		#end
		// TODO: this should not fail (overload accepts)
		//pcc.memberOverload(1, [2]);
		//t(typeError(pcc.memberOverload(1, ["foo"])));

		var pcc2 = new ParamConstraintsClass2();
		pcc2.check([1]);
		//typeError(pcc2.check(["foo"]));

		var pcc2 = new ParamConstraintsClass2();
		pcc2.bind("foo");
		//typeError(pcc2.check([1]));
		pcc2.check(["foo"]);

		var pcc2 = new ParamConstraintsClass2<String>();
		//t(typeError(pcc2.check([1])));
		pcc2.check(["foo"]);
	}

	function testUsing()
	{
		eq(UsingChild1.test(), "FOOFOOFOO");
		eq(UsingChild2.test(), "FOO");
		eq(UsingUnrelated.test(), "trueFOOFOO");
	}

	function testInlineInit()
	{
		eq(InitBase.si, 2);
		eq(InitBase.sop, 27);
		eq(InitBase.sp, 6);
		eq(InitBase.st, String);
		eq(InitBase.sinline, 60000.);
	}

	function testInline()
	{
		typedAs(inlineTest1([1]), var void:Void);
		typedAs(inlineTest2([1]), var void:Void);
	}

	inline function inlineTest1<T>(map:Array<T>) {
		map[0];
	}

	inline function inlineTest2(map:Array<Dynamic>) {
		map[0];
	}

	public function testMacroFollowWithAbstracts()
	{
		#if !macro
		eq(MyMacro.MyMacroHelper.followWithAbstracts(new Map<String,String>()), "TInst(haxe.ds.StringMap,[TInst(String,[])])");
		eq(MyMacro.MyMacroHelper.followWithAbstractsOnce({ var x:TypedefToStringMap<String>; x; }), "TType(Map,[TInst(String,[]),TInst(String,[])])");
		eq(MyMacro.MyMacroHelper.followWithAbstracts(new TypedefToStringMap<String>()), "TInst(haxe.ds.StringMap,[TInst(String,[])])");
		#end
	}

	public function testMacroRest() {
		#if !macro
		var r = MyMacro.MyRestMacro.testRest1(1, 2, 3);
		eq(r.length, 3);
		eq(r[0], 1);
		eq(r[1], 2);
		eq(r[2], 3);

		var r : Array<Dynamic> = MyMacro.MyRestMacro.testRest1(1, [2, 3]);
		eq(r.length, 2);
		eq(r[0], 1);
		eq(r[1][0], 2);
		eq(r[1][1], 3);

		var r = MyMacro.MyRestMacro.testRest1(1);
		eq(r.length, 1);
		eq(r[0], 1);

		var r = MyMacro.MyRestMacro.testRest2(1, 2, 3, 4);
		eq(r.length, 4);
		eq(r[0], 1);
		eq(r[1], 2);
		eq(r[2], 3);
		eq(r[3], 4);

		var r = MyMacro.MyRestMacro.testRest2(1, 2);
		eq(r.length, 2);
		eq(r[0], 1);
		eq(r[1], 2);

		var r : Array<Dynamic> = MyMacro.MyRestMacro.testRest2(1, 2, [3]);
		eq(r.length, 3);
		eq(r[0], 1);
		eq(r[1], 2);
		eq(r[2][0], 3);
		#end
	}

	public function testGenericFunction() {
		gf1(2);
		gf1("foo");
		gf1(true);

		gf1(new haxe.Template("foo"));

		gf1(new haxe.ds.GenericStack<Int>());
		hsf(TestType, "gf1_Int");
		hsf(TestType, "gf1_String");
		hsf(TestType, "gf1_Bool");

		hsf(TestType, "gf1_haxe_Template");

		hsf(TestType, "gf1_haxe_ds_GenericStack_Int");
		t(typeError(gf1(null))); // monos don't work
		t(typeError(gf1( { foo:1 } ))); // structures don't work

		eq("foo[1,2]", gf2("foo", [1, 2]));
		eq("foo[[1,2]]", gf2("foo", [[1, 2]]));
		hsf(TestType, "gf2_String_Int");
		hsf(TestType, "gf2_String_Array_Int");

		var a = gf3("foo", ["bar", "baz"]);
		eq(a[0], "bar");
		eq(a[1], "baz");
		eq(a[2], "foo");
		hsf(TestType, "gf3_String_Array_String");

		var t = new haxe.Template("foo");
		var ta = gf3(t, [])[0];
		f(t == ta);
		hsf(TestType, "gf3_haxe_Template_Array_haxe_Template");

		eq(overloadFake(1), 1);
		eq(overloadFake("bar"), "barfoo");
	}

	@:generic static function gf1<T>(a:T) {
		return a;
	}

	@:generic static function gf2<A,B>(a:A, b:Array<B>) {
		return Std.string(a) + Std.string(b);
	}

	@:generic static function gf3 < A:haxe.Constraints.Constructible<String -> Void>, B:Array<A> > (a:A, b:B) {
		var clone = new A("foo");
		b.push(clone);
		return b;
	}

	@:generic static function overloadFake<A>(a:A) {
		return a;
	}

	static function overloadFake_String(a:String) {
		return a + "foo";
	}

	function testSuperPropAccess() {
		var c = new ChildSuperProp();
		eq(c.prop, 2);
		eq(c.prop = 4, 5);
		eq(c.test(), "test2");
		eq(c.fProp(9), "test09");
	}

	function testVoidFunc() {
		exc(function() { throw null; return 1; } );
		exc(function() { throw null; return "foo"; } );
		exc(function() { throw null; return MyEnum.A; } );
		exc(function() { throw null; return new haxe.Template("foo"); } );
		exc(function() { throw null; return null; } );
		exc(function() { throw null; return { foo: 1}; } );
	}

	function testAbstractCastConstraints() {
		var z:unit.MyAbstract.AbstractZ<String> = new unit.MyAbstract.AbstractBase("foo");
		var s:String = z;
		t(typeError( {
			var i:Int = z;
		}));
		eq("foo", s);

		var z:unit.MyAbstract.AbstractZ<Int> = new unit.MyAbstract.AbstractBase(12);
		var i:Int = z;
		eq(12, i);
		t(typeError( {
			var s:String = z;
		}));
	}

	function testOpArrow() {
		var m = new Map<Int,Int>();
		var map = [1 => 2, 3 => 4];
		typedAs(map, m);
		t((map is haxe.ds.IntMap));
		eq(map.get(1), 2);
		eq(map.get(3), 4);

		var m = new Map<String,Int>();
		var map = ["1" => 2, "3" => 4];
		typedAs(map, m);
		t((map is haxe.ds.StringMap));
		eq(map.get("1"), 2);
		eq(map.get("3"), 4);

		var a = new unit.MyAbstract.ClassWithHashCode(1);
		var b = new unit.MyAbstract.ClassWithHashCode(2);
		var m = new Map<unit.MyAbstract.ClassWithHashCode,Int>();
		var map = [a => 2, b => 4];
		typedAs(map, m);
		//t((map is haxe.ds.IntMap));
		eq(map.get(a), 2);
		eq(map.get(b), 4);

		// duplicate key
		t(typeError([1 => 2, 1 => 3]));
		// key unification
		t(typeError([1 => 2, "1" => 2]));
		// value unification
		t(typeError([1 => 2, 1 => "2"]));
	}

	function testAbstractGeneric() {
		var map = new Map();
		map.set("foo", 1);
		t((map is haxe.ds.StringMap));

		var map = new Map();
		_mapMe(map); // infer from function call
		t((map is haxe.ds.IntMap));

		var map = new Map();
		var a = new unit.MyAbstract.ClassWithHashCode(1);
		var b = new unit.MyAbstract.ClassWithHashCode(2);
		map.set(a, "foo");
		map.set(b, "bar");
		eq(map.get(a), "foo");
		eq(map.get(b), "bar");
		//t((map is haxe.ds.IntMap));

		var map = new Map();
		var a = new unit.MyAbstract.ClassWithoutHashCode(1);
		var b = new unit.MyAbstract.ClassWithoutHashCode(2);
		map.set(a, "foo");
		map.set(b, "bar");
		eq(map.get(a), "foo");
		eq(map.get(b), "bar");
		// this may be specialized
		//t((map is haxe.ds.ObjectMap));

		//var map = new unit.MyAbstract.MyMap();
		//map.set(new haxe.Template("foo"), 99);
		//t((map is unit.MyAbstract.PseudoObjectHash));

		// all these cause a compilation error, but we cannot typeError test that because it happens
		// during a post-process check
		//var map = new Map(); // Could not determine type for IMap<Float, Int>
		//map.set(1.1, 1);

		//var map = new Map(); // Could not determine type for IMap<x : String -> String, Int>
		//map.set(function(x:String) return x, 1);

		//var map = new Map(); // Could not determine type for IMap<Unknown<0>, Unknown<1>>
	}

	static function _mapMe(map:Map < Int, String > ) { }

	function testAbstractOverload() {
		var ms1:unit.MyAbstract.MyString = "foo";
		var ms2:unit.MyAbstract.MyString = "bar";
		var msum = ms1 + ms2;
		eq(msum, "foobar");
		typedAs(msum, ms1);
		t((msum is String));

		var msum2 = ms1 + 1;
		eq(msum2, "foo1");
		typedAs(msum2, ms1);
		t((msum2 is String));

		// operation is defined, but return type is not compatible
		t(typeError(ms1 + true));
		// operation is not defined
		t(typeError(ms1 - ms2));
	}

	function testAbstractUnop() {
		var vec:unit.MyAbstract.MyVector = new unit.MyAbstract.MyPoint3(1, 2, 3);
		var vec2 = -vec;
		t(vec2 != vec);
		eq(vec.toString(), "(1,2,3)");
		eq(vec2.toString(), "(-1,-2,-3)");

		var my = new unit.MyAbstract.MyInt2(12);
		eq( (-my).get(), -12);
		typedAs( -my, my);
		++my;
		eq(my.get(), 13);
		// not defined op
		t(typeError(!my));
		// wrong flag
		t(typeError(my++));
	}

	function testMapComprehension() {
		var map = [for (x in ["a", "b"]) x => x.toUpperCase()];
		t(map.exists("a"));
		t(map.exists("b"));
		eq(map.get("a"), "A");
		eq(map.get("b"), "B");
	}

	function testCustomArrayAccess() {
		var obj = {
			foo: 12,
			bar: "test"
		};
		var mr:unit.MyAbstract.MyReflect = obj;
		eq(mr["foo"], 12);
		eq(mr["bar"], "test");
		mr["foo"] = 11;
		eq(mr["foo"], 11);
		mr["foo"] += 99;
		eq(mr["foo"], 110);
		mr["baz"] = mr["bar"] += mr["foo"];
		eq(mr["baz"], "test110");
		eq(mr["bar"], "test110");

		var v = "hh";
		mr[v] = 1;
		mr[v += "h"] = 2;
		eq(mr["hhh"], 2);
		eq(v, "hhh");

		mr["hhhh"] = 0;
		mr[v += "h"] += 4;
		eq(mr["hhhh"], 4);
		eq(mr["hhh"], 2);
		eq(v, "hhhh");

		// note for later: As3 compilation fails if the function name is removed
		mr["101"] = function n(x) return 9 + x;
		eq(mr["101"](1), 10);
	}

	function testAbstractClosure() {
		var s = new unit.MyAbstract.MyAbstractClosure("foo");
		var func1 = s.test();
		eq(func1(), "foo");
		s.setVal("bar");
		eq(func1(), "foo");
		eq(s.test()(), "bar");
	}

	function testAbstractTypeParameterVariance() {
		var a:Array<unit.MyAbstract.MyInt> = [1, 2, 3];
		var b:Array<unit.MyAbstract.MyInt2> = a;
	}

	function testExposingAbstract() {
		#if !macro
		var ea = new unit.MyAbstract.ExposingAbstract();
		ea.push(12);
		eq(12, ea.pop());
		#end
	}

	function testGADTEnumAbstract() {
		var expectedA:unit.MyAbstract.GADTEnumAbstract<Void->Void>;
		var expectedB:unit.MyAbstract.GADTEnumAbstract<Int->Void>;
		typedAs(unit.MyAbstract.GADTEnumAbstract.A, expectedA);
		typedAs(unit.MyAbstract.GADTEnumAbstract.B, expectedB);
	}

	function testCreateFromComplexType() {
		var c = :Array<Int>,
			v = Type.createInstance(c, []);
		t(Std.is(v, c));
	}
}

typedef TypedefToStringMap<T> = Map<String,T>
