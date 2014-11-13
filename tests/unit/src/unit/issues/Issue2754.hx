package unit.issues;

class Issue2754 extends unit.Test
{
#if (cs || java)
	public function testClass()
	{
		var tc = new TestClass();

		// initialized to null
		nulleq(tc.dyn,null);
		nulleq(tc.nullInt,null);
		nulleq(tc.nullBool,null);
		nulleq(tc.nullFloat,null);
		nulleq(tc.nullArrayInt,null);
		nulleq(tc.nullArrayNullInt,null);

		// explicitly set to null
		tc.dyn = null;
		tc.nullInt = null;
		tc.nullBool = null;
		tc.nullFloat = null;
		tc.nullArrayInt = null;
		tc.nullArrayNullInt = null;
		nulleq(tc.dyn,null);
		nulleq(tc.nullInt,null);
		nulleq(tc.nullBool,null);
		nulleq(tc.nullFloat,null);
		nulleq(tc.nullArrayInt,null);
		nulleq(tc.nullArrayNullInt,null);

		// set to null from dynamic
		var dyn:Dynamic = null;
		tc.dyn = dyn;
		tc.nullInt = dyn;
		tc.nullBool = dyn;
		tc.nullFloat = dyn;
		tc.nullArrayInt = dyn;
		tc.nullArrayNullInt = dyn;
		nulleq(tc.dyn,null);
		nulleq(tc.nullInt,null);
		nulleq(tc.nullBool,null);
		nulleq(tc.nullFloat,null);
		nulleq(tc.nullArrayInt,null);
		nulleq(tc.nullArrayNullInt,null);

		// set to null from complex expression
		tc.dyn = complexGetNull();
		tc.nullInt = complexGetNull();
		tc.nullBool = complexGetNull();
		tc.nullFloat = complexGetNull();
		tc.nullArrayInt = complexGetNull();
		tc.nullArrayNullInt = complexGetNull();
		nulleq(tc.dyn,null);
		nulleq(tc.nullInt,null);
		nulleq(tc.nullBool,null);
		nulleq(tc.nullFloat,null);
		nulleq(tc.nullArrayInt,null);
		nulleq(tc.nullArrayNullInt,null);

		// set to null from generic function
		tc.dyn = getNull();
		tc.nullInt = getNull();
		tc.nullBool = getNull();
		tc.nullFloat = getNull();
		tc.nullArrayInt = getNull();
		tc.nullArrayNullInt = getNull();
		nulleq(tc.dyn,null);
		nulleq(tc.nullInt,null);
		nulleq(tc.nullBool,null);
		nulleq(tc.nullFloat,null);
		nulleq(tc.nullArrayInt,null);
		nulleq(tc.nullArrayNullInt,null);

		// Null<Bool>
		dyn = true;
		tc.dyn = dyn;
		tc.nullBool = dyn;
		eq(tc.dyn,true);
		eq(tc.nullBool,true);
		tc.dyn = !dyn;
		tc.nullBool = !dyn;
		eq(tc.dyn,false);
		eq(tc.nullBool,false);
		tc.dyn = !tc.nullBool;
		tc.nullBool = !tc.nullBool;
		eq(tc.dyn,true);
		eq(tc.nullBool,true);

		// Null<Int> / Null<Float>

		//from Dynamic
		dyn = 42;
		tc.dyn = dyn;
		tc.nullInt = dyn;
		tc.nullFloat = dyn;
		tc.nullArrayInt = [dyn];
		tc.nullArrayNullInt = [dyn];
		eq(tc.dyn,42);
		eq(tc.nullInt,42);
		eq(tc.nullFloat,42);
		eq(tc.nullArrayInt[0],42);
		eq(tc.nullArrayNullInt[0],42);

		//from Null<Int>
		var ni:Null<Int> = 21;
		tc.dyn = ni;
		tc.nullInt = ni;
		tc.nullFloat = ni;
		tc.nullArrayInt = [ni];
		tc.nullArrayNullInt = [ni];
		eq(tc.dyn,21);
		eq(tc.nullInt,21);
		eq(tc.nullFloat,21);
		eq(tc.nullArrayInt[0],21);
		eq(tc.nullArrayNullInt[0],21);

		//binops
		ni = null;
		tc.dyn = tc.nullInt*2 + ni;
		tc.nullInt = tc.nullInt*tc.nullArrayNullInt[0] + tc.nullArrayNullInt[1] + ni;
		tc.nullFloat = tc.nullFloat*tc.nullArrayNullInt[0] + tc.nullArrayNullInt[1] + ni;
		tc.nullArrayInt[0] += tc.nullArrayNullInt[0];
		tc.nullArrayNullInt[0] *= tc.nullArrayNullInt[0] - tc.nullArrayNullInt[1];
		eq(tc.dyn, 42);
		eq(tc.nullInt, 441);
		eq(tc.nullFloat, 441);
		eq(tc.nullArrayInt[0], 42);
		eq(tc.nullArrayNullInt[0], 441);
	}
#end

	private static inline function complexGetNull<T>():Null<T>
	{
		var x = 10;
		var y = 1000;
		x += y; //just do something here
		return null;
	}

	private static inline function getNull<T>():Null<T>
	{
		return null;
	}

	// In normal 'eq', the types are inferred as <int> rather than <Null<Int>>
	// which kind of defeats the purpose of the checks
	private function nulleq<T>(v1:Null<T>, v2:Null<T>,?pos:haxe.PosInfos)
	{
		this.infos("v1 == null");
		this.t(v1 == null,pos);
		this.infos("v2 == null");
		this.t(v2 == null,pos);
		this.eq(v1,v2);
		this.infos(null);
	}
}

private class TestClass
{
	public var dyn:Dynamic;
	public var nullInt:Null<Int>;
	public var nullBool:Null<Bool>;
	public var nullFloat:Null<Float>;
	public var nullArrayInt:Null<Array<Int>>;
	public var nullArrayNullInt:Null<Array<Null<Int>>>;

	public function new()
	{

	}
}
