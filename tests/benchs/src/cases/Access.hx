package cases;

import hxbenchmark.Suite;

typedef Def = {
	var i : Int;
	var f : Float;
	@:optional var oi : Int;
	@:optional var of : Float;
	var ai : Array<Int>;
	var af : Array<Float>;
	var str : String;
}
typedef Obj = {>Def,
	var sub : Def;
}

@:analyzer(ignore)
class Access extends TestCase {
	function measureReal() {
		var o = getObj();
		var suite = new Suite("real");
		suite.add(".Int", o.i);
		suite.add(".Float", o.f);
		suite.add("Int[]", o.ai[0]);
		suite.add("Float[]", o.af[0]);
		suite.add(".Int?", o.oi);
		suite.add(".Float?", o.of);
		suite.add(".String.length", o.str.length);
		suite.add(".sub.Int", o.sub.i);
		suite.add(".sub.Float", o.sub.f);
		suite.add(".sub.Int[]", o.sub.ai[0]);
		suite.add(".sub.Float[]", o.sub.af[0]);
		suite.add(".sub.Int?", o.sub.oi);
		suite.add(".sub.Float?", o.sub.of);
		suite.add(".sub.String.length", o.sub.str.length);
		return suite.run();
	}

	function measureJson() {
		var o = getObj();
		var o = haxe.Json.stringify(o);
		var o:Obj = haxe.Json.parse(o);
		var suite = new Suite("json");
		suite.add(".Int", o.i);
		suite.add(".Float", o.f);
		suite.add("Int[]", o.ai[0]);
		suite.add("Float[]", o.af[0]);
		suite.add(".Int?", o.oi);
		suite.add(".Float?", o.of);
		suite.add(".String.length", o.str.length);
		suite.add(".sub.Int", o.sub.i);
		suite.add(".sub.Float", o.sub.f);
		suite.add(".sub.Int[]", o.sub.ai[0]);
		suite.add(".sub.Float[]", o.sub.af[0]);
		suite.add(".sub.Int?", o.sub.oi);
		suite.add(".sub.Float?", o.sub.of);
		suite.add(".sub.String.length", o.sub.str.length);
		return suite.run();
	}

	function measureDynamic() {
		var o:Dynamic = getObj();
		var suite = new Suite("dynamic");
		suite.add(".Int", o.i);
		suite.add(".Float", o.f);
		suite.add("Int[]", o.ai[0]);
		suite.add("Float[]", o.af[0]);
		suite.add(".Int?", o.oi);
		suite.add(".Float?", o.of);
		suite.add(".String.length", o.str.length);
		suite.add(".sub.Int", o.sub.i);
		suite.add(".sub.Float", o.sub.f);
		suite.add(".sub.Int[]", o.sub.ai[0]);
		suite.add(".sub.Float[]", o.sub.af[0]);
		suite.add(".sub.Int?", o.sub.oi);
		suite.add(".sub.Float?", o.sub.of);
		suite.add(".sub.String.length", o.sub.str.length);
		return suite.run();
	}

	static function getObj():Obj {
		return {
			i : 1,
			f : 1.0,
			oi : 1,
			of : 1.0,
			ai : [1],
			af : [1.0],
			str : "1",
			sub : {
				i : 1,
				f : 1.0,
				oi : 1,
				of : 1.0,
				ai : [1],
				af : [1.0],
				str : "1",
			}
		};
	}
}