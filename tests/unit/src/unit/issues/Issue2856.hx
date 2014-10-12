package unit.issues;

class Issue2856 extends Test {
	#if python
	function test() {
		function x (args:python.KwArgs) {
			var dict = args.toDict();
			var acc = [];
			for (key in dict.keys()) {
				acc.push(key + "=" +dict.get(key, null));
			}
			acc.sort(Reflect.compare);
			return acc.join(";");
		}

		var a = python.Lib.anonToDict({ "a" : 1, "b" : 2});
		var res = x( a );
		eq("a=1;b=2", res);

		var res = x( python.Lib.anonToDict({ "a" : 1, "b" : 2}) );
		eq("a=1;b=2", res);
	}
	#end
}