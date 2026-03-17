package unit.teststd;

class TestLambda extends unit.Test {
	public function test() {
		function name(o:Dynamic) return Type.getClassName(Type.getClass(o));
		// array
		var a = [1];
		var a2 = Lambda.array(a);
		eq(name(a), "Array");
		eq(name(a2), "Array");
		t(a != a2);
		eq(a2.length, 1);
		eq(a2[0], 1);
		var e = [];
		var e2 = Lambda.array(e);
		t(e != e2);
		eq(e2.length, 0);

		// list
		var a = [1];
		var l = Lambda.list(a);
		eq(name(a), "Array");
		eq(name(l), "haxe.ds.List");
		eq(l.length, 1);
		eq(l.first(), 1);
		var l2 = Lambda.list(l);
		eq(name(l2), "haxe.ds.List");
		t(l != l2);
		var e = new List();
		var e2 = Lambda.list(e);
		t(e != e2);
		eq(e2.length, 0);

		// map
		var a = [1, 2, 3];
		var b = Lambda.map(a,function(i) return i * 2);
		eq(b.length, 3);
		eq(b.pop(), 6);
		eq(b.pop(), 4);
		eq(b.pop(), 2);

		// mapi
		var a = [1, 2, 3];
		var total = 0;
		function myMap(index, i) {
			total += index;
			return i * 2;
		}
		var b = Lambda.mapi(a, myMap);
		eq(total, 3);
		eq(b.length, 3);
		eq(b.pop(), 6);
		eq(b.pop(), 4);
		eq(b.pop(), 2);

		// has
		t(Lambda.has([1,2,3],1));
		f(Lambda.has([1,2,3],4));
		f(Lambda.has([],null));
		t(Lambda.has([null],null));

		// exists
		t(Lambda.exists([1, 2, 3], function(i) return i == 2));
		f(Lambda.exists([1, 2, 3], function(i) return i == 4));
		f(Lambda.exists([], function(x) return true));

		// foreach
		t(Lambda.foreach([2, 4, 6],function(i) return i % 2 == 0));
		f(Lambda.foreach([2, 4, 7],function(i) return i % 2 == 0));
		t(Lambda.foreach([], function(x) return false));

		// iter
		var check = 0;
		var sum = 0;
		Lambda.iter([1,2,3],function(i) {
			if (check != i - 1) throw "no match";
			check = i;
			sum += i;
		});
		eq(check, 3);
		eq(sum, 6);
		Lambda.iter([],function(i) return throw "no call");

		// filter
		aeq([1, 2], Lambda.array(Lambda.filter([1,2,3,4],function(i) return i < 3)));
		aeq([1, 2, 3, 4], Lambda.array(Lambda.filter([1,2,3,4],function(i) return true)));
		eq(Lambda.array(Lambda.filter([1,2,3,4],function(i) return false)).length, 0);
		eq(Lambda.array(Lambda.filter([],function(_) return false)).length, 0);
		eq(Lambda.array(Lambda.filter([],function(_) return true)).length, 0);
		eq(Lambda.array(Lambda.filter([],null)).length, 0);


		// fold
		eq(Lambda.fold(["b","c","d"],function(s,acc) return s + acc,"a"), "dcba");
		eq(Lambda.fold([],function(s:String,acc) return s + acc,"a"), "a");
		eq(Lambda.fold([],function(s:String,acc) return s + acc,null), null);

		// foldi
		eq(Lambda.foldi(["b","c","d"],function(s,acc,i) return Std.string(i) + s + acc,"a"), "2d1c0ba");
		eq(Lambda.foldi([],function(s:String,acc,i) return Std.string(i) + s + acc,"a"), "a");
		eq(Lambda.foldi([],function(s:String,acc,i) return Std.string(i) + s + acc,null), null);

		// count
		eq(Lambda.count([1,2,3]), 3);
		eq(Lambda.count([1,2,3], function(x) return false), 0);
		eq(Lambda.count([1,2,3], function(x) return true), 3);
		eq(Lambda.count([1,2,3], function(x) return x % 2 == 1), 2);
		eq(Lambda.count([]), 0);

		// empty
		t(Lambda.empty([]));
		f(Lambda.empty([null]));

		// indexOf
		eq(Lambda.indexOf([1,2,3],1), 0);
		eq(Lambda.indexOf([1,2,3],2), 1);
		eq(Lambda.indexOf([1,2,3],3), 2);
		eq(Lambda.indexOf([1,2,3,3],3), 2);
		eq(Lambda.indexOf([1,2,3],4), -1);
		eq(Lambda.indexOf([],1), -1);

		// find
		eq(Lambda.find([1,2,3,4,5],i -> i % 2 == 0), 2);
		eq(Lambda.find([1,2,3,4,5],i -> i % 4 == 0), 4);
		eq(Lambda.find([1,2,3,4,5],i -> i % 8 == 0), null);
		eq(Lambda.find([1,2,3,4,5],i -> true), 1);
		eq(Lambda.find([1,2,3,4,5],i -> false), null);
		eq(Lambda.find([],i -> true), null);
		eq(Lambda.find([],i -> false), null);

		// findIndex
		eq(Lambda.findIndex([1,2,3,4,5],i -> i % 2 == 0), 1);
		eq(Lambda.findIndex([1,2,3,4,5],i -> i % 4 == 0), 3);
		eq(Lambda.findIndex([1,2,3,4,5],i -> i % 8 == 0), -1);
		eq(Lambda.findIndex([1,2,3,4,5],i -> true), 0);
		eq(Lambda.findIndex([1,2,3,4,5],i -> false), -1);
		eq(Lambda.findIndex([],i -> true), -1);
		eq(Lambda.findIndex([],i -> false), -1);

		// concat
		aeq([1, 2, 3, 3, 4, 5], Lambda.array(Lambda.concat([1,2,3],[3,4,5])));
		aeq([1, 2, 3], Lambda.array(Lambda.concat([1,2,3],[])));
		aeq([1, 2, 3], Lambda.array(Lambda.concat([],[1,2,3])));
		eq(Lambda.array(Lambda.concat([],[])).length, 0);
	}
}
