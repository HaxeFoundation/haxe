package unit.issues;

class Issue2889 extends Test {
	public function test()
	{
		function mapMappable <A,B>(m:Mappable<A>, f:A->B):Mappable<B> {
			return m.map(f);
		}
		var r = mapMappable([1], function (y) return y+1);
		var r:Array<Int> = cast r;
		eq(r.length,1);
		eq(2,r[0]);
	}

	function testDynamic() {
		var a:Dynamic = [1, 2];
		var b:Dynamic = a.map(function(x) return x * 2);
		eq(2, b.length);
		eq(2, b[0]);
		eq(4, b[1]);
		var c:Dynamic = a.filter(function(x) return x % 2 == 0);
		eq(1, c.length);
		eq(2, c[0]);
	}

	function testFilterStructure() {
		var a:{ function filter(f:Int->Bool):Array<Int>; } = [1, 2];
		var b = a.filter(function(x) return x % 2 == 0);
		eq(1, b.length);
		eq(2, b[0]);
	}

	function testMapStructure() {
		var a:{ function map(f:Int->Int):Array<Int>; } = [1, 2];
		var b = a.map(function(x) return x * 2);
		eq(2, b.length);
		eq(2, b[0]);
		eq(4, b[1]);
	}
}

private typedef Mappable<Y> = {
	public function map <X>(f:Y->X):Array<X>;
}

