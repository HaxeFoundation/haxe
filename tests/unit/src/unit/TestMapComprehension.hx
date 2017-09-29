package unit;

class TestMapComprehension extends unit.Test {
	public function testBasic() {
		mapEq([for (i in 0...2) i => i], [0 => 0, 1 => 1]);
		mapEq([for (i in 0...2) (i => i)], [0 => 0, 1 => 1]);
		mapEq([for (i in 0...2) cast i => i], [0 => 0, 1 => 1]);
		mapEq([for (i in 0...2) untyped i => i], [0 => 0, 1 => 1]);
		mapEq([for (i in 0...2) if (i == 1) i => i], [1 => 1]);
		mapEq([for (i in 0...2) if (i == 0) i => i else i => i * 2], [0 => 0, 1 => 2]);
		mapEq([for (i in 0...2) (i == 0) ? i => i : i => i * 2], [0 => 0, 1 => 2]);
		mapEq([for (i in 0...2) switch i { case 0: i => i; case _: i => i * 2; }], [0 => 0, 1 => 2]);
		mapEq([for (i in 0...2) try i => i catch(e:Dynamic) i => i * 2], [0 => 0, 1 => 1]);
		mapEq([for (i in 0...2) try { throw null; i => i; } catch(e:Dynamic) i => i * 2], [0 => 0, 1 => 2]);
		mapEq([for (i in 0...2) try { throw null; } catch(e:Dynamic) i => i * 2], [0 => 0, 1 => 2]);

		mapEq([for (i in 0...2) { continue; i => i; }], new Map<Int, Int>());
	}

	public function testMark() {
		var specialValue = null;
		function doSomething(i:Int) specialValue = ("special" + i);
		var m = [for(i in 0...3) if (i != 1) i => 'number $i' else { doSomething(i); continue; }];
		mapEq(m, [0 => "number 0", 2 => "number 2"]);
		eq(specialValue, "special1");
	}

	function mapEq<K, V>(m1:Map<K, V>, m2:Map<K, V>, ?p:haxe.PosInfos) {
		for (k1 in m1.keys()) {
			eq(m1[k1], m2[k1], p);
		}
		for (k2 in m2.keys()) {
			eq(m1[k2], m2[k2], p);
		}
	}
}