package unit.issues;

#if (java || cs)
typedef Float32 = Single;
#end

class Issue3306 extends Test {
#if (java || cs)
	function test() {
		var iw:Float32 = 0.0;
		var iw2:Float32;
		if (iw > 10)
			iw2 = 2;
		else
			iw2 = 3;
		eq(iw,0);
		eq(iw2,3);
	}
#end
}
