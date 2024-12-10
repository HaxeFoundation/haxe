package unit.issues;

#if jvm
typedef Float32 = Single;
#end

class Issue3306 extends Test {
#if jvm
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
