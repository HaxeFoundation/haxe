package unit.issues;

class Issue11810 extends Test {
	#if hl
	function test() {
		var arrObj = [];
		eq(null, arrObj[arrObj.length-1]);
		var arrBytes : Array<Int> = [];
		eq(0, arrBytes[arrBytes.length-1]);
	}
	#end
}
