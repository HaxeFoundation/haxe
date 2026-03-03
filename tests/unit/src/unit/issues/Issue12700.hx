package unit.issues;

class Issue12700 extends Test {
	#if !lua
	function test() {
		// Assigning to a .length field on an anonymous struct should not generate
		// invalid Python code like `HxOverrides.length(obj) = value`
		var notes:Array<{time:Float, length:Float}> = [{time: 1.0, length: 0.0}];
		var holdIndexes:Array<Int> = [0];
		var lane = 0;
		var time = 2.0;
		notes[holdIndexes[lane]].length = time - notes[holdIndexes[lane]].time;
		eq(1.0, notes[0].length);
	}
	#end
}
