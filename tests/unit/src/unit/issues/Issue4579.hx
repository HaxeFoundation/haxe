package unit.issues;

class Issue4579 extends Test {
	#if js
	function test() {
		var stringMap1:Map<String, Dynamic> = [
			'string-key-1'=> 42
		];

		var stringMap2:Map<String, Dynamic> = [
			'string-key-1'=> 42,
			'string-key-2'=> {a:1}
		];

		//count commas in toString()
		eq(stringMap1.toString().split(',').length - 1, 0);
		eq(stringMap2.toString().split(',').length - 1, 1);
	}
	#end
}
