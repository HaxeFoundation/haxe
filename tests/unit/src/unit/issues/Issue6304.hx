package unit.issues;

class Issue6304 extends unit.Test {
	function test() {
		// eq(2, main1([], 1));
		utest.Assert.pass();
	}

	static function main1 (arr:Array<{}>, multiplier:Int) {
		// function doSomething <T>() {
		// 	var mul:Int =  multiplier;
		// 	arr.push({});
		// 	return arr.length + mul;
		// };
		// return doSomething();
	}
}
