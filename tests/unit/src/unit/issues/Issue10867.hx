package unit.issues;

class Issue10867 extends Test {
	function test() {
		var r = ~/\/\*(((?!\*\/).)*)$/s;
		var a = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

		var test = "/*";
		var res = new StringBuf();
		for (i in 0...10) {
			test += a;
			res.add('iter $i: ${r.match(test)} ');
		}

		res.add("Finished");
		eq("iter 0: true iter 1: true iter 2: true iter 3: true iter 4: true iter 5: true iter 6: true iter 7: true iter 8: true iter 9: true Finished",
			res.toString());
	}
}
