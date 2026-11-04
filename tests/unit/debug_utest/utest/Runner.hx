package utest;

class Runner {

	var classes:Array<ITest> = [];
	var count = 0;

	public function new() {
	}

	public function addCase(c:ITest) {
		classes.push(c);
	}

	public function run() {
		var withResume = true;
		var errors = 0;
		if( withResume ) {
			for( c in classes )
				try {
					c.runTests();
				} catch( e : Dynamic ) {
					Sys.println(e + haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
					errors++;
				}
		} else {
			for( c in classes )
				c.runTests();
		}
		trace(classes.length+" classes and "+Assert.C+" tests run");
		if( errors > 0 ) trace(errors+" ERRORS left");
	}

}