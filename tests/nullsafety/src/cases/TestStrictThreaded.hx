package cases;

import Validator.shouldFail;

class TestStrictThreaded {
	function nonFinalField_immediatelyAfterCheck_shouldFail(o:{field:Null<String>}) {
		if(o.field != null) {
			shouldFail(var notNullable:String = o.field);
		}
	}

	function objIterationAfterNullCheck_shouldFail(result:{?leaks:Array<String>}):Void {
		if (result.leaks != null) {
			shouldFail(shouldFail(
				for (leak in result.leaks) {
					final v:String = leak;
				}
			));
		}
	}

	#if target.threaded

	function tls() {
		final tls = new sys.thread.Tls<String>();
		tls.value = null;
	}

	#end
}
