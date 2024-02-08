package cases;

import Validator.shouldFail;

class TestStrictThreaded {
	function nonFinalField_immediatelyAfterCheck_shouldFail(o:{field:Null<String>}) {
		if(o.field != null) {
			shouldFail(var notNullable:String = o.field);
		}
	}
}