package unit.issues;

class Issue6375 extends Test {
	var memberField(default,set):Int;
	static var staticField(default,set):Int;

	static var rollback:()->Void;

	function set_memberField(_) {
		memberField = 1;
		rollback = () -> memberField = 3;
		return memberField;
	}

	static function set_staticField(_) {
		staticField = 2;
		rollback = () -> staticField = 4;
		return staticField;
	}

	function test() {
		memberField = 42;
		eq(memberField, 1);
		rollback();
		eq(memberField, 3);

		staticField = 42;
		eq(staticField, 2);
		rollback();
		eq(staticField, 4);
	}
}
