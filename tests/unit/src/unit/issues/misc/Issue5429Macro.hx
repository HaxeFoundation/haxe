package unit.issues.misc;

class Issue5429Macro {
	macro static public function mb(){
		return macro {

		@:mergeBlock {
			var a = value();
			var b = that();
		}
		eq(444, a + b); // not in scope
		};
	}
}