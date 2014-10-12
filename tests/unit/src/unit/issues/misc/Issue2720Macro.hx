package unit.issues.misc;

class Issue2720Macro {
	macro static public function dupe(s:String) {
		return macro $v{s+s};
	}
}