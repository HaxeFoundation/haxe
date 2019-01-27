class Macro {
	macro static public function wrongExprOutOfSafety() {
		return macro var s:String = null;
	}
}