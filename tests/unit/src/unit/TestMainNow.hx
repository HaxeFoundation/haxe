package unit;

class TestMainNow {
	static public function printNow() {
		#if !macro
		trace("Generated at: " + HelperMacros.getCompilationDate());
		#end
	}
}
