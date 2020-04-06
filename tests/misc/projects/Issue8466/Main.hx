class Main {
	static public function init() {
		if(Sys.programPath() != null) {
			throw 'Unexpected value';
		}
	}
}
