class Main {
	static function main () {
		#if !macro
		define();
		#end
	}

	macro static public function define() {
		return macro $i{""};
	}
}