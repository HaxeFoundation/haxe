abstract Ref3(Bool) {

	@:from macro static function from(v) {
		return v;
	}

}

class Main3 {

	static function main() {
		#if !macro
		var r : Ref3 = 10;
		#end
	}

}