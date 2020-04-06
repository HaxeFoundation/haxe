abstract Ref2<T>(T) {

	@:from macro static function from(v) {
		return v;
	}

}

class Main2 {

	static function main() {
		#if !macro
		var r : Ref2<String> = 10;
		trace(r);
		#end
	}

}