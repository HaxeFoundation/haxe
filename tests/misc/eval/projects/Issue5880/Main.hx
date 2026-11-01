abstract Ref<T>({ v : T }) {

	@:from macro static function from(v) {
		return macro { somethingNotV : $v };
	}

}

class Main {

	static function main() {
		#if !macro
		var r : Ref<Int> = 10;
		trace(r);
		#end
	}

}