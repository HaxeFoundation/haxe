abstract Ref<T>({ v : T }) {
#if !macro
	@:from static public function fromV<T>(v:{v:T}):Ref<T> {
		return [v];
	}
#end

	@:from macro static function from(v) {
		return macro { v : $v };
	}
}

class Main5 {

	static function main() {
		#if !macro
		var r : Ref<Int> = 10;
		#end
	}

}