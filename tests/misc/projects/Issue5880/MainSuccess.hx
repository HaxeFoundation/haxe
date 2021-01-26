abstract Ref<T>({ v : T }) {

	@:from macro static function from(v) {
		return macro new Ref($v);
	}

	public function new(v:T) {
		this = { v : v }
	}
}

class MainSuccess {

	static function main() {
		#if !macro
		var r : Ref<Int> = 10;
		trace(r);
		#end
	}

}