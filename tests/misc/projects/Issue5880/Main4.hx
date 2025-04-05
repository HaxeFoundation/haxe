abstract Ref(Array<Dynamic>) {

	@:from macro static function from(v) {
		return v;
	}

}

class Main4 {

	static function main() {
		#if !macro
		var r : Ref = ["foo", 12];
		trace(r);
		#end
	}

}