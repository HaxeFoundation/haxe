abstract Under<T>(T) {
	@:from public static function from<T>(v:Under<T>) { return v; }
}

class Main3 {
	static public function main() {
		var a1:Under<{}> = ident;
	}
}