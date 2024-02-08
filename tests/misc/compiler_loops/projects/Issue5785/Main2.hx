typedef Stream<T> = {
	function bufferWhileBy():Stream<Array<T>>;
}

class Main2 {
	public static function main() {
		var mouseMoves:Stream<Int> = null;
		var i:Stream<Dynamic> = mouseMoves;
	}
}