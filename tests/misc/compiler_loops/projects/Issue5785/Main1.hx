typedef Observable<T> = {
	function flatten<U>():Stream<U>;
}

typedef Stream<T> = {
	function flatten<V>():Stream<V>;
	function takeUntilBy<U>(otherObs:Observable<U>):Stream<U>;
}

class Main1 {

	public static function main() {
		var mouseMoves : Stream<Int> = null;
		mouseMoves.takeUntilBy(mouseMoves);
	}
}