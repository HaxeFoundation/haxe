typedef Observable<T> = {
	function flatten<U>():Stream<U>;
};
typedef Stream<T> = {
	function flatten<V>():Stream<V>;
	function takeUntilBy<U>(otherObs:Observable<U>):Stream<U>;
	function bufferWhileBy():Stream<Array<T>>;
};

class Main {

  public static function main() {
    var mouseMoves : Stream<Int> = null;
    mouseMoves.takeUntilBy(mouseMoves);
  } 
}