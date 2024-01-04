class Main {
	public static function main() {}

	public static function forComparable<T : Comparable<T>>(): T->T->Void
		return (a: T, b: T) -> {}
}

typedef Comparable<T> = {
	public function compareTo(that : T) : Int;
}
