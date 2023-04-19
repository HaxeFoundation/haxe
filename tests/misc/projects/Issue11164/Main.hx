typedef A<K = String, V> = haxe.ds.BalancedTree<K,V>;

class B<T1 = Int, T2 = String, T3> {}

class Main {
	static function main() {
		var a:A<_,Int> = null;
		$type(a);

		var b:B<Bool,_,Int> = null;
		$type(b);
	}
}
