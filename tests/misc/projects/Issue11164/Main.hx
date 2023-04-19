typedef A<K = String, V> = haxe.ds.BalancedTree<K,V>;

class Main {
	static function main() {
		var a:A<_,Int> = null;
		$type(a);
	}
}
