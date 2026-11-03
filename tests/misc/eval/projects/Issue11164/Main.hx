typedef A<K = String, V> = haxe.ds.BalancedTree<K,V>;

class B<T1 = Int, T2 = String, T3> {}

class DefaultGeneric<T = String> {
	public function new() {}
}

class Main {
	static function main() {
		var a:A<default,Int> = null;
		$type(a);

		var b:B<Bool,default,Int> = null;
		$type(b);

		var c = new DefaultGeneric();
		$type(c);

		var d = new DefaultGeneric<default>();
		$type(d);

		var e:DefaultGeneric = new DefaultGeneric();
		$type(e);

		var f:DefaultGeneric<default> = new DefaultGeneric();
		$type(f);
	}
}
