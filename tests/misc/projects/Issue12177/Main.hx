// Main.hx
import haxe.Constraints.IMap;
import haxe.ds.IntMap;

@:multiType(K)
abstract Dictionary<K, V>(IMap<K, V>) {
	public function new();

	@:to static function toIntMap<K:Int, V>(t:IMap<K, V>):IntMap<V> {
		return new IntMap<V>();
	}
}

function main() {
	final dict = new Dictionary<Int, Bool>();
}
