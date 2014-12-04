package unit.issues;

@:generic
private interface Arrayable<T>
{
    public function toArray(?target:Array<T>):Array<T>;
}

@:generic
private class KVPair<K, V>
{
    public var key:K;
    public var value:V;

    public function new():Void
    {}
}

@:generic
private class MMap<K, V> implements Arrayable<KVPair<K, V>>
{
    public function new():Void
    {}

    public function toArray(?target:Array<KVPair<K, V>>):Array<KVPair<K, V>>
    {
        if (target == null) {
            target = new Array<KVPair<K, V>>();
        }
        return target;
    }
}

class Issue2581 extends Test {
	function test() {
		var MMap:MMap<String, String> = new MMap<String, String>();
	}
}