interface ITest<K, V> {
	function keys():Array<K>;
	function values():Array<V>;
}

interface ISubTest extends ITest<String, String> {}
