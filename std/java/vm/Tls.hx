package java.vm;

/**
	Thread-local Storage implementation
**/
@:native('haxe.java.vm.Tls') class Tls<T>
{
	var t : java.lang.ThreadLocal<T>;
	public var value(get,set):T;

	public function new()
	{
		this.t = new java.lang.ThreadLocal();
	}

	inline private function get_value():T
	{
		return t.get();
	}

	inline private function set_value(v:T):T
	{
		t.set(v);
		return v;
	}

}
