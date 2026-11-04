function passMethod<T>(f:T->Void) {}
@:generic
class Generic<T> {
	var foo:T;
	public function new()
	{
		passMethod(method);
	}

	function method(value:T) {}
}

typedef Instance = Generic<Int>;
