import Api;

class ApiProxy extends haxe.remoting.Proxy<Api> {}

class Main {
	static function main() {
		var proxy = new ApiProxy(new ConnectionStub());
		var result = proxy.getResult();
		if (result.id != 1 || result.name != "test")
			throw "assert";
	}
}

class ConnectionStub implements haxe.remoting.Connection  {
	public function new() {}
	public function resolve( name : String ) : haxe.remoting.Connection return this;
	public function call( params : Array<Dynamic> ) : Dynamic return {id: 1, name: "test"}
}
