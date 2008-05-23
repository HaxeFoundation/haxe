package unit;

class RemotingApi #if swf_mark implements mt.Protect #end {

	var sub : RemotingApi;

	public function new() {
		sub = this;
	}

	public function add(x : Int,y : Int) {
		return x + y;
	}

	public function id( str : String ) {
		return str;
	}

	public function arr( a : Array<String> ) {
		return a.join("#");
	}

	public function exc( v : Dynamic ) {
		if( v != null )
			throw v;
	}

	public static function context( ?ctx ) {
		if( ctx == null ) ctx = new haxe.remoting.Context();
		ctx.addObject("api",new RemotingApi());
		ctx.addObject("apirec",new RemotingApi(),true);
		return ctx;
	}

}
