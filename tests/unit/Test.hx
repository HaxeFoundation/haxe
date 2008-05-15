package unit;

class Test {

	public function new() {
	}

	function eq<T>( v : T, v2 : T, ?pos : haxe.PosInfos ) {
		count++;
		if( v != v2 ) report(v+" != "+v2,pos);
	}

	function exc( f : Void -> Void, ?pos : haxe.PosInfos ) {
		count++;
		try {
			f();
			report("No exception occured",pos);
		} catch( e : Dynamic ) {
		}
	}

	function unspec( f : Void -> Void, ?pos : haxe.PosInfos ) {
		count++;
		try {
			f();
		} catch( e : Dynamic ) {
		}
	}

	function allow<T>( v : T, values : Array<T>, ?pos : haxe.PosInfos ) {
		count++;
		for( v2 in values )
			if( v == v2 )
				return;
		report(v+" not in "+Std.string(values),pos);
	}

	function infos( m : String ) {
		reportInfos = m;
	}

	static var count = 0;
	static var reportInfos = null;
	static var reportCount = 0;

	static function report( msg : String, pos : haxe.PosInfos ) {
		if( reportInfos != null ) {
			msg += " ("+reportInfos+")";
			reportInfos = null;
		}
		haxe.Log.trace(msg,pos);
		reportCount++;
		if( reportCount > 10 ) throw "Too many errors";
	}

	static function main() {
		#if neko
		if( neko.Web.isModNeko ) neko.Lib.print("<pre>");
		#end
		var classes = [
			new TestBytes(),
			new TestIO(),
		];
		var current = null;
		try {
			for( inst in classes ) {
				current = Type.getClass(inst);
				for( f in Type.getInstanceFields(current) )
					if( f.substr(0,4) == "test" )
						Reflect.callMethod(inst,Reflect.field(inst,f),[]);
			}
			report("DONE ["+count+" tests]",here);
		} catch( e : Dynamic ) {
			reportInfos = null;
			var msg = "???";
			var stack = haxe.Stack.toString(haxe.Stack.exceptionStack());
			try msg = Std.string(e) catch( e : Dynamic ) {};
			reportCount = 0;
			report("ABORTED : "+msg+" in "+Type.getClassName(current),here);
			trace("STACK :\n"+stack);
		}
	}

}