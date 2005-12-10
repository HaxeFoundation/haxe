interface LogInfos {
	var fileName : String;
	var lineNumber : Int;
	var className : String;
	var methodName : String;
	var customParams : Array<Dynamic>;
}


class Log {

	public static function trace( v : Dynamic, infos : LogInfos ) : Void {
		#flash
		untyped Boot.__trace(v,infos);
		#else neko
		untyped __dollar__print(infos.fileName+":"+infos.lineNumber+": ",v,"\n");
		#else error
		#end
	}

	public static function clear() : Void {
		#flash
		untyped Boot.__clear_trace();
		#else neko
		// nothing
		#else error
		#end
	}

}