class Main {
    static function main() {
		haxe.Log.trace = function(s,?p) {
			Sys.stderr().writeString("" + s + " " + p.customParams[0]);
		}
		var n:UInt = -4;
		if ( n > 0 )
			trace("positive", n);
		else
			trace("negative", n);
    }
}