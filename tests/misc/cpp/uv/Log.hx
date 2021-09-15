import haxe.PosInfos;

class Log {
	static final T0 = haxe.Timer.stamp();

	public static function print(msg:String, ?pos:PosInfos) {
		var slashPos = pos.fileName.lastIndexOf('/');
		if(slashPos < 0)
			slashPos = pos.fileName.lastIndexOf('\\');
		var namePos = slashPos < 0 ? 0 : slashPos + 1;
		var sampleName = pos.fileName.substr(namePos);
		var stamp = Std.int((haxe.Timer.stamp() - T0) * 100);
		Sys.println('[$stamp] $sampleName:${pos.lineNumber}: $msg');
	}
}