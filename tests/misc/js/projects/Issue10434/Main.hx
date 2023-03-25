extern class ToolCache {
	overload static function extractTar(?flags:Array<String>):Void;
	overload static function extractTar(?flags:String):Void;
}

function main() {
	ToolCache.extractTar();
}