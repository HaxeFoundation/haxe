using StringTools;

@:nullSafety
class Issue11904 {
	static function main() {}
	static function extractReturnType(hint:String):Void {
		for (i => code in hint) {}
	}
}
