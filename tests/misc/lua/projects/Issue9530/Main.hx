using StringTools;

@:nullSafety(Strict)
class Main {
	static var f = "field";
	static function main() {
		final sclass = new String("foo");
		final scl = sclass.toUpperCase();
		trace(scl);
		final f2 = f.toUpperCase();
		trace(f2);
		final str = "str".toUpperCase();
		trace(str);
		final isS = "sss".startsWith("s");
		trace(isS);
		// test internal static call
		final i = "foo".indexOf("");
		trace(i);
		trace(("dyn" : Dynamic).toUpperCase());
	}
}
