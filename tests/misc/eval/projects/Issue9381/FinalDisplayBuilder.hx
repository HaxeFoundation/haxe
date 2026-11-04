import haxe.macro.Context;

class FinalDisplayBuilder {
	#if macro
	public static function build() {
		var cls = Context.getLocalClass().get();
		if (!cls.isFinal) {
			Context.fatalError('class needs to be final', cls.pos);
		} else {
			Sys.stderr().writeString(cls.name + " is very final\n");
		}
		return null;
	}
	#end
}