#if macro
import haxe.macro.Context;
#end

#if !macro
class Main {
	public static function main() {
		final array:MyArray<String> = ["hello", "world"];
	}
}
#end

#if !macro
@:build(MyMacro.build())
#end
abstract MyArray<T>(Array<T>) from Array<T> to Iterable<T> {}

class MyMacro {
	#if macro
	static function build() {
		switch (Context.getLocalType()) {
			case TInst(_.get() => type, _):
				switch (type.kind) {
					case KAbstractImpl(_.get() => abstractType):
					default:
				}
			default:
		}
		return null;
	}
	#end
}