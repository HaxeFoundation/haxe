import haxe.macro.Context;

class Main {
	#if !macro
	static function main(){
		test();
	}
	#end
	static macro function test() {
		var pos = Context.makePosition({min: 20, max: 23, file: 'my_template.mtt' });
		Context.warning(Std.string(pos), pos);
		Context.parse('foo', pos);
		Context.warning(Std.string(pos), pos);
		return macro null;
	}
}