import haxe.macro.Context;

class Main {
	#if !macro
	static function main(){
		test();
	}
	#end
	static macro function test() {
		var pos = Context.makePosition({min: 20, max: 23, file: 'my_template.mtt' });
		Context.warning('check pos', pos);
		Context.parse('foo', pos);
		Context.warning('check pos', pos);
		return macro null;
	}
}