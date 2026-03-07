import haxe.macro.Context;
import haxe.macro.PositionTools;

class Main {
	#if !macro
	static function main(){
		test();
	}
	#end
	static macro function test() {
		var pos = Context.makePosition({min: 20, max: 23, file: 'my_template.mtt' });
		var range = PositionTools.toLocation(pos).range;
		if(range.start.line != range.end.line || range.end.line != 2) {
			Context.fatalError('Invalid position', pos);
		}
		Context.parse('foo', pos);
		if(range.start.line != range.end.line || range.end.line != 2) {
			Context.fatalError('Invalid position after Context.parse', pos);
		}
		return macro null;
	}
}