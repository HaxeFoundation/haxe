package builder;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
#end

#if !macro
@:autoBuild(builder.Builder.build())
#end
interface IBuilder {}

class Builder {
	macro static public function build():Array<Field> {
		var cCur = Context.getLocalClass().get();

		function loop(c:ClassType) {
			var fs = c.fields.get();
			if (c.superClass != null) loop(c.superClass.t.get());
		}
		loop(cCur);

		return null;
	}
}