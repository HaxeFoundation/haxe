import haxe.macro.Context;
import haxe.macro.Expr;

class Macro 
{
	static function build() : Array<Field>
	{
		Context.getModule("Empty");
		
		/*var klass = Context.getLocalClass().get();
		if (klass.name == "ChildChild")
		{
			var fields = Context.getBuildFields();
			trace("\rChildChild fields:\r" + fields.map(function(f) return "\tField: " + f.name + " " + f.access).join("\r"));
		}*/
		
		return null;
	}
}
