package unit.hxcpp_issues;

import haxe.macro.Context;
import haxe.macro.Expr;

#if !macro
@:build(unit.hxcpp_issues.Issue193.build())
#end
class Issue193 extends Test
{
   var field:Int;

   inline function incField()  field++;

   inline static function doInc(t:Issue193) t.incField();

   macro public static function build ():Array<Field> {
      var fields = Context.getBuildFields();
      var newField = macro { 
        doInc(null);
      };
      fields.push ({ name: "genField", access: [ APublic ], kind: FFun({ args: [], expr: newField, params: [], ret: null }), pos: Context.currentPos() });
      return fields;
   }

   function test()
   {
      var field = Reflect.field(this,"genField");
      t(field!=null);
   }
}

