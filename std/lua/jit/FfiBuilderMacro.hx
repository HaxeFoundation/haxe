package lua.jit;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.TypeTools;
using haxe.macro.ComplexTypeTools;

@:noPackageRestrict
class FfiBuilderMacro {
	static function ctype(t : Type) : String{
		return if (t.unify(Context.getType("Int"))){
			"int ";
		} else if (t.unify(Context.getType("String"))){
			"const char *";
		} else {
			"const char *";
		}
	}
	macro public function build() : Array<Field>{
		var fields =  Context.getBuildFields();
		var b = [];
		var cls = Context.getLocalClass().get();
		if (cls.meta.has(":luaFfiLoad")){
			var loads = cls.meta.extract(":luaFfiLoad");
			for (load in loads){
				for (param in load.params){
					b.push(macro lua.Ffi.load($param));
				}
			}
		}
		for (f in fields){
			switch(f.kind){
				case FieldType.FFun(fn) : {
					var ret_type = fn.ret.toType();
					var ret = ctype(ret_type);
					var args =  [];
					for (arg in fn.args){
						var type = ctype(arg.type.toType());
						args.push('${type}${arg.name}');
					}
					b.push(macro lua.Ffi.cdef('${ret}${f.name}(${args.join(", ")});'));
				}
				default : null;
			}
		}

		fields.push({
			name : "__init__",
			access : [Access.AStatic],
			pos : Context.currentPos(),
			kind :FieldType.FFun({
				args  : [],
				expr : macro $b{b},
				params : [],
				ret : Context.getType("Void").toComplexType()
			})
		});
		cls.meta.add(":native", [ macro "__lua_Ffi.C"], Context.currentPos());
		return fields;
	}
}
