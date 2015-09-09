package haxe.macro;

import haxe.macro.Expr;

class CallbackBuilder {
	#if macro
	static public function build() {
		switch (Context.getLocalType()) {
			case TInst(_, [Context.follow(_) => TFun(args, ret)]):
				var argsComplexTypes = [for (a in args) {
					if (a.opt)
						TOptional(Context.toComplexType(a.t));
					else
						Context.toComplexType(a.t);
				}];
				var retComplexType = Context.toComplexType(ret);
				var types = [for (len in 0...args.length) argsComplexTypes.slice(0, len+1)];

				// add Void->X
				types.unshift([macro:Void]);

				return either([for (ts in types) TFunction(ts, retComplexType)]);
			case _: throw "Type param of Callback should be a function type.";
		}
		return null;
	}
	#end

	static function either(types:Array<ComplexType>):ComplexType {
		return switch (types.length) {
			case 0: 
				throw "There should be at least one type.";
			case 1: 
				types[0];
			case len:
				var type1 = types[0];
				var type2 = either(types.slice(1));
				macro:haxe.extern.EitherType<$type1, $type2>;
		}
	}
}