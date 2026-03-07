import haxe.macro.Compiler;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

class Check {
	static public function run() {

		Context.onAfterTyping(function(types) {
			for(type in types) {
				switch type {
					case TClassDecl(_.get() => cls) if (cls.name == 'Arr'):
						for(field in cls.fields.get()) {
							if(field.name == 'map') {
								var expr = field.expr();
								switch expr.expr {
									/*
										{
											var val = f(this.get);
											null;
										}
									*/
									case TFunction({expr: {expr: TBlock(exprs)}}):
										switch exprs[0].expr {
											// var val = f(this.get);
											case TVar(_, {expr: TCall({expr: TLocal(_), t: t}, _)}):
												// type of `f` from expr above
												switch t {
													//Should be `Arr.T->map.S`. Before the fix it was `map.S->map.S`
													case TFun([{t: TInst(_.toString() => 'Arr.T',[])}], _):
														//success;
														return;
													case _:
														Context.error('Invalid type of "f(get())" in Arr.map: $t', exprs[0].pos);
												}
											case _:
												Context.error('Invalid expression of Arr.map block', exprs[0].pos);
										}
									case _:
										Context.error('Invalid expression of Arr.map', expr.pos);
								}
							}
						}
					case _:
				}
			}
			Context.error('Class "Arr" was not found', Context.currentPos());
		});
	}
}