class Main {
	static function main() {
		tmp = 1;
	}
	static var tmp:Int;

	#if macro
	static function checkPos() {
		haxe.macro.Context.onGenerate(function(types) {
			for (t in types) {
				switch t {
					case TInst(_.get() => cl, _) if (cl.name == "Main"):
						switch cl.statics.get()[0].expr().expr {
							case TFunction({ expr: { expr: TBlock(_), pos:pos } }):
								var p = haxe.macro.Context.getPosInfos(pos);
								if(p.min != 37 || p.max != 52) {
									throw '`static main` has invalid position for block expression: $pos';
								}
							case _: throw '`static main` is expected to have a block expression';
						}
					case _:
				}
			}
		});
	}
	#end
}