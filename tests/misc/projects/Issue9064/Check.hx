import haxe.macro.Type;
import haxe.macro.Context;

class Check {
	static public function init() {
		Context.onGenerate(function(types:Array<Type>) {
			for(type in types) {
				switch type {
					case TInst(_.get() => cls, []) if(cls.name == 'Main'):
						for(field in cls.statics.get()) {
							if(field.name == 'testMeta') {
								var pureCount = field.meta.extract(':pure').length;
								if(pureCount != 1) {
									Context.error('Main.testMeta is expected to have exactly one @:pure meta, got $pureCount', field.pos);
									return;
								}
								//success
								return;
							}
						}
					case _:
				}
			}
			Context.error('Main.testMeta not found', (macro {}).pos);
		});
	}
}