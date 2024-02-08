package utils.macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.Exception;

using StringTools;

private class BuilderException extends Exception {
	override function toString():String {
		return message;
	}
}

class DisplayTestBuilder {
	static public function build(fields:Array<Field>):Array<Field> {
		for (field in fields) {
			if (field.name.startsWith('test')) {
				try {
					patchExpr(field);
				} catch (e) {
					Context.error('Failed to build display test: $e', field.pos);
				}
			}
		}
		return fields;
	}

	static function patchExpr(field:Field) {
		switch field.kind {
			case FFun(fn):
				switch fn.expr {
					case null:
					case { expr:EBlock(exprs) }:
						exprs.unshift(generateInit(field));
					case e:
						fn.expr = macro {
							${generateInit(field)};
							$e;
						}
				}
			case _:
		}
	}

	static function generateInit(field:Field):Expr {
		return switch field.doc {
			case null:
				macro {}
			case src:
				macro {
					_markers = utils.Markers.parse($v{src});
					vfs.putContent("Main.hx", markers.source);
				}
		}
	}
}
