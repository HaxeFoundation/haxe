package haxe.locale;
import haxe.macro.*;
import haxe.macro.Expr;

class Translator {

	#if run_time_translator
	public static var runTimeLocale:Null<String>;
	#end

	macro public static function translate(self:ExprOf<String>):ExprOf<String> return {
		#if run_time_translator
		runTimeTranslate(self, defaultDictionary, macro Translator.runTimeLocale);
		#else
		compileTimeTranslate(self, defaultDictionary, Context.definedValue("locale"));
		#end
	}

	#if macro

	@:access(haxe.format.JsonParser)
	public static function addTranslationFile(locale:String, translationFile:String):Void {
		var path = Context.resolvePath(translationFile);
		var content = sys.io.File.getContent(path);
		var parser = new haxe.format.JsonParser(content);
		var json = try {
			parser.parseRec();
		} catch (e:Dynamic) {
			Context.error(Std.string(e), Context.makePosition({ min: parser.pos, max: parser.pos, file: path }));
		}
		addTranslation(locale, json);
	}

	public static function addTranslation(locale:String, translation:Iterable<{ var source(default, never):String; var translated(default, never):String; }>):Void {
		var map = defaultDictionary.get(locale);
		if (map == null) {
			map = new Map<String, String>();
			defaultDictionary.set(locale, map);
		}
		for (entry in translation) {
			map.set(entry.source, entry.translated);
		}
	}

	static var defaultDictionary(default, never) = new Map<String, Map<String, String>>();

	@:noUsing
	public static function compileTimeTranslate(text:ExprOf<String>, dictionary:Map<String, Map<String, String>>, locale:Null<String>):ExprOf<String> return {
		if (locale == null) {
			text;
		} else {
			switch (text) {
				case { expr: EConst(CString(source)) }:
					var mapping = dictionary.get(locale);
					if (mapping == null) {
						text;
					} else {
						var translated = mapping.get(source);
						if (translated == null) {
							text;
						} else {
							if (MacroStringTools.isFormatExpr(text)) {
								MacroStringTools.formatString(translated, Context.currentPos());
							} else {
								Context.makeExpr(translated, Context.currentPos());
							}
						}
					}
				case { expr: EMeta({ name: ":this", params: []}, {expr: EConst(CIdent("this")) }), pos: p }:
					Context.error(translate("Using mix-in macro is not supported for string interpolation"), p);
				case { pos: p }:
					Context.error(translate("Expect a string literal"), p);
			}
		}
	}

	@:noUsing
	public static function runTimeTranslate(text:ExprOf<String>, dictionary:Map<String, Map<String, String>>, locale:ExprOf<String>):ExprOf<String> return {
		pos: Context.currentPos(),
		expr: ESwitch(
			locale,
			[
				for (locale in dictionary.keys()) {
					values: [ macro $v{locale} ],
					expr: compileTimeTranslate(text, dictionary, locale),
				}
			],
			text)
	}

	#end
}
