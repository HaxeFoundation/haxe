package unit;

import haxe.macro.*;
using haxe.locale.Translator;

class TestTranslator extends Test {

	macro static function setupLocale():Expr
	{
		Compiler.define("locale", "zh_CN.UTF-8");
		return macro null;
	}

	macro static function setupTranslation():Expr
	{
		Translator.addTranslationFile("zh_CN.UTF-8", "unit/translation.zh_CN.UTF-8.json");
		return macro null;
	}

	#if !macro
	public function test()
	{
		eq({ var who = "World".translate(); Translator.translate('Hello, $who!'); }, "Hello, World!");
		setupTranslation();
		eq({ var who = "World".translate(); Translator.translate('Hello, $who!'); }, "Hello, World!");
		#if run_time_translation
			Translator.runTimeLocale = "zh_CN.UTF-8";
			eq({ var who = "World".translate(); Translator.translate('Hello, $who!'); }, "世界，你好！");
			Translator.runTimeLocale = null;
			eq({ var who = "World".translate(); Translator.translate('Hello, $who!'); }, "Hello, World!");
		#else
			setupLocale();
			eq({ var who = "World".translate(); Translator.translate('Hello, $who!'); }, "世界，你好！");
		#end
	}
	#end
	
}
