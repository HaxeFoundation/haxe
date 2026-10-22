import haxe.macro.Expr;

// read by other expansions: macro statics are reset for every expansion
@:persistent final calls:Array<String> = [];
@:persistent final builds:Array<String> = [];

macro function getCalls() {
	return macro $v{calls};
}

macro function getBuilds() {
	return macro $v{builds};
}

function c(name) {
	calls.push(name);
}

function b(name) {
	builds.push(name);
}

function lowerCase() {
	c("lowerCase");
}

function UpperCase() {
	c("UpperCase");
}

function build():Array<Field> {
	b("build");
	return [];
}

function Build():Array<Field> {
	b("Build");
	return [];
}

class Macro {
	static function lowerCase() {
		c("Macro.lowerCase");
	}

	static function UpperCase() {
		c("Macro.UpperCase");
	}

	static function build():Array<Field> {
		b("Macro.build");
		return [];
	}

	static function Build():Array<Field> {
		b("Macro.Build");
		return [];
	}
}
