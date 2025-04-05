import haxe.macro.Expr;

final calls = [];
final builds = [];

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

function build() {
	b("build");
	return [];
}

function Build() {
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

	static function build() {
		b("Macro.build");
		return [];
	}

	static function Build() {
		b("Macro.Build");
		return [];
	}
}
