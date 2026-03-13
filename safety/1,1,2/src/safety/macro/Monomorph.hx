package safety.macro;

/**
 *  Used for auto-casting array declarations to `SafeArray`.
 */
#if !macro
@:genericBuild(safety.macro.MonomorphBuilder.build())
class Monomorph<Const> {}
#end

class MonomorphBuilder {
	public static function build() return null;
}