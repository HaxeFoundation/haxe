class Main {
	public static function main() {
		trace(markup(<something onClick={e -> e.preventDefault()} />));
		trace(markup(<something isSelected={42 > 0} />));
	}

	static macro function markup(e) {
		return switch e {
			case macro @:markup $v: macro "ok";
			case _: throw 'Not markup';
		};
	}
}
