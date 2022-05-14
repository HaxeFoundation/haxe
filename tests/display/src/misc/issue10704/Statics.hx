package misc.issue10704;

class Statics {
	public static final fooPublic = 0;

	@:noCompletion
	public static final fooNoCompletion = 0;

	static final fooPrivate = 0;
}
