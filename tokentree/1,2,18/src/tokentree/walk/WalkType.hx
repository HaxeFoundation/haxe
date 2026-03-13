package tokentree.walk;

class WalkType {
	public static function walkType(stream:TokenStream, parent:TokenTree) {
		switch (stream.token()) {
			case Kwd(KwdClass):
				WalkClass.walkClass(stream, parent);
			case Kwd(KwdInterface):
				WalkInterface.walkInterface(stream, parent);
			case Kwd(KwdAbstract):
				WalkAbstract.walkAbstract(stream, parent);
			case Kwd(KwdTypedef):
				WalkTypedef.walkTypedef(stream, parent);
			case Kwd(KwdEnum):
				WalkEnum.walkEnum(stream, parent);
			default:
		}
	}
}