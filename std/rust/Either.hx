package rust;

@:native("either.Either") extern enum Either<A, B> {
	Left(a:A);
	Right(b:B);
}