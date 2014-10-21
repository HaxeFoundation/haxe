package cs;

/**
	This macro will do the type check of the native C# macros.
**/
@:noPackageRestrict
@:autoBuild(cs.internal.StrictMetaMacro.build()) interface StrictMeta
{
}
