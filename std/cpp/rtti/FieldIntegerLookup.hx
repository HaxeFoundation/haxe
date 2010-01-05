package cpp.rtti;

/**
	If you implement this interface, then the backend will generate code that
	allows fast dynamic access to fields by integer id.  This should speed up the CFFI.
**/
interface FieldIntegerLookup {
}
