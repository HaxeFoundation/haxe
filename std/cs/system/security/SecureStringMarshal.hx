package cs.system.security;

/** Provides a collection of methods for allocating unmanaged memory and copying unmanaged memory blocks. */
@:native("System.Security.SecureStringMarshal")
extern class SecureStringMarshal {
	/**
	 * Copies the contents of a managed  object to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where the  parameter was copied to, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToCoTaskMemAnsi(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  object to a block of memory allocated from the
	 * unmanaged COM task allocator.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where the  parameter was copied to, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToCoTaskMemUnicode(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  into unmanaged memory, converting into ANSI
	 * format as it copies.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, to where the  parameter was copied, or
	 * 0 if a null object was supplied.
	 */
	static function SecureStringToGlobalAllocAnsi(s:cs.system.security.SecureString):cs.system.IntPtr;
	/**
	 * Copies the contents of a managed  object into unmanaged memory.
	 * @param s The managed object to copy.
	 * @return The address, in unmanaged memory, where  was copied, or 0 if  is a 
	 * object whose length is 0.
	 */
	static function SecureStringToGlobalAllocUnicode(s:cs.system.security.SecureString):cs.system.IntPtr;
}
