package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.UploadDataCompletedEventArgs")
extern class UploadDataCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets the server reply to a data upload operation started by calling an  method.
	 * @return A  array containing the server reply.
	 */
	var Result(default, never):cs.NativeArray<cs.UInt8>;
}
