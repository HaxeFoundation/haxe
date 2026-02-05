package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.UploadStringCompletedEventArgs")
extern class UploadStringCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets the server reply to a string upload operation that is started by calling an
	 * method.
	 * @return A  array that contains the server reply.
	 */
	var Result(default, never):String;
}
