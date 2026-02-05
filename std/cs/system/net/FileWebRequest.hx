package cs.system.net;

/** Provides a file system implementation of the  class. */
@:native("System.Net.FileWebRequest")
extern class FileWebRequest extends cs.system.net.WebRequest {
	/** Cancels a request to an Internet resource. */
	function Abort():Void;
	/**
	 * Begins an asynchronous request for a  object to use to write data.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetRequestStream(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous request for a file system resource.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetResponse(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends an asynchronous request for a  instance that the application uses to write
	 * data.
	 * @param asyncResult An  that references the pending request for a stream.
	 * @return A  object that the application uses to write data.
	 */
	function EndGetRequestStream(asyncResult:cs.system.IAsyncResult):cs.system.io.Stream;
	/**
	 * Ends an asynchronous request for a file system resource.
	 * @param asyncResult An  that references the pending request for a response.
	 * @return A  that contains the response from the file system resource.
	 */
	function EndGetResponse(asyncResult:cs.system.IAsyncResult):cs.system.net.WebResponse;
	/**
	 * Returns a  object for writing data to the file system resource.
	 * @return A  for writing data to the file system resource.
	 */
	function GetRequestStream():cs.system.io.Stream;
	function GetRequestStreamAsync():cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	/**
	 * Returns a response to a file system request.
	 * @return A  that contains the response from the file system resource.
	 */
	function GetResponse():cs.system.net.WebResponse;
	function GetResponseAsync():cs.system.threading.tasks.Task_1<cs.system.net.WebResponse>;
}
