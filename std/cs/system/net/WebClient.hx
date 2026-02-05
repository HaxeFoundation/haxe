package cs.system.net;

/** Provides common methods for sending data to and receiving data from a resource identified by a URI. */
@:native("System.Net.WebClient")
extern class WebClient extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets the base URI for requests made by a .
	 * @return A  containing the base URI for requests made by a  or  if no base
	 * address has been specified.
	 */
	var BaseAddress(default, default):String;
	/**
	 * Gets or sets the application's cache policy for any resources obtained by this
	 * WebClient instance using  objects.
	 * @return A  object that represents the application's caching requirements.
	 */
	var CachePolicy(default, default):cs.system.net.cache.RequestCachePolicy;
	/**
	 * Gets or sets the network credentials that are sent to the host and used to
	 * authenticate the request.
	 * @return An  containing the authentication credentials for the request. The
	 * default is .
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * Gets or sets the  used to upload and download strings.
	 * @return A  that is used to encode strings. The default value of this property is
	 * the encoding returned by .
	 */
	var Encoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets a collection of header name/value pairs associated with the
	 * request.
	 * @return A  containing header name/value pairs associated with this request.
	 */
	var Headers(default, default):cs.system.net.WebHeaderCollection;
	/**
	 * Gets whether a Web request is in progress.
	 * @return if the Web request is still in progress; otherwise .
	 */
	var IsBusy(default, never):Bool;
	/**
	 * Gets or sets the proxy used by this  object.
	 * @return An  instance used to send requests.
	 */
	var Proxy(default, default):cs.system.net.IWebProxy;
	/**
	 * Gets or sets a collection of query name/value pairs associated with the request.
	 * @return A  that contains query name/value pairs associated with the request. If
	 * no pairs are associated with the request, the value is an empty .
	 */
	var QueryString(default, default):cs.system.collections.specialized.NameValueCollection;
	/**
	 * Gets a collection of header name/value pairs associated with the response.
	 * @return A  containing header name/value pairs associated with the response, or 
	 * if no response has been received.
	 */
	var ResponseHeaders(default, never):cs.system.net.WebHeaderCollection;
	/**
	 * Gets or sets a  value that controls whether the  are sent with requests.
	 * @return if the default credentials are used; otherwise . The default value is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	function new():Void;
	/** Cancels a pending asynchronous operation. */
	function CancelAsync():Void;
	@:overload(function(address:String):cs.NativeArray<cs.UInt8> {})
	/**
	 * Downloads the resource as a  array from the URI specified.
	 * @param address The URI from which to download data.
	 * @return A  array containing the downloaded resource.
	 */
	function DownloadData(address:cs.system.Uri):cs.NativeArray<cs.UInt8>;
	@:overload(function(address:cs.system.Uri):Void {})
	/**
	 * Downloads the resource as a  array from the URI specified as an asynchronous
	 * operation.
	 * @param address A  containing the URI to download.
	 */
	function DownloadDataAsync(address:cs.system.Uri, userToken:Dynamic):Void;
	@:overload(function(address:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	/**
	 * Downloads the resource as a  array from the URI specified as an asynchronous
	 * operation using a task object.
	 * @param address The URI of the resource to download.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the downloaded resource.
	 */
	function DownloadDataTaskAsync(address:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	@:overload(function(address:String, fileName:String):Void {})
	/**
	 * Downloads the resource with the specified URI to a local file.
	 * @param address The URI from which to download data.
	 * @param fileName The name of the local file that is to receive the data.
	 */
	function DownloadFile(address:cs.system.Uri, fileName:String):Void;
	@:overload(function(address:cs.system.Uri, fileName:String):Void {})
	/**
	 * Downloads, to a local file, the resource with the specified URI. This method
	 * does not block the calling thread.
	 * @param address The URI of the resource to download.
	 * @param fileName The name of the file to be placed on the local computer.
	 */
	function DownloadFileAsync(address:cs.system.Uri, fileName:String, userToken:Dynamic):Void;
	@:overload(function(address:String, fileName:String):cs.system.threading.tasks.Task {})
	/**
	 * Downloads the specified resource to a local file as an asynchronous operation
	 * using a task object.
	 * @param address The URI of the resource to download.
	 * @param fileName The name of the file to be placed on the local computer.
	 * @return The task object representing the asynchronous operation.
	 */
	function DownloadFileTaskAsync(address:cs.system.Uri, fileName:String):cs.system.threading.tasks.Task;
	@:overload(function(address:String):String {})
	/**
	 * Downloads the requested resource as a . The resource to download is specified as
	 * a  containing the URI.
	 * @param address A  containing the URI to download.
	 * @return A  containing the requested resource.
	 */
	function DownloadString(address:cs.system.Uri):String;
	@:overload(function(address:cs.system.Uri):Void {})
	/**
	 * Downloads the resource specified as a . This method does not block the calling
	 * thread.
	 * @param address A  containing the URI to download.
	 */
	function DownloadStringAsync(address:cs.system.Uri, userToken:Dynamic):Void;
	@:overload(function(address:String):cs.system.threading.tasks.Task_1<String> {})
	/**
	 * Downloads the resource as a  from the URI specified as an asynchronous operation
	 * using a task object.
	 * @param address The URI of the resource to download.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the downloaded resource.
	 */
	function DownloadStringTaskAsync(address:cs.system.Uri):cs.system.threading.tasks.Task_1<String>;
	@:overload(function(address:String):cs.system.io.Stream {})
	/**
	 * Opens a readable stream for the data downloaded from a resource with the URI
	 * specified as a .
	 * @param address The URI specified as a  from which to download data.
	 * @return A  used to read data from a resource.
	 */
	function OpenRead(address:cs.system.Uri):cs.system.io.Stream;
	@:overload(function(address:cs.system.Uri):Void {})
	/**
	 * Opens a readable stream containing the specified resource. This method does not
	 * block the calling thread.
	 * @param address The URI of the resource to retrieve.
	 */
	function OpenReadAsync(address:cs.system.Uri, userToken:Dynamic):Void;
	@:overload(function(address:String):cs.system.threading.tasks.Task_1<cs.system.io.Stream> {})
	/**
	 * Opens a readable stream containing the specified resource as an asynchronous
	 * operation using a task object.
	 * @param address The URI of the resource to retrieve.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  used to read data from a resource.
	 */
	function OpenReadTaskAsync(address:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	@:overload(function(address:String):cs.system.io.Stream {})
	@:overload(function(address:cs.system.Uri):cs.system.io.Stream {})
	@:overload(function(address:String, method:String):cs.system.io.Stream {})
	/**
	 * Opens a stream for writing data to the specified resource.
	 * @param address The URI of the resource to receive the data.
	 * @return A  used to write data to the resource.
	 */
	function OpenWrite(address:cs.system.Uri, method:String):cs.system.io.Stream;
	@:overload(function(address:cs.system.Uri):Void {})
	@:overload(function(address:cs.system.Uri, method:String):Void {})
	/**
	 * Opens a stream for writing data to the specified resource. This method does not
	 * block the calling thread.
	 * @param address The URI of the resource to receive the data.
	 */
	function OpenWriteAsync(address:cs.system.Uri, method:String, userToken:Dynamic):Void;
	@:overload(function(address:String):cs.system.threading.tasks.Task_1<cs.system.io.Stream> {})
	@:overload(function(address:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.system.io.Stream> {})
	@:overload(function(address:String, method:String):cs.system.threading.tasks.Task_1<cs.system.io.Stream> {})
	/**
	 * Opens a stream for writing data to the specified resource as an asynchronous
	 * operation using a task object.
	 * @param address The URI of the resource to receive the data.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  used to write data to the resource.
	 */
	function OpenWriteTaskAsync(address:cs.system.Uri, method:String):cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	@:overload(function(address:String, data:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:cs.system.Uri, data:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:String, method:String, data:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * Uploads a data buffer to a resource identified by a URI.
	 * @param address The URI of the resource to receive the data.
	 * @param data The data buffer to send to the resource.
	 * @return A  array containing the body of the response from the resource.
	 */
	function UploadData(address:cs.system.Uri, method:String, data:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	@:overload(function(address:cs.system.Uri, data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(address:cs.system.Uri, method:String, data:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * Uploads a data buffer to a resource identified by a URI, using the POST method.
	 * This method does not block the calling thread.
	 * @param address The URI of the resource to receive the data.
	 * @param data The data buffer to send to the resource.
	 */
	function UploadDataAsync(address:cs.system.Uri, method:String, data:cs.NativeArray<cs.UInt8>, userToken:Dynamic):Void;
	@:overload(function(address:String, data:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:cs.system.Uri, data:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:String, method:String, data:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	/**
	 * Uploads a data buffer that contains a  array to the URI specified as an
	 * asynchronous operation using a task object.
	 * @param address The URI of the resource to receive the data.
	 * @param data The data buffer to send to the resource.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the body of the response received
	 * from the resource when the data buffer was uploaded.
	 */
	function UploadDataTaskAsync(address:cs.system.Uri, method:String, data:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	@:overload(function(address:String, fileName:String):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:cs.system.Uri, fileName:String):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:String, method:String, fileName:String):cs.NativeArray<cs.UInt8> {})
	/**
	 * Uploads the specified local file to a resource with the specified URI.
	 * @param address The URI of the resource to receive the file. For example,
	 * ftp://localhost/samplefile.txt.
	 * @param fileName The file to send to the resource. For example, "samplefile.txt".
	 * @return A  array containing the body of the response from the resource.
	 */
	function UploadFile(address:cs.system.Uri, method:String, fileName:String):cs.NativeArray<cs.UInt8>;
	@:overload(function(address:cs.system.Uri, fileName:String):Void {})
	@:overload(function(address:cs.system.Uri, method:String, fileName:String):Void {})
	/**
	 * Uploads the specified local file to the specified resource, using the POST
	 * method. This method does not block the calling thread.
	 * @param address The URI of the resource to receive the file. For HTTP resources,
	 * this URI must identify a resource that can accept a request sent with the POST
	 * method, such as a script or ASP page.
	 * @param fileName The file to send to the resource.
	 */
	function UploadFileAsync(address:cs.system.Uri, method:String, fileName:String, userToken:Dynamic):Void;
	@:overload(function(address:String, fileName:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:cs.system.Uri, fileName:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:String, method:String, fileName:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	/**
	 * Uploads the specified local file to a resource as an asynchronous operation
	 * using a task object.
	 * @param address The URI of the resource to receive the file. For HTTP resources,
	 * this URI must identify a resource that can accept a request sent with the POST
	 * method, such as a script or ASP page.
	 * @param fileName The local file to send to the resource.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the body of the response received
	 * from the resource when the file was uploaded.
	 */
	function UploadFileTaskAsync(address:cs.system.Uri, method:String, fileName:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	@:overload(function(address:String, data:String):String {})
	@:overload(function(address:cs.system.Uri, data:String):String {})
	@:overload(function(address:String, method:String, data:String):String {})
	/**
	 * Uploads the specified string to the specified resource, using the POST method.
	 * @param address The URI of the resource to receive the string. For Http
	 * resources, this URI must identify a resource that can accept a request sent with
	 * the POST method, such as a script or ASP page.
	 * @param data The string to be uploaded.
	 * @return A  containing the response sent by the server.
	 */
	function UploadString(address:cs.system.Uri, method:String, data:String):String;
	@:overload(function(address:cs.system.Uri, data:String):Void {})
	@:overload(function(address:cs.system.Uri, method:String, data:String):Void {})
	/**
	 * Uploads the specified string to the specified resource. This method does not
	 * block the calling thread.
	 * @param address The URI of the resource to receive the string. For HTTP
	 * resources, this URI must identify a resource that can accept a request sent with
	 * the POST method, such as a script or ASP page.
	 * @param data The string to be uploaded.
	 */
	function UploadStringAsync(address:cs.system.Uri, method:String, data:String, userToken:Dynamic):Void;
	@:overload(function(address:String, data:String):cs.system.threading.tasks.Task_1<String> {})
	@:overload(function(address:cs.system.Uri, data:String):cs.system.threading.tasks.Task_1<String> {})
	@:overload(function(address:String, method:String, data:String):cs.system.threading.tasks.Task_1<String> {})
	/**
	 * Uploads the specified string to the specified resource as an asynchronous
	 * operation using a task object.
	 * @param address The URI of the resource to receive the string. For HTTP
	 * resources, this URI must identify a resource that can accept a request sent with
	 * the POST method, such as a script or ASP page.
	 * @param data The string to be uploaded.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  containing the response sent by the server.
	 */
	function UploadStringTaskAsync(address:cs.system.Uri, method:String, data:String):cs.system.threading.tasks.Task_1<String>;
	@:overload(function(address:String, data:cs.system.collections.specialized.NameValueCollection):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:cs.system.Uri, data:cs.system.collections.specialized.NameValueCollection):cs.NativeArray<cs.UInt8> {})
	@:overload(function(address:String, method:String, data:cs.system.collections.specialized.NameValueCollection):cs.NativeArray<cs.UInt8> {})
	/**
	 * Uploads the specified name/value collection to the resource identified by the
	 * specified URI.
	 * @param address The URI of the resource to receive the collection.
	 * @param data The  to send to the resource.
	 * @return A  array containing the body of the response from the resource.
	 */
	function UploadValues(address:cs.system.Uri, method:String, data:cs.system.collections.specialized.NameValueCollection):cs.NativeArray<cs.UInt8>;
	@:overload(function(address:cs.system.Uri, data:cs.system.collections.specialized.NameValueCollection):Void {})
	@:overload(function(address:cs.system.Uri, method:String, data:cs.system.collections.specialized.NameValueCollection):Void {})
	/**
	 * Uploads the data in the specified name/value collection to the resource
	 * identified by the specified URI. This method does not block the calling thread.
	 * @param address The URI of the resource to receive the collection. This URI must
	 * identify a resource that can accept a request sent with the default method.
	 * @param data The  to send to the resource.
	 */
	function UploadValuesAsync(address:cs.system.Uri, method:String, data:cs.system.collections.specialized.NameValueCollection, userToken:Dynamic):Void;
	@:overload(function(address:String, data:cs.system.collections.specialized.NameValueCollection):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:cs.system.Uri, data:cs.system.collections.specialized.NameValueCollection):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	@:overload(function(address:String, method:String, data:cs.system.collections.specialized.NameValueCollection):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	/**
	 * Uploads the specified name/value collection to the resource identified by the
	 * specified URI as an asynchronous operation using a task object.
	 * @param address The URI of the resource to receive the collection.
	 * @param data The  to send to the resource.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the response sent by the server.
	 */
	function UploadValuesTaskAsync(address:cs.system.Uri, method:String, data:cs.system.collections.specialized.NameValueCollection):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
}
