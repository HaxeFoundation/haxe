package haxe.web;
/**
* HTTP defines methods (sometimes referred to as verbs) to indicate the desired action to be performed on the identified resource. What this resource represents, whether pre-existing data or data that is generated dynamically, depends on the implementation of the server. Often, the resource corresponds to a file or the output of an executable residing on the server. The HTTP/1.0 specification[11] defined the GET, POST and HEAD methods and the HTTP/1.1 specification[12] added 5 new methods: OPTIONS, PUT, DELETE, TRACE and CONNECT. By being specified in these documents their semantics are well known and can be depended upon. Any client can use any method and the server can be configured to support any combination of methods. If a method is unknown to an intermediate it will be treated as an unsafe and non-idempotent method. There is no limit to the number of methods that can be defined and this allows for future methods to be specified without breaking existing infrastructure.
* @class HTTPMethod
*/
@:enum abstract HTTPMethod(String) from String to String  {

/**
 * The POST method requests that the server accept the entity enclosed in the request as a new subordinate of the web resource identified by the URI. The data POSTed might be, for example, an annotation for existing resources; a message for a bulletin board, newsgroup, mailing list, or comment thread; a block of data that is the result of submitting a web form to a data-handling process; or an item to add to a database
 * @property POST
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var POST = 'POST';

/**
 * The GET method requests a representation of the specified resource. Requests using GET should only retrieve data and should have no other effect. (This is also true of some other HTTP methods.)[1] The W3C has published guidance principles on this distinction, saying, "Web application design should be informed by the above principles, but also by the relevant limitations."[13] See safe methods below.
 * @property GET
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var GET = 'GET';

/**
 * The HEAD method asks for a response identical to that of a GET request, but without the response body. This is useful for retrieving meta-information written in response headers, without having to transport the entire content.
 * @property HEAD
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var HEAD = 'HEAD';

/**
 * The PUT method requests that the enclosed entity be stored under the supplied URI. If the URI refers to an already existing resource, it is modified; if the URI does not point to an existing resource, then the server can create the resource with that URI
 * @property PUT
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var PUT = 'PUT';

/**
 * The DELETE method deletes the specified resource.
 * @property DELETE
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var DELETE = 'DELETE';

/**
 * The TRACE method echoes the received request so that a client can see what (if any) changes or additions have been made by intermediate servers
 * @property TRACE
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var TRACE = 'TRACE';

/**
 * The OPTIONS method returns the HTTP methods that the server supports for the specified URL. This can be used to check the functionality of a web server by requesting '*' instead of a specific resource.
 * @property OPTIONS
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var OPTIONS = 'OPTIONS';

/**
 * The CONNECT method converts the request connection to a transparent TCP/IP tunnel, usually to facilitate SSL-encrypted communication (HTTPS) through an unencrypted HTTP proxy.
 * @property CONNECT
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var CONNECT = 'CONNECT';

/**
 * The PATCH method applies partial modifications to a resource.
 * @property PATCH
 * @type HTTPMethod
 * @static
 * @readOnly
 */
    var PATCH = 'GET';

}
