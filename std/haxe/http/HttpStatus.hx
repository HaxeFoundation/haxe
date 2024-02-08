package haxe.http;

/**
	HTTP Request Status
**/
enum abstract HttpStatus(Int) from Int to Int {
	var Continue = 100;
	var SwitchingProtocols = 101;
	var Processing = 102;
	var OK = 200;
	var Created = 201;
	var Accepted = 202;
	var NonAuthoritativeInformation = 203;
	var NoContent = 204;
	var ResetContent = 205;
	var PartialContent = 206;
	var MultiStatus = 207;
	var AlreadyReported = 208;
	var IMUsed = 226;
	var MultipleChoices = 300;
	var MovedPermanently = 301;
	var Found = 302;
	var SeeOther = 303;
	var NotModified = 304;
	var UseProxy = 305;
	var SwitchProxy = 306;
	var TemporaryRedirect = 307;
	var PermanentRedirect = 308;
	var BadRequest = 400;
	var Unauthorized = 401;
	var PaymentRequired = 402;
	var Forbidden = 403;
	var NotFound = 404;
	var MethodNotAllowed = 405;
	var NotAcceptable = 406;
	var ProxyAuthenticationRequired = 407;
	var RequestTimeout = 408;
	var Conflict = 409;
	var Gone = 410;
	var LengthRequired = 411;
	var PreconditionFailed = 412;
	var PayloadTooLarge = 413;
	var URITooLong = 414;
	var UnsupportedMediaType = 415;
	var RangeNotSatisfiable = 416;
	var ExpectationFailed = 417;
	var ImATeapot = 418;
	var MisdirectedRequest = 421;
	var UnprocessableEntity = 422;
	var Locked = 423;
	var FailedDependency = 424;
	var UpgradeRequired = 426;
	var PreconditionRequired = 428;
	var TooManyRequests = 429;
	var RequestHeaderFieldsTooLarge = 431;
	var UnavailableForLegalReasons = 451;
	var InternalServerError = 500;
	var NotImplemented = 501;
	var BadGateway = 502;
	var ServiceUnavailable = 503;
	var GatewayTimeout = 504;
	var HTTPVersionNotSupported = 505;
	var VariantAlsoNegotiates = 506;
	var InsufficientStorage = 507;
	var LoopDetected = 508;
	var NotExtended = 510;
	var NetworkAuthenticationRequired = 511;
}
