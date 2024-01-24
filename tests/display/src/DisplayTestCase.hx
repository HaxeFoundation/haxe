#if (display.protocol == "jsonrpc")
typedef DisplayTestCase = RpcDisplayTestCase;
#else
typedef DisplayTestCase = XmlDisplayTestCase;
#end
