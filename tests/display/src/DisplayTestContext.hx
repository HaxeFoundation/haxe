#if (display.protocol == "jsonrpc")
typedef DisplayTestContext = RpcDisplayTestContext;
#else
typedef DisplayTestContext = XmlDisplayTestContext;
#end
