package eval.luv;

/**
	Handles, which could be sent via `eval.luv.Stream.write2`
	and received via `eval.luv.Pipe.receiveHandle`.
**/
enum TransferableHandle {
	TTcp(t:Tcp);
	TPipe(p:Pipe);
}