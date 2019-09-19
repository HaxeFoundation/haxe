import asys.CurrentProcess;

class IpcEcho {
	public static function main():Void {
		CurrentProcess.initUv();
		CurrentProcess.initIpc(0);
		var done = false;
		CurrentProcess.messageSignal.on(message -> {
			CurrentProcess.send(message);
			@:privateAccess CurrentProcess.ipc.destroy((err) -> {
				if (err != null) trace("err", err);
				done = true;
			});
		});
		while (!done)
			CurrentProcess.runUv(RunOnce);
		CurrentProcess.stopUv();
	}
}
