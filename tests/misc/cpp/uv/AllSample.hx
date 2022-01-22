class AllSample extends UVSample {
	public function run() {
		new CheckSample().run();
		new PrepareSample().run();
		new IdleSample().run();
		new TcpSample().run();
		new DnsSample().run();
		new UdpSample().run();
		new ProcessSample().run();
		new FileSyncSample().run();
		new FileSample().run();
		new DirSyncSample().run();
		new DirSample().run();
		new FsEventSample().run();
		new FsPollSample().run();
		new MiscSample().run();
		new TtySample().run();
		new SignalSample().run();
		new VersionSample().run();
		new PipeSample().run();
	}
}