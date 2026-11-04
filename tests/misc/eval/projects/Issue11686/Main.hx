class Main {
    static public function main() {
        trace("Working archive");
        var zipread = sys.io.File.read("./test_zip_roller.zip", true);
        var zipfile_entries = haxe.zip.Reader.readZip(zipread);
        for (entry in zipfile_entries) {
            trace("[", entry.fileName, entry.fileSize, entry.crc32, "]");
            trace("[", entry.data.toString(), "]");
        }
        trace("Broken archive");
        var zipread = sys.io.File.read("./test_zip_7zip.zip", true);
        var zipfile_entries = haxe.zip.Reader.readZip(zipread);
        for (entry in zipfile_entries) {
            trace("[", entry.fileName, entry.fileSize, entry.crc32, "]");
            trace("[", entry.data.toString(), "]");
        }
        trace("Archive from neko haxe.zip.Writer");
        var zipread = sys.io.File.read("./test_neko_zip_writer.zip", true);
        var zipfile_entries = haxe.zip.Reader.readZip(zipread);
        for (entry in zipfile_entries) {
            trace("[", entry.fileName, entry.fileSize, entry.crc32, "]");
            trace("[", entry.data.toString(), "]");
        }
    }
}
