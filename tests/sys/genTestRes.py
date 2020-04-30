#!/usr/bin/env python3

# Generates files and directories in test-res used for Unicode sys tests.
# The test vector printf'ed into data.bin, as well as the names in filenames()
# should correspond exactly to the sequences in UnicodeSequences.valid.

# Run with:
#   python3 genTestRes.py
# Or:
#   python3 genTestRes.py TEST_INVALID_UNICODE_FS
# The latter will attempt to create filenames which contain invalid Unicode
# codepoints; this does not work on some filesystems, e.g. APFS.

import os
import shutil
import sys

MODE = " ".join(sys.argv[1:])

TESTDIR = "test-res"

# delete previous, if any
if os.path.isdir(TESTDIR):
  shutil.rmtree(TESTDIR)

os.mkdir(TESTDIR)

# Unicode test vectors
allUnicode = [
    [0x01], # will not work on NTFS
    [0x7F],
    [0xC2, 0x80],
    [0xDF, 0xBF],
    [0xE0, 0xA0, 0x80],
    [0xED, 0x9F, 0xBF], # will not work on APFS
    [0xEE, 0x80, 0x80],
    [0xEF, 0xBF, 0xBD],
    [0xF0, 0x90, 0x80, 0x80],
    [0xF0, 0x9F, 0xBF, 0xBF], # will not work on APFS
    [0xF3, 0xBF, 0xBF, 0xBF], # will not work on APFS
    [0xF4, 0x80, 0x80, 0x80],
    [0xF4, 0x8F, 0xBF, 0xBF], # will not work on APFS
    [0xF0, 0x9F, 0x98, 0x82, 0xF0, 0x9F, 0x98, 0x84, 0xF0, 0x9F, 0x98, 0x99],
    [0xC8, 0xA7],
    [0xE4, 0xB8, 0xAD, 0xE6, 0x96, 0x87, 0xEF, 0xBC, 0x8C, 0xE3, 0x81, 0xAB, 0xE3, 0x81, 0xBB, 0xE3, 0x82, 0x93, 0xE3, 0x81, 0x94]
  ]

allStrings = [ bytes(data).decode("utf-8") for data in allUnicode ]

allFilenames = allStrings[:]
# Windows does not allow codepoints in the U+0000 - U+001F range
# see https://docs.microsoft.com/en-us/windows/desktop/FileIO/naming-a-file
if os.name == "nt":
  allFilenames.remove(bytes([0x01]).decode("utf-8"))

# on APFS (macOS 10.13+), filenames must consist of valid Unicode codepoints
if MODE != "TEST_INVALID_UNICODE_FS":
  allFilenames.remove(bytes([0xED, 0x9F, 0xBF]).decode("utf-8"))
  allFilenames.remove(bytes([0xF0, 0x9F, 0xBF, 0xBF]).decode("utf-8"))
  allFilenames.remove(bytes([0xF3, 0xBF, 0xBF, 0xBF]).decode("utf-8"))
  allFilenames.remove(bytes([0xF4, 0x8F, 0xBF, 0xBF]).decode("utf-8"))

allBinary = b""
for data in allUnicode:
  allBinary += bytes(data) + b"\n"

# generate a file with Unicode data
with open(os.path.join(TESTDIR, "data.bin"), "wb") as f:
  f.write(allBinary)

# generate sub-directories with symlinks
os.mkdir(os.path.join(TESTDIR, "a"))
for data in allFilenames:
  os.mkdir(os.path.join(TESTDIR, data))
  os.mkdir(os.path.join(TESTDIR, "a", data))
  if os.name != "nt":
    for target, name in [
      ("../../bin/cpp/UtilityProcess-debug", "bin-cpp-debug"),
      ("../../bin/cpp/UtilityProcess", "bin-cpp"),
      ("../../bin/cs/bin/UtilityProcess-Debug.exe", "bin-cs-debug"),
      ("../../bin/cs/bin/UtilityProcess.exe", "bin-cs"),
      ("../../bin/hl/UtilityProcess.hl", "bin-hl"),
      ("../../bin/lua/UtilityProcess.lua", "bin-lua"),
      ("../../bin/java/UtilityProcess-Debug.jar", "bin-java-debug"),
      ("../../bin/java/UtilityProcess.jar", "bin-java"),
      ("../../bin/jvm/UtilityProcess-Debug.jar", "bin-jvm-debug"),
      ("../../bin/jvm/UtilityProcess.jar", "bin-jvm"),
      ("../../bin/neko/UtilityProcess.n", "bin-neko"),
      ("../../bin/php/UtilityProcess/index.php", "bin-php"),
      ("../../bin/python/UtilityProcess.py", "bin-py"),
      ("../../src/UtilityProcess.hx", "bin-eval")
    ]:
      os.symlink(target, os.path.join(TESTDIR, data, name), target_is_directory = False)

# files
os.mkdir(os.path.join(TESTDIR, "b"))
for data in allFilenames:
  with open(os.path.join(TESTDIR, "b", data), "wb") as f:
    f.write(allBinary)
