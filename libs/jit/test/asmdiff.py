#!/usr/bin/env python3
"""Compares the object files assembled by GNU as with the output of
X86_encode written by asmdiff.exe (<obj>.jit), section by section.

Each allocated section of the object file is matched with the place of its
first symbol in the JIT output; bytes covered by a relocation on either side,
and alignment padding of the JIT output, are not compared.

Usage: asmdiff.py <object files...>"""

import struct
import subprocess
import sys

SHF_ALLOC = 0x2
SHF_EXECINSTR = 0x4
SHT_SYMTAB = 2
SHT_RELA = 4

# relocation type -> size of the field
RELOC_SIZE = {1: 8, 2: 4, 4: 4, 9: 4, 10: 4, 11: 4, 24: 8, 41: 4, 42: 4}


def read_elf(path):
    data = open(path, "rb").read()
    (shoff,) = struct.unpack_from("<Q", data, 0x28)
    shentsize, shnum, shstrndx = struct.unpack_from("<HHH", data, 0x3A)
    sections = []
    for i in range(shnum):
        name, typ, flags, _addr, off, size, link, info, _align, entsize = struct.unpack_from(
            "<IIQQQQIIQQ", data, shoff + i * shentsize)
        sections.append(dict(name_off=name, type=typ, flags=flags, off=off, size=size,
                             link=link, info=info, entsize=entsize))
    strtab = sections[shstrndx]

    def cstr(base, off):
        end = data.index(b"\0", base + off)
        return data[base + off:end].decode()

    for s in sections:
        s["name"] = cstr(strtab["off"], s["name_off"])
        s["bytes"] = data[s["off"]:s["off"] + s["size"]] if s["type"] != 8 else b"\0" * s["size"]
        s["relocs"] = []
        s["syms"] = []
    for s in sections:
        if s["type"] == SHT_SYMTAB:
            names = sections[s["link"]]
            for k in range(s["size"] // 24):
                st_name, st_info, _other, shndx, value, _size = struct.unpack_from(
                    "<IBBHQQ", data, s["off"] + 24 * k)
                if st_name and 0 < shndx < len(sections) and (st_info & 0xF) != 3:
                    sections[shndx]["syms"].append((value, cstr(names["off"], st_name)))
        elif s["type"] == SHT_RELA:
            target = sections[s["info"]]
            for k in range(s["size"] // 24):
                off, info, _addend = struct.unpack_from("<QQq", data, s["off"] + 24 * k)
                typ = info & 0xFFFFFFFF
                if typ not in RELOC_SIZE:
                    raise Exception("%s: relocation type %d" % (path, typ))
                target["relocs"].append((off, RELOC_SIZE[typ]))
    return sections


def read_jit(path):
    jit = dict(T=b"", D=b"", syms={}, relocs=dict(T=[], D=[]), pad=[], unsupported=None, time=0.0)
    for line in open(path):
        parts = line.split()
        if parts[0] == "TEXT":
            jit["T"] = bytes.fromhex(parts[1]) if len(parts) > 1 else b""
        elif parts[0] == "DATA":
            jit["D"] = bytes.fromhex(parts[1]) if len(parts) > 1 else b""
        elif parts[0] == "SYM":
            jit["syms"][parts[1]] = (parts[2], int(parts[3]))
        elif parts[0] == "RELOC":
            jit["relocs"][parts[1]].append((int(parts[2]), int(parts[3])))
        elif parts[0] == "PAD":
            jit["pad"].append((int(parts[1]), int(parts[2])))
        elif parts[0] == "TIME":
            jit["time"] = float(parts[1])
        elif parts[0] == "UNSUPPORTED":
            jit["unsupported"] = line.strip()
    return jit


def disasm(path, section, off):
    out = subprocess.run(["objdump", "-d", "-j", section, "--start-address=%d" % max(0, off - 8),
                          "--stop-address=%d" % (off + 16), path],
                         capture_output=True, text=True).stdout
    return "\n".join(l for l in out.splitlines() if l.startswith(" "))


TIMES = []


def compare(obj):
    jit = read_jit(obj + ".jit")
    TIMES.append((jit["time"], len(jit["T"]) + len(jit["D"])))
    if jit["unsupported"]:
        return ["%s: %s" % (obj, jit["unsupported"])], 0
    errors = []
    compared = 0
    for s in read_elf(obj):
        if not s["flags"] & SHF_ALLOC or s["size"] == 0 or s["name"].startswith(".eh_frame") \
                or s["name"].startswith(".note"):
            continue
        kind = "T" if s["flags"] & SHF_EXECINSTR else "D"
        anchors = [(v, n) for (v, n) in s["syms"] if n in jit["syms"]]
        if not anchors:
            if any(b for b in s["bytes"]):
                errors.append("%s: section %s has no symbol to match" % (obj, s["name"]))
            continue
        value, name = min(anchors)
        jkind, joff = jit["syms"][name]
        if jkind != kind:
            errors.append("%s: %s in a different kind of section" % (obj, name))
            continue
        base = joff - value
        mask = set()
        for off, size in s["relocs"]:
            mask.update(range(off, off + size))
        for off, size in jit["relocs"][kind]:
            mask.update(range(off - base, off - base + size))
        if kind == "T":
            for off, size in jit["pad"]:
                mask.update(range(off - base, off - base + size))
        ours = jit[kind]
        for i in range(s["size"]):
            if i in mask:
                continue
            j = base + i
            if j < 0 or j >= len(ours) or ours[j] != s["bytes"][i]:
                errors.append("%s: %s+%#x: as %02x, jit %s\n%s" % (
                    obj, s["name"], i, s["bytes"][i],
                    "%02x" % ours[j] if 0 <= j < len(ours) else "--",
                    disasm(obj, s["name"], i)))
                break
            compared += 1
    return errors, compared


def main():
    total = 0
    failed = 0
    unsupported = 0
    for obj in sys.argv[1:]:
        errors, compared = compare(obj)
        total += compared
        if errors:
            if "UNSUPPORTED" in errors[0]:
                unsupported += 1
            else:
                failed += 1
            for e in errors[:3]:
                print(e)
    print("%d units, %d failed, %d unsupported, %d bytes compared" % (
        len(sys.argv) - 1, failed, unsupported, total))
    t = sum(x for x, _ in TIMES)
    size = sum(y for _, y in TIMES)
    if t > 0:
        print("encoder: %.3f s for %d bytes (%.1f MB/s)" % (t, size, size / t / 1e6))
    sys.exit(1 if failed or unsupported else 0)


if __name__ == "__main__":
    main()
