#!/usr/bin/env python3
"""Runs the eval benchmarks with --interp, as a macro and on node, several
times, checks that every mode computes the same checksums, and prints the
median times (ms) as a Markdown table.

Usage: run.py [--haxe <haxe binary>] [--runs <n>] [--modes interp,macro,js]"""

import argparse
import os
import statistics
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def run(haxe, hxml):
    out = subprocess.run([haxe, hxml], cwd=HERE, capture_output=True, text=True)
    if out.returncode != 0:
        sys.exit("%s failed:\n%s%s" % (hxml, out.stdout, out.stderr))
    results = {}
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) == 3:
            results[parts[0]] = (int(parts[1]), parts[2])
    return results


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--haxe", default="haxe")
    ap.add_argument("--runs", type=int, default=3)
    ap.add_argument("--modes", default="interp,macro,js")
    args = ap.parse_args()
    modes = args.modes.split(",")
    times = {m: {} for m in modes}
    sums = {}
    for _ in range(args.runs):
        for m in modes:
            for name, (t, s) in run(args.haxe, m + ".hxml").items():
                times[m].setdefault(name, []).append(t)
                if sums.setdefault(name, s) != s:
                    sys.exit("checksum mismatch for %s in mode %s: %s, expected %s" % (name, m, s, sums[name]))
    names = list(times[modes[0]])
    header = ["case"] + modes
    if "js" in modes:
        header += ["%s/js" % m for m in modes if m != "js"]
    print("| " + " | ".join(header) + " |")
    print("|" + "---|" * len(header))
    for n in names:
        med = {m: statistics.median(times[m][n]) for m in modes}
        row = [n] + ["%d" % med[m] for m in modes]
        if "js" in modes:
            row += ["%.1f×" % (med[m] / max(med["js"], 1)) for m in modes if m != "js"]
        print("| " + " | ".join(row) + " |")


if __name__ == "__main__":
    main()
