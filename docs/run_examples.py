#!/usr/bin/env python3
"""Run the Mathematica code blocks embedded in the Markdown documentation.

Every fenced code block whose info string starts with ``mathematica`` (or
``wl``/``wolfram``) is executed by ``wolframscript`` in a fresh kernel.  The
output of each top-level expression is printed, mimicking a notebook, and the
block fails if the kernel emits any message (``Foo::bar: ...``), exits with a
non-zero status, or aborts before the end-of-block marker is reached.

A code block may build on a common setup:

    <!-- setup -->
    ```mathematica
    Needs["Peanotica`"];
    DefSimpleTensor[T, {mf, mf}, {SCycles@{1, 2}}];
    ```

Setup blocks are validated once, and then prepended to every later block of
the same file, so that examples stay short while still being runnable on their
own.

Supported directives (HTML comments immediately preceding a fence):

    <!-- setup -->            part of the document setup, inherited by later blocks
    <!-- skip -->             do not execute this block
    <!-- expect-error -->     the block is expected to fail (messages or non-zero exit)
    <!-- allow-messages -->   only require a zero exit status, ignore messages
    <!-- timeout: 120 -->     per-block timeout in seconds (default: --timeout)
    <!-- label: foo -->       human readable label used in the report

Usage:
    python3 docs/run_examples.py                     # all docs/**/*.md
    python3 docs/run_examples.py docs/features.md    # selected files
    python3 docs/run_examples.py --list              # show the blocks
    python3 docs/run_examples.py --filter 分块张量    # run matching blocks only
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
RUNNABLE_LANGS = {"mathematica", "wl", "wolfram", "mma"}
MESSAGE_RE = re.compile(r"(?m)^[A-Za-z][A-Za-z0-9`]*::[a-z][A-Za-z0-9]*:")
FENCE_RE = re.compile(r"^(\s*)(`{3,}|~{3,})\s*(\S*)\s*$")
HEADING_RE = re.compile(r"^(#{1,6})\s+(.*)$")
COMMENT_RE = re.compile(r"^\s*<!--\s*(.*?)\s*-->\s*$")
END_MARKER = "__PEANOTICA_DOC_BLOCK_OK__"
PARSE_MARKER = "__PEANOTICA_DOC_PARSE_ERROR__"


@dataclass
class Block:
    path: Path
    number: int
    line: int
    lang: str
    code: str
    heading: str
    directives: dict = field(default_factory=dict)

    @property
    def label(self) -> str:
        return self.directives.get("label") or f"block {self.number}"

    @property
    def setup(self) -> bool:
        return "setup" in self.directives

    @property
    def skip(self) -> bool:
        return "skip" in self.directives

    @property
    def timeout(self) -> float:
        return self.directives.get("timeout", 0.0)

    def location(self) -> str:
        try:
            path = self.path.relative_to(REPO_ROOT)
        except ValueError:
            path = self.path
        return f"{path}:{self.line}"


def parse_directives(lines: list[str], fence_index: int) -> dict:
    """Collect the HTML-comment directives directly above a fence."""
    comments: list[str] = []
    i = fence_index - 1
    while i >= 0:
        line = lines[i]
        if not line.strip():
            i -= 1
            continue
        match = COMMENT_RE.match(line)
        if match:
            comments.append(match.group(1))
            i -= 1
            continue
        break

    directives: dict = {}
    for comment in reversed(comments):
        text = comment.strip()
        if text in {"setup", "skip", "expect-error", "allow-messages"}:
            directives[text] = True
            continue
        match = re.match(r"^(timeout|label)\s*:\s*(.+)$", text)
        if match:
            key, value = match.group(1), match.group(2).strip()
            directives[key] = float(value) if key == "timeout" else value
            continue
        directives.setdefault("unknown", []).append(text)
    return directives


def parse_markdown(path: Path) -> list[Block]:
    lines = path.read_text(encoding="utf-8").splitlines()
    blocks: list[Block] = []
    heading = ""
    index = 0
    i = 0
    while i < len(lines):
        line = lines[i]
        heading_match = HEADING_RE.match(line)
        if heading_match:
            heading = heading_match.group(2).strip()
            i += 1
            continue

        fence_match = FENCE_RE.match(line)
        if not fence_match:
            i += 1
            continue

        indent, fence, info = fence_match.groups()
        if len(indent) > 3:
            # An indented fence is part of a list item / block quote, not a
            # top-level documentation block we want to execute.
            i += 1
            continue

        closing = re.compile(r"^\s*" + re.escape(fence[0]) + "{" + str(len(fence)) + r",}\s*$")
        j = i + 1
        while j < len(lines) and not closing.match(lines[j]):
            j += 1
        if j >= len(lines):
            print(f"warning: unterminated code fence at {path}:{i + 1}", file=sys.stderr)
            break

        code = "\n".join(lines[i + 1 : j])
        language = (info or "").split(",")[0].strip().lower()
        directives = parse_directives(lines, i)
        if directives.get("unknown"):
            print(
                f"warning: {path}:{i + 1}: unknown directive(s): "
                + ", ".join(directives["unknown"]),
                file=sys.stderr,
            )

        if language in RUNNABLE_LANGS:
            index += 1
            blocks.append(
                Block(
                    path=path,
                    number=index,
                    line=i + 1,
                    lang=language,
                    code=code,
                    heading=heading,
                    directives=directives,
                )
            )
        i = j + 1
    return blocks


PREAMBLE = """\
$PeanoticaDocsRoot = {root};
If[!MemberQ[$Path, ParentDirectory[$PeanoticaDocsRoot]],
    PrependTo[$Path, ParentDirectory[$PeanoticaDocsRoot]]];
Quiet[Needs["Peanotica`"], Needs::nocont];
"""


def quote(text: str) -> str:
    """Embed arbitrary Mathematica code as a Mathematica string literal."""
    return json.dumps(text, ensure_ascii=False)


def build_script(block: Block, setup_code: str) -> str:
    parts = [PREAMBLE.format(root=quote(str(REPO_ROOT)))]
    if setup_code.strip():
        parts.append(
            "Module[{peanoticaDocSetup},\n"
            f"  peanoticaDocSetup = ToExpression[{quote(setup_code)}, InputForm, Hold];\n"
            "  If[Head[peanoticaDocSetup] =!= Hold,\n"
            f"    Print[\"{PARSE_MARKER}\"]; Exit[2]];\n"
            "  Do[peanoticaDocSetup[[i]], {i, Length[peanoticaDocSetup]}]\n"
            "];\n"
        )
    parts.append(
        "Module[{peanoticaDocHeld, peanoticaDocRes},\n"
        f"  peanoticaDocHeld = ToExpression[{quote(block.code)}, InputForm, Hold];\n"
        "  If[Head[peanoticaDocHeld] =!= Hold,\n"
        f"    Print[\"{PARSE_MARKER}\"]; Exit[2]];\n"
        "  Do[\n"
        "    peanoticaDocRes = peanoticaDocHeld[[i]];\n"
        "    If[peanoticaDocRes =!= Null, Print[peanoticaDocRes]],\n"
        "    {i, Length[peanoticaDocHeld]}\n"
        "  ]\n"
        "];\n"
        f"Print[\"{END_MARKER}\"];\n"
    )
    return "".join(parts)


@dataclass
class Result:
    block: Block
    ok: bool
    duration: float
    output: str
    reason: str = ""


def run_block(block: Block, setup_code: str, default_timeout: float, keep: bool, verbose: bool) -> Result:
    timeout = block.timeout or default_timeout
    script = build_script(block, setup_code)
    tmpdir = Path(tempfile.mkdtemp(prefix="peanotica-doc-"))
    script_path = tmpdir / f"block_{block.number}.wls"
    script_path.write_text(script, encoding="utf-8")
    started = time.time()
    try:
        proc = subprocess.run(
            ["wolframscript", "-script", str(script_path)],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            timeout=timeout,
            env=None,
        )
        output = (proc.stdout or "") + (proc.stderr or "")
        duration = time.time() - started
        timed_out = False
        exit_code = proc.returncode
    except subprocess.TimeoutExpired as exc:
        duration = time.time() - started
        output = ((exc.stdout or "") if isinstance(exc.stdout, str) else "") + (
            (exc.stderr or "") if isinstance(exc.stderr, str) else ""
        )
        timed_out = True
        exit_code = -1
    finally:
        if not keep:
            shutil.rmtree(tmpdir, ignore_errors=True)

    has_messages = bool(MESSAGE_RE.search(output))
    has_marker = END_MARKER in output
    has_parse_error = PARSE_MARKER in output
    expect_error = "expect-error" in block.directives
    allow_messages = "allow-messages" in block.directives

    reasons = []
    if timed_out:
        reasons.append(f"timeout after {timeout:g}s")
    if has_parse_error:
        reasons.append("Mathematica parse error")
    if exit_code not in (0, None):
        reasons.append(f"exit status {exit_code}")
    if not has_marker and not timed_out:
        reasons.append("end-of-block marker missing (kernel aborted?)")
    if has_messages and not allow_messages:
        reasons.append("messages were emitted")

    if expect_error:
        ok = bool(reasons)
        if not ok:
            reasons = ["expected a failure, but the block succeeded"]
    else:
        ok = not reasons

    if verbose or not ok:
        print(f"\n----- output of {block.location()} ({block.label}) -----")
        text = output.replace(END_MARKER, "").replace(PARSE_MARKER, "").strip()
        print(text if text else "(no output)")
        print("----- end of output -----")

    return Result(block=block, ok=ok, duration=duration, output=output, reason="; ".join(reasons))


def collect_setup(blocks: list[Block]) -> str:
    return "\n".join(b.code for b in blocks if b.setup and not b.skip)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Execute the Mathematica code blocks of the Markdown documentation.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("paths", nargs="*", type=Path, help="Markdown files (default: docs/**/*.md)")
    parser.add_argument("--timeout", type=float, default=120.0, help="default per-block timeout in seconds")
    parser.add_argument("--filter", default=None, help="only run blocks whose location/label/heading matches")
    parser.add_argument("--list", action="store_true", help="list the runnable blocks and exit")
    parser.add_argument("--verbose", action="store_true", help="show the output of successful blocks too")
    parser.add_argument("--keep", action="store_true", help="keep the generated .wls files")
    parser.add_argument("--fail-fast", action="store_true", help="stop at the first failure")
    parser.add_argument("--jobs", type=int, default=1, help="reserved; blocks are currently run sequentially")
    args = parser.parse_args(argv)

    if args.paths:
        paths = [p.resolve() for p in args.paths]
    else:
        paths = sorted((REPO_ROOT / "docs").rglob("*.md"))
    paths = [p for p in paths if p.exists()]
    if not paths:
        print("no Markdown files found", file=sys.stderr)
        return 2

    failures = 0
    total = 0
    for path in paths:
        blocks = parse_markdown(path)
        if not blocks:
            continue
        for block in blocks:
            if block.skip:
                if args.verbose:
                    print(f"[skip] {block.location()} ({block.label})")
                continue
            if args.filter and args.filter not in f"{block.location()} {block.label} {block.heading}":
                continue
            total += 1
            if args.list:
                kind = "setup" if block.setup else "block"
                print(f"{block.location():<40} [{kind}] § {block.heading} :: {block.label}")
                continue
            print(f"[{total}] {block.location():<40} § {block.heading} ... ", end="", flush=True)
            result = run_block(
                block,
                collect_setup([b for b in blocks if b is not block and b.line < block.line]),
                args.timeout,
                args.keep,
                args.verbose,
            )
            status = "ok" if result.ok else "FAILED"
            print(f"{status} ({result.duration:.1f}s)" + (f"  {result.reason}" if result.reason else ""))
            if not result.ok:
                failures += 1
                if args.fail_fast:
                    break
        if args.fail_fast and failures:
            break

    if args.list:
        return 0
    print()
    if failures:
        print(f"{failures} of {total} block(s) failed.")
        return 1
    print(f"all {total} block(s) passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
