from __future__ import annotations

import re
import subprocess
import sys
from collections import OrderedDict
from html.parser import HTMLParser
from pathlib import Path
from typing import Iterable, List, Tuple

MANUAL_URL = "https://libisl.sourceforge.io/user.html"
DOCS_DIR = Path(__file__).resolve().parent
MANUAL_PATH = DOCS_DIR / "ISL_MANUAL.html"
APIS_PATH = DOCS_DIR / "ISL_APIS.md"


class _CodeCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self._depth = 0
        self._current: List[str] = []
        self.snippets: List[str] = []

    def handle_starttag(self, tag: str, attrs: List[Tuple[str, str | None]]) -> None:
        if tag.lower() == "code":
            self._depth += 1
            if self._depth == 1:
                self._current = []

    def handle_endtag(self, tag: str) -> None:
        if tag.lower() == "code" and self._depth > 0:
            snippet = "".join(self._current).strip()
            if snippet:
                self.snippets.append(_sanitize_lines(snippet))
            self._current = []
            self._depth -= 1

    def handle_data(self, data: str) -> None:
        if self._depth > 0:
            self._current.append(data)


def _sanitize_lines(snippet: str) -> str:
    cleaned: List[str] = []
    for line in snippet.splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            continue
        cleaned.append(line)
    return "\n".join(cleaned)


def _extract_functions(snippet: str) -> Iterable[Tuple[str, str]]:
    statements = [part.strip() for part in snippet.replace("\r", "").split(";")]
    for statement in statements:
        if not statement:
            continue
        if "isl_" not in statement:
            continue
        match = re.search(r"(isl_[A-Za-z0-9_]+)\s*\(", statement)
        if not match:
            continue
        prefix = statement[: match.start(1)].strip()
        if not prefix:
            continue
        name = match.group(1)
        signature_lines = [line.rstrip() for line in statement.splitlines() if line.strip()]
        signature = "\n".join(signature_lines).strip() + ";"
        yield name, signature


def _fetch_manual() -> str:
    print(f"Fetching manual from {MANUAL_URL}...")
    try:
        result = subprocess.run(
            ["curl", "-fsSL", MANUAL_URL],
            check=True,
            capture_output=True,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        if MANUAL_PATH.exists():
            print(
                "curl failed (",
                exc.returncode,
                "); falling back to existing ISL_MANUAL.html",
                file=sys.stderr,
            )
            return MANUAL_PATH.read_text(encoding="utf-8")
        raise
    MANUAL_PATH.write_text(result.stdout, encoding="utf-8")
    return result.stdout


def _parse_code_blocks(html: str) -> List[str]:
    parser = _CodeCollector()
    parser.feed(html)
    return parser.snippets


def _build_function_map(snippets: Iterable[str]) -> "OrderedDict[str, str]":
    functions: "OrderedDict[str, str]" = OrderedDict()
    for snippet in snippets:
        for name, signature in _extract_functions(snippet):
            functions.setdefault(name, signature)
    if not functions:
        raise RuntimeError("No functions extracted from manual HTML.")
    return functions


def _write_api_doc(functions: "OrderedDict[str, str]") -> None:
    lines: List[str] = ["# ISL API Checklist", ""]
    for name, signature in functions.items():
        lines.append(f"### {name}")
        lines.append("")
        lines.append("```")
        lines.append(signature)
        lines.append("```")
        lines.append("")
        lines.append("**Checklist**")
        lines.append("- [ ] Implemented")
        lines.append("- [ ] Reachable as a method in class (if so, implemented at `...`)")
        lines.append("- [ ] Included in Mixin (if so, implemented at `...`)")
        lines.append("")
    APIS_PATH.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    html = _fetch_manual()
    snippets = _parse_code_blocks(html)
    functions = _build_function_map(snippets)
    _write_api_doc(functions)
    print(f"Extracted {len(functions)} functions to {APIS_PATH}.")


if __name__ == "__main__":
    main()
