import json
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

MANUAL_URL = "https://libisl.sourceforge.io/user.html"
AUTOGEN_DIR = Path(__file__).resolve().parent
MANUAL_PATH = AUTOGEN_DIR / "ISL_MANUAL.html"
CATALOG_DIR = AUTOGEN_DIR / "gen"
CATALOG_PATH = CATALOG_DIR / "catalog.json"

class CodeCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self._depth = 0
        self._current: List[str] = []
        self.snippets: List[str] = []

    def handle_starttag(self, tag: str, attrs: List[Tuple[str, Optional[str]]]) -> None:
        if tag.lower() == "code":
            self._depth += 1
            if self._depth == 1:
                self._current = []

    def handle_endtag(self, tag: str) -> None:
        if tag.lower() == "code" and self._depth > 0:
            snippet = "".join(self._current).strip()
            if snippet:
                self.snippets.append(self._sanitize(snippet))
            self._current = []
            self._depth -= 1

    def handle_data(self, data: str) -> None:
        if self._depth > 0:
            self._current.append(data)

    def _sanitize(self, snippet: str) -> str:
        cleaned = []
        for line in snippet.splitlines():
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                cleaned.append(stripped)
        return " ".join(cleaned)

def fetch_manual() -> str:
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
            print(f"curl failed ({exc.returncode}); using cached manual.", file=sys.stderr)
            return MANUAL_PATH.read_text(encoding="utf-8")
        raise
    MANUAL_PATH.write_text(result.stdout, encoding="utf-8")
    return result.stdout

def parse_arg(arg_str: str) -> Dict[str, Optional[str]]:
    qualifier = None
    if "__isl_take" in arg_str:
        qualifier = "__isl_take"
        arg_str = arg_str.replace("__isl_take", "").strip()
    elif "__isl_keep" in arg_str:
        qualifier = "__isl_keep"
        arg_str = arg_str.replace("__isl_keep", "").strip()
    elif "__isl_give" in arg_str:
        qualifier = "__isl_give"
        arg_str = arg_str.replace("__isl_give", "").strip()
        
    parts = arg_str.rsplit(" ", 1)
    if len(parts) == 2:
        type_part, name_part = parts
        while name_part.startswith("*"):
            type_part += "*"
            name_part = name_part[1:].strip()
    else:
        # Fallback for weird args
        return {"name": "arg", "type": arg_str, "qualifier": qualifier}

    return {
        "name": name_part.replace(";", ""),
        "type": type_part,
        "qualifier": qualifier
    }

def parse_function(sig: str) -> Optional[Dict[str, Any]]:
    sig = sig.strip()
    if not sig.endswith(";"):
        return None
    sig = sig[:-1]
    
    # Regex for: [qualifier] [type] [name]([args])
    # Handles pointers and whitespace
    match = re.match(r"^(.*?)\b(isl_\w+)\s*\((.*)\)$", sig)
    if not match:
        return None
    
    ret_part, name, args_part = match.groups()
    
    if "=" in ret_part:
        return None
    
    ret_qual = None
    if "__isl_give" in ret_part:
        ret_qual = "__isl_give"
        ret_part = ret_part.replace("__isl_give", "").strip()
    elif "__isl_take" in ret_part:
        ret_qual = "__isl_take"
        ret_part = ret_part.replace("__isl_take", "").strip()
    elif "__isl_keep" in ret_part:
        ret_qual = "__isl_keep"
        ret_part = ret_part.replace("__isl_keep", "").strip()
    elif "__isl_null" in ret_part:
        ret_qual = "__isl_null"
        ret_part = ret_part.replace("__isl_null", "").strip()
        
    ret_type = ret_part.strip()
    
    args = []
    if args_part.strip() and args_part.strip() != "void":
        raw_args = args_part.split(",")
        for raw_arg in raw_args:
            args.append(parse_arg(raw_arg.strip()))
    
    return {
        "name": name,
        "return_type": ret_type,
        "return_qualifier": ret_qual,
        "arguments": args,
        "original_sig": sig
    }

def apply_patches(functions: List[Dict[str, Any]], types: Dict[str, Any]) -> None:
    existing_funcs = {f["name"] for f in functions}
    
    # 1. Add read_from_str for types if missing
    for slug, type_info in types.items():
        if slug == "ctx":
            continue
        func_name = f"isl_{slug}_read_from_str"
        if func_name not in existing_funcs:
            c_type_ptr = type_info["c_decl"]
            new_func = {
                "name": func_name,
                "owner_slug": slug,
                "method": "read_from_str",
                "return_type": c_type_ptr,
                "return_qualifier": "__isl_give",
                "arguments": [
                    {"qualifier": None, "type": "isl_ctx *", "name": "ctx"},
                    {"qualifier": None, "type": "const char *", "name": "str"}
                ]
            }
            functions.append(new_func)
            existing_funcs.add(func_name)

    # 2. Manual additions
    manual_additions: List[Dict[str, Any]] = [
        {
            "name": "isl_constraint_copy",
            "owner_slug": "constraint",
            "method": "copy",
            "return_type": "isl_constraint *",
            "return_qualifier": "__isl_give",
            "arguments": [{"type": "isl_constraint *", "qualifier": "__isl_keep", "name": "c"}]
        },
        {
            "name": "isl_local_space_to_str",
            "owner_slug": "local_space",
            "method": "to_str",
            "return_type": "char *",
            "return_qualifier": "__isl_give",
            "arguments": [{"type": "isl_local_space *", "qualifier": "__isl_keep", "name": "ls"}]
        },
        {
            "name": "isl_basic_set_intersect",
            "owner_slug": "basic_set",
            "method": "intersect",
            "return_type": "isl_basic_set *",
            "return_qualifier": "__isl_give",
            "arguments": [
                {"type": "isl_basic_set *", "qualifier": "__isl_take", "name": "bset1"},
                {"type": "isl_basic_set *", "qualifier": "__isl_take", "name": "bset2"}
            ]
        },
        {
            "name": "isl_basic_map_remove_unknown_divs",
            "owner_slug": "basic_map",
            "method": "remove_unknown_divs",
            "return_type": "isl_basic_map *",
            "return_qualifier": "__isl_give",
            "arguments": [{"type": "isl_basic_map *", "qualifier": "__isl_take", "name": "bmap"}]
        },
        {
            "name": "isl_ast_node_list_from_ast_node",
            "owner_slug": "ast_node_list",
            "method": "from_ast_node",
            "return_type": "isl_ast_node_list *",
            "return_qualifier": "__isl_give",
            "arguments": [{"type": "isl_ast_node *", "qualifier": "__isl_take", "name": "node"}]
        },
        {
            "name": "isl_basic_set_project_out",
            "owner_slug": "basic_set",
            "method": "project_out",
            "return_type": "isl_basic_set *",
            "return_qualifier": "__isl_give",
            "arguments": [
                {"type": "isl_basic_set *", "qualifier": "__isl_take", "name": "bset"},
                {"type": "enum isl_dim_type", "qualifier": None, "name": "type"},
                {"type": "unsigned", "qualifier": None, "name": "first"},
                {"type": "unsigned", "qualifier": None, "name": "n"}
            ]
        },
    ]
    
    for func in manual_additions:
        if func["name"] not in existing_funcs:
            functions.append(func)
            existing_funcs.add(func["name"])
            
            # Ensure type exists
            if "owner_slug" in func:
                slug = func["owner_slug"]
                if slug not in types:
                    class_name = "".join(x.capitalize() for x in slug.split("_"))
                    types[slug] = {
                        "slug": slug,
                        "c_decl": f"isl_{slug} *",
                        "class_name": class_name
                    }

def generate_catalog() -> None:
    html = fetch_manual()
    parser = CodeCollector()
    parser.feed(html)
    
    functions = []
    for snippet in parser.snippets:
        # Insert missing semicolons between function definitions
        # Heuristic: ) followed by __isl_ or isl_ (return type start)
        snippet = re.sub(r"\)\s+((?:__isl_|isl_))", r"); \1", snippet)
        
        for stmt in snippet.split(";"):
            stmt = stmt.strip()
            if not stmt or "isl_" not in stmt:
                continue
            func = parse_function(stmt + ";")
            if func:
                functions.append(func)

    unique_funcs = {f["name"]: f for f in functions}
    functions = list(unique_funcs.values())
    
    types: Dict[str, Any] = {}
    
    for func in functions:
        name = func["name"]
        candidates = []
        
        for t_str in [func["return_type"]] + [a["type"] for a in func["arguments"]]:
            t_str = t_str.strip()
            if t_str.startswith("isl_") and t_str.endswith("*"):
                candidates.append(t_str[4:-1].strip())
        
        candidates.sort(key=len, reverse=True)
        
        for c in candidates:
            prefix = f"isl_{c}_"
            if name.startswith(prefix):
                func["owner_slug"] = c
                func["method"] = name[len(prefix):]
                
                if c not in types:
                    class_name = "".join(x.capitalize() for x in c.split("_"))
                    types[c] = {
                        "slug": c,
                        "c_decl": f"isl_{c} *",
                        "class_name": class_name
                    }
                break
        
        if "owner_slug" not in func:
            if name.startswith("isl_ctx_"):
                func["owner_slug"] = "ctx"
                func["method"] = name[8:]
                if "ctx" not in types:
                    types["ctx"] = {"slug": "ctx", "c_decl": "isl_ctx *", "class_name": "Context"}

    if "ctx" not in types:
        types["ctx"] = {"slug": "ctx", "c_decl": "isl_ctx *", "class_name": "Context"}
    else:
        types["ctx"]["class_name"] = "Context"

    apply_patches(functions, types)

    catalog = {
        "types": types,
        "functions": functions
    }
    
    CATALOG_DIR.mkdir(parents=True, exist_ok=True)
    with open(CATALOG_PATH, "w") as f:
        json.dump(catalog, f, indent=2)
    print(f"Generated catalog at {CATALOG_PATH} with {len(functions)} functions and {len(types)} types.")

if __name__ == "__main__":
    generate_catalog()
