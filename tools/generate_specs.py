import json
import re
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, List

# Mapping from C types to Python primitive types and ctypes
PRIMITIVE_TYPES = {
    "int": ("int", "c_int"),
    "long": ("int", "c_long"),
    "unsigned long": ("int", "c_ulong"),
    "size_t": ("int", "c_size_t"),
    "double": ("float", "c_double"),
    "char *": ("str", "c_char_p"),
    "const char *": ("str", "c_char_p"),
    "void *": ("Any", "c_void_p"),
    "const void *": ("Any", "c_void_p"),
    "isl_bool": ("bool", "c_int"),
    "isl_size": ("int", "c_int"),
    "isl_stat": ("int", "c_int"),
    "unsigned": ("int", "c_uint"),
    "unsigned int": ("int", "c_uint"),
    "long long": ("int", "c_longlong"),
    "unsigned long long": ("int", "c_ulonglong"),
    "FILE *": ("None", "c_void_p"),
}

# Special types that are handled via Context or implicit mechanisms
IGNORED_TYPES = {
    "isl_ctx *",
}

CLASS_RENAMES = {
    "AstExpr": "ASTExpr",
    "AstNode": "ASTNode",
    "AstBuild": "ASTBuild",
    "AstPrintOptions": "ASTPrintOptions",
    "AstExprList": "AstExprList",
    "AstNodeList": "AstNodeList",
    "IdToAstExpr": "IdToAstExpr",
}

METHOD_RENAMES = {
    "alloc_equality": "equality",
    "alloc_inequality": "inequality",
    "alloc_user": "from_expr",
    "get_user": "user",
    "plain_is_equal": "is_equal",
    "from_ast_node": "from_node",
    "get_name": "name",
    # "read_from_str": "from_str", # Handled explicitly
}

class Generator:
    def __init__(self, catalog_path: str, output_dir: str):
        self.catalog_path = Path(catalog_path)
        self.output_dir = Path(output_dir)
        self.catalog = self._load_catalog()
        self.types = self.catalog["types"]
        self._apply_renames()
        self.c_type_map = {}
        self._build_type_map()
        self.functions = self.catalog["functions"]
        self.functions_by_owner = defaultdict(list)
        self._organize_functions()

    def _load_catalog(self) -> Dict[str, Any]:
        with open(self.catalog_path, "r") as f:
            return json.load(f)

    def _apply_renames(self):
        for slug, type_info in self.types.items():
            original_name = type_info["class_name"]
            if original_name in CLASS_RENAMES:
                type_info["class_name"] = CLASS_RENAMES[original_name]

    def _build_type_map(self):
        for slug, info in self.types.items():
            c_decl = info["c_decl"]
            clean_c = c_decl.replace("*", "").strip()
            self.c_type_map[clean_c] = slug
            # Also map the pointer version just in case, though clean_type removes it
            self.c_type_map[c_decl] = slug
            # Handle struct prefix
            if clean_c.startswith("struct "):
                clean_struct = clean_c.replace("struct ", "").strip()
                self.c_type_map[clean_struct] = slug

    def _organize_functions(self):
        for func in self.functions:
            owner = func.get("owner_slug")
            if owner:
                self.functions_by_owner[owner].append(func)

    def generate(self):
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate specs for each type
        generated_files = []
        for slug, type_info in self.types.items():
            class_name = type_info["class_name"]
            # Skip ctx as it is handled specially in context.py
            if slug == "ctx":
                continue
                
            content = self._generate_class_file(slug, class_name)
            filename = f"{slug}.py"
            with open(self.output_dir / filename, "w") as f:
                f.write(content)
            generated_files.append(slug)
            
        self._generate_init(generated_files)

    def _generate_class_file(self, slug: str, class_name: str) -> str:
        functions = self.functions_by_owner[slug]
        
        # Imports
        lines = [
            "from __future__ import annotations",
            "",
            "from ctypes import c_int, c_long, c_ulong, c_size_t, c_double, c_char_p, c_void_p, c_uint, c_longlong, c_ulonglong",
            "from typing import Any, TYPE_CHECKING",
            "",
            "from ..ffi import load_libisl",
            "from ..func import ISLFunction",
            "from ..obj import ISLObject",
            "from ..mixin import ISLObjectMixin",
            "from ..registry import register_type",
            "from ..qualifier import Give, Keep, Null, Param, Take",
            "from .context import Context",
            "",
            "if TYPE_CHECKING:",
            "    from .context import Context",
            "",
            "_lib = load_libisl()",
            "",
        ]

        # Class definition
        lines.append(f"class {class_name}(ISLObject, ISLObjectMixin):")
        lines.append("    __slots__ = ()")
        lines.append("")

        # Identify key methods
        methods = {}
        for f in functions:
            methods[f["method"]] = f

        # __init__
        read_from_str = methods.get("read_from_str")
        if read_from_str:
            lines.append("    def __init__(self, handle_or_spec: Any) -> None:")
            lines.append("        if isinstance(handle_or_spec, str):")
            lines.append(f"            handle = _{read_from_str['name']}(handle_or_spec, return_raw_pointer=True)")
            lines.append("            super().__init__(handle)")
            lines.append("        else:")
            lines.append("            super().__init__(handle_or_spec)")
            lines.append("")
            
            # Add from_str alias
            lines.append("    @classmethod")
            lines.append("    def from_str(cls, spec: str) -> Any:")
            lines.append(f"        return _{read_from_str['name']}(spec)")
        else:
            lines.append("    def __init__(self, handle: Any = None) -> None:")
            lines.append("        super().__init__(handle)")

        lines.append("")
        
        # copy_handle
        copy_func = methods.get("copy")
        if copy_func:
            lines.append("    def copy_handle(self) -> Any:")
            lines.append(f"        return _{copy_func['name']}(self, return_raw_pointer=True)")
        else:
             pass 

        lines.append("")

        # free_handle
        free_func = methods.get("free")
        if free_func:
            lines.append("    @classmethod")
            lines.append("    def free_handle(cls, handle: Any) -> None:")
            lines.append(f"        _lib.{free_func['name']}(handle)")
        
        lines.append("")

        # Standard methods
        to_str = methods.get("to_str")
        if to_str:
            lines.append("    def __str__(self) -> str:")
            lines.append(f"        return _{to_str['name']}(self)")
            lines.append("")
            lines.append("    def __repr__(self) -> str:")
            lines.append(f"        return f'{class_name}({{self.__str__()}})'")
            lines.append("")

        # Generate all methods
        for func in functions:
            method_name = func["method"]
            if method_name in ("free", "copy", "read_from_str", "to_str"):
                continue
            
            self._generate_method(lines, func, slug)

        # Special classes
        if class_name == "ASTNode":
             lines.append("")
             lines.append("class ASTUserNode(ASTNode):")
             lines.append("    pass")
             lines.append('register_type("ASTUserNode", ASTUserNode)')

        lines.append("")
        lines.append(f'register_type("{class_name}", {class_name})')
        lines.append("")

        # Generate ISLFunction definitions
        for func in functions:
            self._generate_isl_function(lines, func)

        return "\n".join(lines)

    def _generate_method(self, lines: List[str], func: Dict[str, Any], owner_slug: str):
        name = func["method"]
        
        # Sanitize method name
        if name == "2exp":
            name = "exp2"
        elif name[0].isdigit():
            name = f"_{name}"
        
        if name in METHOD_RENAMES:
            name = METHOD_RENAMES[name]
            
        KEYWORDS = {"and", "or", "not", "from", "in", "is", "del", "global", "lambda", "yield", "pass", "class", "def", "if", "else", "elif", "while", "for", "try", "except", "finally", "raise", "import", "return", "break", "continue", "with", "as", "assert", "async", "await"}
        
        if name in KEYWORDS:
            name = f"{name}_"
            
        c_name = func["name"]
        args = func["arguments"]
        
        # Validate types - skip if any argument or return type is unknown
        for arg in args:
            c_type = arg["type"].strip()
            if c_type == "isl_ctx *":
                continue
            if c_type in PRIMITIVE_TYPES:
                continue
            if c_type.startswith("enum "):
                continue
            if "(*" in c_type: # Function pointer
                continue
            clean_type = c_type.replace("*", "").strip()
            if clean_type in self.c_type_map:
                continue
            # Unknown type
            print(f"Skipping {c_name} due to unknown arg type: '{c_type}'")
            return

        ret_type = func["return_type"].strip()
        if ret_type != "void" and ret_type not in PRIMITIVE_TYPES and not ret_type.startswith("enum "):
             clean_ret = ret_type.replace("*", "").strip()
             if clean_ret not in self.c_type_map:
                 # Unknown return type
                 print(f"Skipping {c_name} due to unknown return type: '{ret_type}'")
                 return

        return_info = func
        
        py_args = []
        call_args = []
        
        seen_args = set()
        
        # Prepare arguments
        # Skip the first argument if it's 'self' (the owner object)
        start_idx = 0
        
        # Heuristic: if the first argument type matches the class C type, it's an instance method.
        # Otherwise it's a static method (classmethod).
        is_instance = False
        class_c_type = self.types[owner_slug]["c_decl"]
        
        if args and args[0]["type"] == class_c_type:
            is_instance = True
            py_args.append("self")
            call_args.append("self")
            seen_args.add("self")
            start_idx = 1
        else:
            py_args.append("cls")
            
        for arg in args[start_idx:]:
            arg_name = arg["name"]
            
            # Try to extract name from function pointer type
            if "(*" in arg["type"]:
                match = re.search(r"\(\*([a-zA-Z0-9_]+)\)", arg["type"])
                if match:
                    arg_name = match.group(1)
                else:
                    arg_name = "fn"

            arg_name = arg_name.replace(")", "")
            
            if arg_name in KEYWORDS:
                arg_name = f"{arg_name}_"
            
            # Deduplicate
            while arg_name in seen_args:
                arg_name = f"{arg_name}_"
            seen_args.add(arg_name)
            
            # Determine type hint
            arg_type = self._map_type_hint(arg["type"].strip())
            
            # Handle ctx specially? No, usually hidden.
            if arg["type"] == "isl_ctx *":
                # context is implicit usually. 
                # But if it's not the first arg? ISL functions usually have ctx as first if not method.
                # If it is present in python signature, user provides it? 
                # In `set.py`, ctx is implicit.
                continue
                
            if arg_type == "Any" and arg_name in ("user", "user_"):
                py_args.append(f"{arg_name}: {arg_type} = None")
            else:
                py_args.append(f"{arg_name}: {arg_type}")
            call_args.append(arg_name)

        # Return type hint
        ret_type = self._map_type_hint(return_info["return_type"].strip())
        
        decorator = "    @classmethod" if not is_instance else ""
        if decorator:
            lines.append(decorator)
            
        lines.append(f"    def {name}({', '.join(py_args)}) -> {ret_type}:")
        lines.append(f"        return _{c_name}({', '.join(call_args)})")
        lines.append("")

    def _map_type_hint(self, c_type: str) -> str:
        c_type = c_type.strip()
        if c_type in PRIMITIVE_TYPES:
            return PRIMITIVE_TYPES[c_type][0]
        
        if c_type.startswith("enum "):
            return "int"
        
        if "(*" in c_type:
            return "Any"
        
        # Check if it's an ISL object type
        clean_type = c_type.replace("*", "").strip()
        if clean_type in self.c_type_map:
             # Return string forward reference
             slug = self.c_type_map[clean_type]
             return f"\"{self.types[slug]['class_name']}\""
             
        # Fallback
        return "Any"

    def _generate_isl_function(self, lines: List[str], func: Dict[str, Any]):
        c_name = func["name"]
        
        qualifiers = []
        for arg in func["arguments"]:
            c_type = arg["type"].strip()
            if c_type == "isl_ctx *":
                qualifiers.append("Context()")
                continue
                
            # Determine qualifier class
            qual_name = "Param"
            target = "None"
            extra = ""
            
            if c_type in PRIMITIVE_TYPES:
                qual_name = "Param"
                py_prim, c_prim = PRIMITIVE_TYPES[c_type]
                target = py_prim
                extra = f", ctype={c_prim}"
                # Special case for void*
                if c_type in ("void *", "const void *", "FILE *"):
                    target = "None"
            elif c_type.startswith("enum "):
                qual_name = "Param"
                target = "int"
                extra = ", ctype=c_int"
            elif "(*" in c_type:
                qual_name = "Param"
                target = "None"
                extra = ", ctype=c_void_p"
            else:
                # Object type
                # Map qualifier string from JSON to Python class
                q_str = arg["qualifier"]
                if q_str == "__isl_take": qual_name = "Take"
                elif q_str == "__isl_keep": qual_name = "Keep"
                elif q_str == "__isl_give": qual_name = "Give"
                else: qual_name = "Keep" # Default to Keep for objects if unspecified
                
                clean_type = c_type.replace("*", "").strip()
                if clean_type in self.c_type_map:
                    slug = self.c_type_map[clean_type]
                    target = f"\"{self.types[slug]['class_name']}\""
                else:
                    # Unknown type - skip this function
                    print(f"Skipping {c_name} due to unknown arg type: '{c_type}'")
                    return

            qualifiers.append(f"{qual_name}({target}{extra})")

        # Return spec
        ret_type = func["return_type"].strip()
        ret_qual_name = "Param"
        ret_target = "None"
        ret_extra = ""
        
        if ret_type == "void":
            ret_qual_name = "Null"
            ret_target = ""
        elif ret_type in PRIMITIVE_TYPES:
            ret_qual_name = "Param"
            py_prim, c_prim = PRIMITIVE_TYPES[ret_type]
            ret_target = py_prim
            ret_extra = f", ctype={c_prim}"
        elif ret_type.startswith("enum "):
            ret_qual_name = "Param"
            ret_target = "int"
            ret_extra = ", ctype=c_int"
        else:
            q_str = func["return_qualifier"]
            if q_str == "__isl_give": ret_qual_name = "Give"
            else: ret_qual_name = "Give" # Default for objects returned?
            
            clean_type = ret_type.replace("*", "").strip()
            if clean_type in self.c_type_map:
                slug = self.c_type_map[clean_type]
                ret_target = f"\"{self.types[slug]['class_name']}\""
            else:
                 # Unknown return type - skip
                 print(f"Skipping {c_name} due to unknown return type: '{ret_type}'")
                 return

        if ret_qual_name == "Null":
            ret_spec = "Null()"
        else:
            ret_spec = f"{ret_qual_name}({ret_target}{ret_extra})"

        lines.append(f"_{c_name} = ISLFunction.create(")
        lines.append(f'    "{c_name}",')
        for q in qualifiers:
            lines.append(f"    {q},")
        lines.append(f"    return_={ret_spec},")
        lines.append("    lib=_lib,")
        lines.append(")")
        lines.append("")

    def _generate_init(self, generated_files: List[str]):
        lines = []
        for slug in generated_files:
            if slug in self.types:
                class_name = self.types[slug]["class_name"]
                lines.append(f"from .{slug} import {class_name}")
        
        if "ast_node" in generated_files:
            lines.append("from .ast_node import ASTUserNode")

        # I should check if I need to include `Context` in __init__.
        lines.append("from .context import Context, context, current, ISLContextError")
        lines.append("from .local_types import ISLDimType")
        
        lines.append("")
        lines.append("__all__ = [")
        lines.append('    "Context",')
        lines.append('    "context",')
        lines.append('    "current",')
        lines.append('    "ISLContextError",')
        lines.append('    "ISLDimType",')
        for slug in generated_files:
            if slug in self.types:
                class_name = self.types[slug]["class_name"]
                lines.append(f'    "{class_name}",')
        lines.append('    "ASTUserNode",') # Add ASTUserNode to __all__
        lines.append("]")
        
        with open(self.output_dir / "__init__.py", "w") as f:
            f.write("\n".join(lines))

if __name__ == "__main__":
    g = Generator(
        "caten/isl/gen/catalog.json",
        "caten/isl/specs"
    )
    g.generate()