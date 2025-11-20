import json
from pathlib import Path

CATALOG_PATH = Path("caten/isl/gen/catalog.json")

def fix_catalog():
    with open(CATALOG_PATH, "r") as f:
        catalog = json.load(f)

    functions = catalog["functions"]
    existing_funcs = {f["name"] for f in functions}
    
    types = catalog["types"]
    
    added_count = 0
    
    for slug, type_info in types.items():
        func_name = f"isl_{slug}_read_from_str"
        if func_name not in existing_funcs:
            # We assume it exists in libisl. If not, it will fail at runtime (symbol not found), 
            # but better than missing init support.
            
            c_type_ptr = type_info["c_decl"] # e.g. isl_set *
            
            new_func = {
                "name": func_name,
                "owner_slug": slug,
                "method": "read_from_str",
                "return_type": c_type_ptr,
                "return_normalized": c_type_ptr,
                "return_qualifier": "__isl_give",
                "return_target": slug,
                "arguments": [
                    {
                        "qualifier": None,
                        "type": "isl_ctx *",
                        "normalized_type": "isl_ctx *",
                        "name": "ctx",
                        "target_slug": "ctx"
                    },
                    {
                        "qualifier": None,
                        "type": "const char *",
                        "normalized_type": "char *",
                        "name": "str",
                        "target_slug": None
                    }
                ]
            }
            functions.append(new_func)
            existing_funcs.add(func_name)
            added_count += 1
            print(f"Added {func_name}")

    # Add specific missing functions
    manual_additions = [
        {
            "name": "isl_constraint_copy",
            "owner_slug": "constraint",
            "method": "copy",
            "return_type": "isl_constraint *",
            "return_qualifier": "__isl_give",
            "arguments": [
                {"type": "isl_constraint *", "qualifier": "__isl_keep", "name": "c"}
            ]
        },
        {
            "name": "isl_local_space_to_str",
            "owner_slug": "local_space",
            "method": "to_str",
            "return_type": "char *",
            "return_qualifier": "__isl_give",
            "arguments": [
                {"type": "isl_local_space *", "qualifier": "__isl_keep", "name": "ls"}
            ]
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
            "arguments": [
                {"type": "isl_basic_map *", "qualifier": "__isl_take", "name": "bmap"}
            ]
        },
        {
            "name": "isl_ast_node_list_from_ast_node",
            "owner_slug": "ast_node_list",
            "method": "from_ast_node",
            "return_type": "isl_ast_node_list *",
            "return_qualifier": "__isl_give",
            "arguments": [
                {"type": "isl_ast_node *", "qualifier": "__isl_take", "name": "node"}
            ]
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
            print(f"Adding {func['name']}")
            functions.append(func)
            existing_funcs.add(func["name"])
            added_count += 1

    print(f"Total added: {added_count}")

    # Save the updated catalog
    with open(CATALOG_PATH, "w") as f:
        json.dump(catalog, f, indent=2)

if __name__ == "__main__":
    fix_catalog()