# ISL API Checklist

### isl_ctx_alloc

```
isl_ctx *isl_ctx_alloc();
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.copy_handle`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_free

```
void isl_ctx_free(isl_ctx *ctx);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.free_handle`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_set_max_operations

```
void isl_ctx_set_max_operations(isl_ctx *ctx,
        unsigned long max_operations);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.__init__`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_get_max_operations

```
unsigned long isl_ctx_get_max_operations(isl_ctx *ctx);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.is_empty`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_reset_operations

```
void isl_ctx_reset_operations(isl_ctx *ctx);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.is_equal`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_ctx

```
isl_ctx *isl_val_get_ctx(__isl_keep isl_val *val);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.union`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_ctx

```
isl_ctx *isl_multi_val_get_ctx(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.intersect`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_get_ctx

```
isl_ctx *isl_id_get_ctx(__isl_keep isl_id *id);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.subtract`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_get_ctx

```
isl_ctx *isl_multi_id_get_ctx(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [x] Implemented
- [x] Reachable as a method in class (if so, implemented at `caten/isl/specs/set.py:Set.__str__`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_get_ctx

```
isl_ctx *isl_local_space_get_ctx(
        __isl_keep isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_get_ctx

```
isl_ctx *isl_set_list_get_ctx(
        __isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_ctx

```
isl_ctx *isl_aff_get_ctx(__isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_ctx

```
isl_ctx *isl_multi_aff_get_ctx(
        __isl_keep isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_ctx

```
isl_ctx *isl_pw_aff_get_ctx(__isl_keep isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_ctx

```
isl_ctx *isl_pw_multi_aff_get_ctx(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_ctx

```
isl_ctx *isl_multi_pw_aff_get_ctx(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_get_ctx

```
isl_ctx *isl_union_pw_aff_get_ctx(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_get_ctx

```
isl_ctx *isl_union_pw_multi_aff_get_ctx(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_ctx

```
isl_ctx *isl_multi_union_pw_aff_get_ctx(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_get_ctx

```
isl_ctx *isl_id_to_ast_expr_get_ctx(
        __isl_keep isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_get_ctx

```
isl_ctx *isl_point_get_ctx(__isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_get_ctx

```
isl_ctx *isl_vec_get_ctx(__isl_keep isl_vec *vec);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_get_ctx

```
isl_ctx *isl_mat_get_ctx(__isl_keep isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertices_get_ctx

```
isl_ctx *isl_vertices_get_ctx(
        __isl_keep isl_vertices *vertices);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertex_get_ctx

```
isl_ctx *isl_vertex_get_ctx(__isl_keep isl_vertex *vertex);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_cell_get_ctx

```
isl_ctx *isl_cell_get_ctx(__isl_keep isl_cell *cell);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_get_ctx

```
isl_ctx *isl_restriction_get_ctx(
        __isl_keep isl_restriction *restr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_get_ctx

```
isl_ctx *isl_union_access_info_get_ctx(
        __isl_keep isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_ctx

```
isl_ctx *isl_union_flow_get_ctx(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_get_ctx

```
isl_ctx *isl_schedule_get_ctx(
        __isl_keep isl_schedule *sched);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_ctx

```
isl_ctx *isl_schedule_constraints_get_ctx(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_ctx

```
isl_ctx *isl_schedule_node_get_ctx(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_get_ctx

```
isl_ctx *isl_ast_build_get_ctx(
        __isl_keep isl_ast_build *build);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_ctx

```
isl_ctx *isl_ast_expr_get_ctx(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_get_ctx

```
isl_ctx *isl_ast_node_get_ctx(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stride_info_get_ctx

```
isl_ctx *isl_stride_info_get_ctx(
        __isl_keep isl_stride_info *si);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_get_ctx

```
isl_ctx *isl_fixed_box_get_ctx(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_bool_not

```
isl_bool isl_bool_not(isl_bool b);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_bool_ok

```
isl_bool isl_bool_ok(int b);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stat_non_null

```
isl_stat isl_stat_non_null(void *obj);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stat_non_error_bool

```
isl_stat isl_stat_non_error_bool(isl_bool b);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_zero

```
__isl_give isl_val *isl_val_zero(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_one

```
__isl_give isl_val *isl_val_one(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_negone

```
__isl_give isl_val *isl_val_negone(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_nan

```
__isl_give isl_val *isl_val_nan(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_infty

```
__isl_give isl_val *isl_val_infty(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_neginfty

```
__isl_give isl_val *isl_val_neginfty(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_int_from_si

```
__isl_give isl_val *isl_val_int_from_si(isl_ctx *ctx,
        long i);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_int_from_ui

```
__isl_give isl_val *isl_val_int_from_ui(isl_ctx *ctx,
        unsigned long u);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_int_from_chunks

```
__isl_give isl_val *isl_val_int_from_chunks(isl_ctx *ctx,
        size_t n, size_t size, const void *chunks);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_copy

```
__isl_give isl_val *isl_val_copy(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_free

```
__isl_null isl_val *isl_val_free(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_num_si

```
long isl_val_get_num_si(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_den_si

```
long isl_val_get_den_si(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_den_val

```
__isl_give isl_val *isl_val_get_den_val(
        __isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_d

```
double isl_val_get_d(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_n_abs_num_chunks

```
isl_size isl_val_n_abs_num_chunks(__isl_keep isl_val *v,
        size_t size);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_abs_num_chunks

```
isl_stat isl_val_get_abs_num_chunks(__isl_keep isl_val *v,
        size_t size, void *chunks);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_set_si

```
__isl_give isl_val *isl_val_set_si(__isl_take isl_val *v,
        long i);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_sgn

```
int isl_val_sgn(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_zero

```
isl_bool isl_val_is_zero(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_one

```
isl_bool isl_val_is_one(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_negone

```
isl_bool isl_val_is_negone(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_nonneg

```
isl_bool isl_val_is_nonneg(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_nonpos

```
isl_bool isl_val_is_nonpos(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_pos

```
isl_bool isl_val_is_pos(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_neg

```
isl_bool isl_val_is_neg(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_int

```
isl_bool isl_val_is_int(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_rat

```
isl_bool isl_val_is_rat(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_nan

```
isl_bool isl_val_is_nan(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_infty

```
isl_bool isl_val_is_infty(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_neginfty

```
isl_bool isl_val_is_neginfty(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_lt

```
isl_bool isl_val_lt(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_le

```
isl_bool isl_val_le(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_gt

```
isl_bool isl_val_gt(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_ge

```
isl_bool isl_val_ge(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_eq

```
isl_bool isl_val_eq(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_ne

```
isl_bool isl_val_ne(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_abs_eq

```
isl_bool isl_val_abs_eq(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_is_divisible_by

```
isl_bool isl_val_is_divisible_by(__isl_keep isl_val *v1,
        __isl_keep isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_gt_si

```
isl_bool isl_val_gt_si(__isl_keep isl_val *v, long i);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_eq_si

```
isl_bool isl_val_eq_si(__isl_keep isl_val *v, long i);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_cmp_si

```
int isl_val_cmp_si(__isl_keep isl_val *v, long i);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_abs

```
__isl_give isl_val *isl_val_abs(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_neg

```
__isl_give isl_val *isl_val_neg(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_floor

```
__isl_give isl_val *isl_val_floor(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_ceil

```
__isl_give isl_val *isl_val_ceil(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_trunc

```
__isl_give isl_val *isl_val_trunc(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_inv

```
__isl_give isl_val *isl_val_inv(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_min

```
__isl_give isl_val *isl_val_min(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_max

```
__isl_give isl_val *isl_val_max(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_add

```
__isl_give isl_val *isl_val_add(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_add_ui

```
__isl_give isl_val *isl_val_add_ui(__isl_take isl_val *v1,
        unsigned long v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_sub

```
__isl_give isl_val *isl_val_sub(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_sub_ui

```
__isl_give isl_val *isl_val_sub_ui(__isl_take isl_val *v1,
        unsigned long v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_mul

```
__isl_give isl_val *isl_val_mul(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_mul_ui

```
__isl_give isl_val *isl_val_mul_ui(__isl_take isl_val *v1,
        unsigned long v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_div

```
__isl_give isl_val *isl_val_div(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_div_ui

```
__isl_give isl_val *isl_val_div_ui(__isl_take isl_val *v1,
        unsigned long v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_pow2

```
__isl_give isl_val *isl_val_pow2(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_2exp

```
__isl_give isl_val *isl_val_2exp(__isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_mod

```
__isl_give isl_val *isl_val_mod(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_gcd

```
__isl_give isl_val *isl_val_gcd(__isl_take isl_val *v1,
        __isl_take isl_val *v2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_gcdext

```
__isl_give isl_val *isl_val_gcdext(__isl_take isl_val *v1,
        __isl_take isl_val *v2, __isl_give isl_val **x,
        __isl_give isl_val **y);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_int_from_gmp

```
__isl_give isl_val *isl_val_int_from_gmp(isl_ctx *ctx,
        mpz_t z);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_from_gmp

```
__isl_give isl_val *isl_val_from_gmp(isl_ctx *ctx,
        const mpz_t n, const mpz_t d);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_num_gmp

```
int isl_val_get_num_gmp(__isl_keep isl_val *v, mpz_t z);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_get_den_gmp

```
int isl_val_get_den_gmp(__isl_keep isl_val *v, mpz_t z);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_last_error

```
enum isl_error isl_ctx_last_error(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_last_error_msg

```
const char *isl_ctx_last_error_msg(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_last_error_file

```
const char *isl_ctx_last_error_file(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_last_error_line

```
int isl_ctx_last_error_line(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ctx_reset_error

```
void isl_ctx_reset_error(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_on_error

```
isl_stat isl_options_set_on_error(isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_on_error

```
int isl_options_get_on_error(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_alloc

```
__isl_give isl_id *isl_id_alloc(isl_ctx *ctx,
        __isl_keep const char *name, void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_set_free_user

```
__isl_give isl_id *isl_id_set_free_user(
        __isl_take isl_id *id,
        void (*free_user)(void *user));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_get_free_user

```
void (*isl_id_get_free_user(__isl_keep isl_id *id))
        (void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_copy

```
__isl_give isl_id *isl_id_copy(isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_free

```
__isl_null isl_id *isl_id_free(__isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_get_user

```
void *isl_id_get_user(__isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_get_name

```
__isl_keep const char *isl_id_get_name(__isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_id

```
__isl_give isl_printer *isl_printer_print_id(
        __isl_take isl_printer *p, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_unit

```
__isl_give isl_space *isl_space_unit(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_alloc

```
__isl_give isl_space *isl_space_alloc(isl_ctx *ctx,
        unsigned nparam, unsigned n_in, unsigned n_out);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_params_alloc

```
__isl_give isl_space *isl_space_params_alloc(isl_ctx *ctx,
        unsigned nparam);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_alloc

```
__isl_give isl_space *isl_space_set_alloc(isl_ctx *ctx,
        unsigned nparam, unsigned dim);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_copy

```
__isl_give isl_space *isl_space_copy(__isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_free

```
__isl_null isl_space *isl_space_free(__isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_params

```
isl_bool isl_space_is_params(__isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_set

```
isl_bool isl_space_is_set(__isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_map

```
isl_bool isl_space_is_map(__isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_equal

```
isl_bool isl_space_is_equal(__isl_keep isl_space *space1,
        __isl_keep isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_equal_params

```
isl_bool isl_space_has_equal_params(
        __isl_keep isl_space *space1,
        __isl_keep isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_equal_tuples

```
isl_bool isl_space_has_equal_tuples(
        __isl_keep isl_space *space1,
        __isl_keep isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_domain

```
isl_bool isl_space_is_domain(__isl_keep isl_space *space1,
        __isl_keep isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_range

```
isl_bool isl_space_is_range(__isl_keep isl_space *space1,
        __isl_keep isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_tuple_is_equal

```
isl_bool isl_space_tuple_is_equal(
        __isl_keep isl_space *space1,
        enum isl_dim_type type1,
        __isl_keep isl_space *space2,
        enum isl_dim_type type2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_space

```
__isl_give isl_space *isl_basic_set_get_space(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_space

```
__isl_give isl_space *isl_set_get_space(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_get_space

```
__isl_give isl_space *isl_union_set_get_space(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_get_space

```
__isl_give isl_space *isl_basic_map_get_space(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_space

```
__isl_give isl_space *isl_map_get_space(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_get_space

```
__isl_give isl_space *isl_union_map_get_space(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_space

```
__isl_give isl_space *isl_constraint_get_space(
        __isl_keep isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_get_domain_space

```
__isl_give isl_space *isl_qpolynomial_get_domain_space(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_get_space

```
__isl_give isl_space *isl_qpolynomial_get_space(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_get_domain_space

```
__isl_give isl_space *
isl_qpolynomial_fold_get_domain_space(
        __isl_keep isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_get_space

```
__isl_give isl_space *isl_qpolynomial_fold_get_space(
        __isl_keep isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_get_domain_space

```
__isl_give isl_space *isl_pw_qpolynomial_get_domain_space(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_get_space

```
__isl_give isl_space *isl_pw_qpolynomial_get_space(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_get_domain_space

```
__isl_give isl_space *isl_pw_qpolynomial_fold_get_domain_space(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_get_space

```
__isl_give isl_space *isl_pw_qpolynomial_fold_get_space(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_get_space

```
__isl_give isl_space *isl_union_pw_qpolynomial_get_space(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_get_space

```
__isl_give isl_space *isl_union_pw_qpolynomial_fold_get_space(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_get_space

```
__isl_give isl_space *isl_multi_id_get_space(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_space

```
__isl_give isl_space *isl_multi_val_get_space(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_domain_space

```
__isl_give isl_space *isl_aff_get_domain_space(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_space

```
__isl_give isl_space *isl_aff_get_space(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_domain_space

```
__isl_give isl_space *isl_pw_aff_get_domain_space(
        __isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_space

```
__isl_give isl_space *isl_pw_aff_get_space(
        __isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_domain_space

```
__isl_give isl_space *isl_multi_aff_get_domain_space(
        __isl_keep isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_space

```
__isl_give isl_space *isl_multi_aff_get_space(
        __isl_keep isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_domain_space

```
__isl_give isl_space *isl_pw_multi_aff_get_domain_space(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_space

```
__isl_give isl_space *isl_pw_multi_aff_get_space(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_get_space

```
__isl_give isl_space *isl_union_pw_aff_get_space(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_get_space

```
__isl_give isl_space *isl_union_pw_multi_aff_get_space(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_domain_space

```
__isl_give isl_space *isl_multi_pw_aff_get_domain_space(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_space

```
__isl_give isl_space *isl_multi_pw_aff_get_space(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_domain_space

```
__isl_give isl_space *
isl_multi_union_pw_aff_get_domain_space(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_space

```
__isl_give isl_space *
isl_multi_union_pw_aff_get_space(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_get_space

```
__isl_give isl_space *isl_point_get_space(
        __isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_get_space

```
__isl_give isl_space *isl_fixed_box_get_space(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_dim

```
isl_size isl_space_dim(__isl_keep isl_space *space,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_dim

```
isl_size isl_local_space_dim(__isl_keep isl_local_space *ls,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_dim

```
isl_size isl_basic_set_dim(__isl_keep isl_basic_set *bset,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_tuple_dim

```
isl_size isl_set_tuple_dim(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim

```
isl_size isl_set_dim(__isl_keep isl_set *set,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_dim

```
isl_size isl_union_set_dim(__isl_keep isl_union_set *uset,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_dim

```
isl_size isl_basic_map_dim(__isl_keep isl_basic_map *bmap,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_tuple_dim

```
isl_size isl_map_domain_tuple_dim(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_tuple_dim

```
isl_size isl_map_range_tuple_dim(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_dim

```
isl_size isl_map_dim(__isl_keep isl_map *map,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_dim

```
isl_size isl_union_map_dim(__isl_keep isl_union_map *umap,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_dim

```
isl_size isl_multi_val_dim(__isl_keep isl_multi_val *mv,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_dim

```
isl_size isl_aff_dim(__isl_keep isl_aff *aff,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_dim

```
isl_size isl_multi_aff_dim(__isl_keep isl_multi_aff *maff,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_dim

```
isl_size isl_pw_aff_dim(__isl_keep isl_pw_aff *pwaff,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_dim

```
isl_size isl_pw_multi_aff_dim(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_dim

```
isl_size isl_multi_pw_aff_dim(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_dim

```
isl_size isl_union_pw_aff_dim(
        __isl_keep isl_union_pw_aff *upa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_dim

```
isl_size isl_union_pw_multi_aff_dim(
        __isl_keep isl_union_pw_multi_aff *upma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_dim

```
isl_size isl_multi_union_pw_aff_dim(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_dim

```
isl_size isl_union_pw_qpolynomial_dim(
        __isl_keep isl_union_pw_qpolynomial *upwqp,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_dim

```
isl_size isl_union_pw_qpolynomial_fold_dim(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_add_param_id

```
__isl_give isl_space *isl_space_add_param_id(
        __isl_take isl_space *space,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_drop_all_params

```
__isl_give isl_space *isl_space_drop_all_params(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_dim_id

```
__isl_give isl_space *isl_space_set_dim_id(
        __isl_take isl_space *space,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_dim_id

```
isl_bool isl_space_has_dim_id(__isl_keep isl_space *space,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_dim_id

```
__isl_give isl_id *isl_space_get_dim_id(
        __isl_keep isl_space *space,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_dim_name

```
__isl_give isl_space *isl_space_set_dim_name(
        __isl_take isl_space *space,
         enum isl_dim_type type, unsigned pos,
         __isl_keep const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_dim_name

```
isl_bool isl_space_has_dim_name(__isl_keep isl_space *space,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_dim_name

```
__isl_keep const char *isl_space_get_dim_name(
        __isl_keep isl_space *space,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_set_dim_id

```
__isl_give isl_local_space *isl_local_space_set_dim_id(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_has_dim_id

```
isl_bool isl_local_space_has_dim_id(
        __isl_keep isl_local_space *ls,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_get_dim_id

```
__isl_give isl_id *isl_local_space_get_dim_id(
        __isl_keep isl_local_space *ls,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_set_dim_name

```
__isl_give isl_local_space *isl_local_space_set_dim_name(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned pos, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_has_dim_name

```
isl_bool isl_local_space_has_dim_name(
        __isl_keep isl_local_space *ls,
        enum isl_dim_type type, unsigned pos)
const char *isl_local_space_get_dim_name(
        __isl_keep isl_local_space *ls,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_dim_name

```
const char *isl_constraint_get_dim_name(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_dim_id

```
__isl_give isl_id *isl_basic_set_get_dim_id(
        __isl_keep isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_set_dim_id

```
__isl_give isl_set *isl_set_set_dim_id(
        __isl_take isl_set *set, enum isl_dim_type type,
        unsigned pos, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_has_dim_id

```
isl_bool isl_set_has_dim_id(__isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_dim_id

```
__isl_give isl_id *isl_set_get_dim_id(
        __isl_keep isl_set *set, enum isl_dim_type type,
        unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_dim_name

```
const char *isl_basic_set_get_dim_name(
        __isl_keep isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_has_dim_name

```
isl_bool isl_set_has_dim_name(__isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_dim_name

```
const char *isl_set_get_dim_name(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_set_dim_id

```
__isl_give isl_map *isl_map_set_dim_id(
        __isl_take isl_map *map, enum isl_dim_type type,
        unsigned pos, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_has_dim_id

```
isl_bool isl_basic_map_has_dim_id(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_dim_id

```
isl_bool isl_map_has_dim_id(__isl_keep isl_map *map,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_dim_id

```
__isl_give isl_id *isl_map_get_dim_id(
        __isl_keep isl_map *map, enum isl_dim_type type,
        unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_get_dim_id

```
__isl_give isl_id *isl_union_map_get_dim_id(
        __isl_keep isl_union_map *umap,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_get_dim_name

```
const char *isl_basic_map_get_dim_name(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_dim_name

```
isl_bool isl_map_has_dim_name(__isl_keep isl_map *map,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_dim_name

```
const char *isl_map_get_dim_name(
        __isl_keep isl_map *map,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_dim_id

```
__isl_give isl_multi_val *isl_multi_val_set_dim_id(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_dim_id

```
__isl_give isl_id *isl_multi_val_get_dim_id(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_dim_name

```
__isl_give isl_multi_val *isl_multi_val_set_dim_name(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, unsigned pos, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_dim_id

```
__isl_give isl_aff *isl_aff_set_dim_id(
        __isl_take isl_aff *aff, enum isl_dim_type type,
        unsigned pos, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_dim_id

```
__isl_give isl_multi_aff *isl_multi_aff_set_dim_id(
        __isl_take isl_multi_aff *maff,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_set_dim_id

```
__isl_give isl_pw_aff *isl_pw_aff_set_dim_id(
        __isl_take isl_pw_aff *pma,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_dim_id

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_set_dim_id(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_dim_id

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_dim_id(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_dim_id

```
__isl_give isl_id *isl_multi_aff_get_dim_id(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_has_dim_id

```
isl_bool isl_pw_aff_has_dim_id(__isl_keep isl_pw_aff *pa,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_dim_id

```
__isl_give isl_id *isl_pw_aff_get_dim_id(
        __isl_keep isl_pw_aff *pa,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_dim_id

```
__isl_give isl_id *isl_pw_multi_aff_get_dim_id(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_dim_id

```
__isl_give isl_id *isl_multi_pw_aff_get_dim_id(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_dim_id

```
__isl_give isl_id *isl_multi_union_pw_aff_get_dim_id(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_dim_name

```
__isl_give isl_aff *isl_aff_set_dim_name(
        __isl_take isl_aff *aff, enum isl_dim_type type,
        unsigned pos, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_dim_name

```
__isl_give isl_multi_aff *isl_multi_aff_set_dim_name(
        __isl_take isl_multi_aff *maff,
        enum isl_dim_type type, unsigned pos, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_dim_name

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_set_dim_name(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned pos, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_set_dim_name

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_set_dim_name(
        __isl_take isl_union_pw_aff *upa,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_set_dim_name

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_set_dim_name(
        __isl_take isl_union_pw_multi_aff *upma,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_dim_name

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_dim_name(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_dim_name

```
const char *isl_aff_get_dim_name(__isl_keep isl_aff *aff,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_dim_name

```
const char *isl_pw_aff_get_dim_name(
        __isl_keep isl_pw_aff *pa,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_dim_name

```
const char *isl_pw_multi_aff_get_dim_name(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_set_dim_name

```
__isl_give isl_qpolynomial *isl_qpolynomial_set_dim_name(
        __isl_take isl_qpolynomial *qp,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_set_dim_name

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_set_dim_name(
        __isl_take isl_pw_qpolynomial *pwqp,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_set_dim_name

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_set_dim_name(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_set_dim_name

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_set_dim_name(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_set_dim_name

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_set_dim_name(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        enum isl_dim_type type, unsigned pos,
        const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_find_dim_by_id

```
int isl_space_find_dim_by_id(__isl_keep isl_space *space,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_find_dim_by_name

```
int isl_space_find_dim_by_name(__isl_keep isl_space *space,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_find_dim_by_name

```
int isl_local_space_find_dim_by_name(
        __isl_keep isl_local_space *ls,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_find_dim_by_id

```
int isl_multi_val_find_dim_by_id(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_find_dim_by_name

```
int isl_multi_val_find_dim_by_name(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_find_dim_by_id

```
int isl_set_find_dim_by_id(__isl_keep isl_set *set,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_find_dim_by_name

```
int isl_set_find_dim_by_name(__isl_keep isl_set *set,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_find_dim_by_id

```
int isl_map_find_dim_by_id(__isl_keep isl_map *map,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_find_dim_by_name

```
int isl_basic_map_find_dim_by_name(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_find_dim_by_name

```
int isl_map_find_dim_by_name(__isl_keep isl_map *map,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_find_dim_by_name

```
int isl_union_map_find_dim_by_name(
        __isl_keep isl_union_map *umap,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_find_dim_by_id

```
int isl_multi_aff_find_dim_by_id(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_find_dim_by_id

```
int isl_multi_pw_aff_find_dim_by_id(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_find_dim_by_id

```
int isl_multi_union_pw_aff_find_dim_by_id(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_find_dim_by_name

```
int isl_aff_find_dim_by_name(__isl_keep isl_aff *aff,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_find_dim_by_name

```
int isl_multi_aff_find_dim_by_name(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_find_dim_by_name

```
int isl_pw_aff_find_dim_by_name(__isl_keep isl_pw_aff *pa,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_find_dim_by_name

```
int isl_multi_pw_aff_find_dim_by_name(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_find_dim_by_name

```
int isl_pw_multi_aff_find_dim_by_name(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_find_dim_by_name

```
int isl_union_pw_aff_find_dim_by_name(
        __isl_keep isl_union_pw_aff *upa,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_find_dim_by_name

```
int isl_union_pw_multi_aff_find_dim_by_name(
        __isl_keep isl_union_pw_multi_aff *upma,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_find_dim_by_name

```
int isl_multi_union_pw_aff_find_dim_by_name(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_find_dim_by_name

```
int isl_pw_qpolynomial_find_dim_by_name(
        __isl_keep isl_pw_qpolynomial *pwqp,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_find_dim_by_name

```
int isl_pw_qpolynomial_fold_find_dim_by_name(
        __isl_keep isl_pw_qpolynomial_fold *pwf,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_find_dim_by_name

```
int isl_union_pw_qpolynomial_find_dim_by_name(
        __isl_keep isl_union_pw_qpolynomial *upwqp,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_find_dim_by_name

```
int isl_union_pw_qpolynomial_fold_find_dim_by_name(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf,
        enum isl_dim_type type, const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_domain_tuple_id

```
__isl_give isl_space *isl_space_set_domain_tuple_id(
        __isl_take isl_space *space,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_range_tuple_id

```
__isl_give isl_space *isl_space_set_range_tuple_id(
        __isl_take isl_space *space,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_tuple_id

```
__isl_give isl_space *isl_space_set_tuple_id(
        __isl_take isl_space *space,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_reset_tuple_id

```
__isl_give isl_space *isl_space_reset_tuple_id(
        __isl_take isl_space *space, enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_domain_tuple_id

```
isl_bool isl_space_has_domain_tuple_id(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_range_tuple_id

```
isl_bool isl_space_has_range_tuple_id(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_tuple_id

```
isl_bool isl_space_has_tuple_id(
        __isl_keep isl_space *space,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_domain_tuple_id

```
__isl_give isl_id *isl_space_get_domain_tuple_id(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_range_tuple_id

```
__isl_give isl_id *isl_space_get_range_tuple_id(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_tuple_id

```
__isl_give isl_id *isl_space_get_tuple_id(
        __isl_keep isl_space *space, enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_tuple_name

```
__isl_give isl_space *isl_space_set_tuple_name(
        __isl_take isl_space *space,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_has_tuple_name

```
isl_bool isl_space_has_tuple_name(
        __isl_keep isl_space *space,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_get_tuple_name

```
__isl_keep const char *isl_space_get_tuple_name(
        __isl_keep isl_space *space,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_set_tuple_id

```
__isl_give isl_local_space *isl_local_space_set_tuple_id(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_set_tuple_id

```
__isl_give isl_basic_set *isl_basic_set_set_tuple_id(
        __isl_take isl_basic_set *bset,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_set_tuple_id

```
__isl_give isl_set *isl_set_set_tuple_id(
        __isl_take isl_set *set, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_reset_tuple_id

```
__isl_give isl_set *isl_set_reset_tuple_id(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_has_tuple_id

```
isl_bool isl_set_has_tuple_id(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_tuple_id

```
__isl_give isl_id *isl_set_get_tuple_id(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_set_tuple_name

```
__isl_give isl_basic_set *isl_basic_set_set_tuple_name(
        __isl_take isl_basic_set *set, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_set_tuple_name

```
__isl_give isl_set *isl_set_set_tuple_name(
        __isl_take isl_set *set, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_tuple_name

```
const char *isl_basic_set_get_tuple_name(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_has_tuple_name

```
isl_bool isl_set_has_tuple_name(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_tuple_name

```
const char *isl_set_get_tuple_name(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_set_tuple_id

```
__isl_give isl_basic_map *isl_basic_map_set_tuple_id(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_set_domain_tuple_id

```
__isl_give isl_map *isl_map_set_domain_tuple_id(
        __isl_take isl_map *map, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_set_range_tuple_id

```
__isl_give isl_map *isl_map_set_range_tuple_id(
        __isl_take isl_map *map, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_set_tuple_id

```
__isl_give isl_map *isl_map_set_tuple_id(
        __isl_take isl_map *map, enum isl_dim_type type,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_reset_tuple_id

```
__isl_give isl_map *isl_map_reset_tuple_id(
        __isl_take isl_map *map, enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_domain_tuple_id

```
isl_bool isl_map_has_domain_tuple_id(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_range_tuple_id

```
isl_bool isl_map_has_range_tuple_id(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_tuple_id

```
isl_bool isl_map_has_tuple_id(__isl_keep isl_map *map,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_domain_tuple_id

```
__isl_give isl_id *isl_map_get_domain_tuple_id(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_range_tuple_id

```
__isl_give isl_id *isl_map_get_range_tuple_id(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_tuple_id

```
__isl_give isl_id *isl_map_get_tuple_id(
        __isl_keep isl_map *map, enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_set_tuple_name

```
__isl_give isl_map *isl_map_set_tuple_name(
        __isl_take isl_map *map,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_get_tuple_name

```
const char *isl_basic_map_get_tuple_name(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_set_tuple_name

```
__isl_give isl_basic_map *isl_basic_map_set_tuple_name(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_has_tuple_name

```
isl_bool isl_map_has_tuple_name(__isl_keep isl_map *map,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_tuple_name

```
const char *isl_map_get_tuple_name(
        __isl_keep isl_map *map,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_range_tuple_id

```
__isl_give isl_multi_val *isl_multi_val_set_range_tuple_id(
        __isl_take isl_multi_val *mv,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_tuple_id

```
__isl_give isl_multi_val *isl_multi_val_set_tuple_id(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_reset_range_tuple_id

```
__isl_give isl_multi_val *
isl_multi_val_reset_range_tuple_id(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_reset_tuple_id

```
__isl_give isl_multi_val *isl_multi_val_reset_tuple_id(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_has_range_tuple_id

```
isl_bool isl_multi_val_has_range_tuple_id(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_range_tuple_id

```
__isl_give isl_id *isl_multi_val_get_range_tuple_id(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_has_tuple_id

```
isl_bool isl_multi_val_has_tuple_id(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_tuple_id

```
__isl_give isl_id *isl_multi_val_get_tuple_id(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_tuple_name

```
__isl_give isl_multi_val *isl_multi_val_set_tuple_name(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_tuple_name

```
const char *isl_multi_val_get_tuple_name(
        __isl_keep isl_multi_val *mv,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_tuple_id

```
__isl_give isl_aff *isl_aff_set_tuple_id(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_range_tuple_id

```
__isl_give isl_multi_aff *isl_multi_aff_set_range_tuple_id(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_tuple_id

```
__isl_give isl_multi_aff *isl_multi_aff_set_tuple_id(
        __isl_take isl_multi_aff *maff,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_set_tuple_id

```
__isl_give isl_pw_aff *isl_pw_aff_set_tuple_id(
        __isl_take isl_pw_aff *pwaff,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_set_range_tuple_id

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_set_range_tuple_id(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_set_tuple_id

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_set_tuple_id(
        __isl_take isl_pw_multi_aff *pma,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_range_tuple_id

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_set_range_tuple_id(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_range_tuple_id

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_range_tuple_id(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_tuple_id

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_tuple_id(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_reset_range_tuple_id

```
__isl_give isl_multi_aff *
isl_multi_aff_reset_range_tuple_id(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_reset_range_tuple_id

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_reset_range_tuple_id(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_reset_range_tuple_id

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_reset_range_tuple_id(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_reset_tuple_id

```
__isl_give isl_multi_aff *isl_multi_aff_reset_tuple_id(
        __isl_take isl_multi_aff *ma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_reset_tuple_id

```
__isl_give isl_pw_aff *isl_pw_aff_reset_tuple_id(
        __isl_take isl_pw_aff *pa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_reset_tuple_id

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_reset_tuple_id(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_reset_tuple_id

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_reset_tuple_id(
        __isl_take isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_reset_tuple_id

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_reset_tuple_id(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_has_range_tuple_id

```
isl_bool isl_multi_aff_has_range_tuple_id(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_range_tuple_id

```
__isl_give isl_id *isl_multi_aff_get_range_tuple_id(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_has_tuple_id

```
isl_bool isl_multi_aff_has_tuple_id(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_tuple_id

```
__isl_give isl_id *isl_multi_aff_get_tuple_id(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_has_tuple_id

```
isl_bool isl_pw_aff_has_tuple_id(__isl_keep isl_pw_aff *pa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_get_tuple_id

```
__isl_give isl_id *isl_pw_aff_get_tuple_id(
        __isl_keep isl_pw_aff *pa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_has_range_tuple_id

```
isl_bool isl_pw_multi_aff_has_range_tuple_id(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_has_tuple_id

```
isl_bool isl_pw_multi_aff_has_tuple_id(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_range_tuple_id

```
__isl_give isl_id *isl_pw_multi_aff_get_range_tuple_id(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_tuple_id

```
__isl_give isl_id *isl_pw_multi_aff_get_tuple_id(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_has_range_tuple_id

```
isl_bool isl_multi_pw_aff_has_range_tuple_id(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_range_tuple_id

```
__isl_give isl_id *isl_multi_pw_aff_get_range_tuple_id(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_has_tuple_id

```
isl_bool isl_multi_pw_aff_has_tuple_id(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_tuple_id

```
__isl_give isl_id *isl_multi_pw_aff_get_tuple_id(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_has_range_tuple_id

```
isl_bool isl_multi_union_pw_aff_has_range_tuple_id(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_range_tuple_id

```
__isl_give isl_id *
isl_multi_union_pw_aff_get_range_tuple_id(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_has_tuple_id

```
isl_bool isl_multi_union_pw_aff_has_tuple_id(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_tuple_id

```
__isl_give isl_id *isl_multi_union_pw_aff_get_tuple_id(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_tuple_name

```
__isl_give isl_multi_aff *isl_multi_aff_set_tuple_name(
        __isl_take isl_multi_aff *maff,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_tuple_name

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_set_tuple_name(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_tuple_name

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_tuple_name(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, const char *s);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_tuple_name

```
const char *isl_multi_aff_get_tuple_name(
        __isl_keep isl_multi_aff *multi,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_has_tuple_name

```
isl_bool isl_pw_multi_aff_has_tuple_name(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_tuple_name

```
const char *isl_pw_multi_aff_get_tuple_name(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_tuple_name

```
const char *isl_multi_union_pw_aff_get_tuple_name(
        __isl_keep isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_reset_user

```
__isl_give isl_space *isl_space_reset_user(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_reset_user

```
__isl_give isl_set *isl_set_reset_user(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_reset_user

```
__isl_give isl_map *isl_map_reset_user(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_reset_user

```
__isl_give isl_union_set *isl_union_set_reset_user(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_reset_user

```
__isl_give isl_union_map *isl_union_map_reset_user(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_reset_user

```
__isl_give isl_multi_id *isl_multi_id_reset_user(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_reset_user

```
__isl_give isl_multi_val *isl_multi_val_reset_user(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_reset_user

```
__isl_give isl_multi_aff *isl_multi_aff_reset_user(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_reset_user

```
__isl_give isl_pw_aff *isl_pw_aff_reset_user(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_reset_user

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_reset_user(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_reset_user

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_reset_user(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_reset_user

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_reset_user(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_reset_user

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_reset_user(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_reset_user

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_reset_user(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_reset_user

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_reset_user(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_reset_user

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_reset_user(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_reset_user

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_reset_user(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_reset_user

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_reset_user(
        __isl_take isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_local_space

```
__isl_give isl_local_space *isl_constraint_get_local_space(
        __isl_keep isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_local_space

```
__isl_give isl_local_space *isl_basic_set_get_local_space(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_get_local_space

```
__isl_give isl_local_space *isl_basic_map_get_local_space(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_domain_local_space

```
__isl_give isl_local_space *isl_aff_get_domain_local_space(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_local_space

```
__isl_give isl_local_space *isl_aff_get_local_space(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_from_space

```
__isl_give isl_local_space *isl_local_space_from_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_is_params

```
isl_bool isl_local_space_is_params(
        __isl_keep isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_is_set

```
isl_bool isl_local_space_is_set(
        __isl_keep isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_get_space

```
__isl_give isl_space *isl_local_space_get_space(
        __isl_keep isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_get_div

```
__isl_give isl_aff *isl_local_space_get_div(
        __isl_keep isl_local_space *ls, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_copy

```
__isl_give isl_local_space *isl_local_space_copy(
        __isl_keep isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_free

```
__isl_null isl_local_space *isl_local_space_free(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_is_equal

```
isl_bool isl_local_space_is_equal(
        __isl_keep isl_local_space *ls1,
        __isl_keep isl_local_space *ls2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_empty

```
__isl_give isl_basic_set *isl_basic_set_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_empty

```
__isl_give isl_basic_map *isl_basic_map_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_empty

```
__isl_give isl_set *isl_set_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_empty

```
__isl_give isl_map *isl_map_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_empty_ctx

```
__isl_give isl_union_set *isl_union_set_empty_ctx(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_empty_space

```
__isl_give isl_union_set *isl_union_set_empty_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_empty

```
__isl_give isl_union_set *isl_union_set_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_empty_ctx

```
__isl_give isl_union_map *isl_union_map_empty_ctx(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_empty_space

```
__isl_give isl_union_map *isl_union_map_empty_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_empty

```
__isl_give isl_union_map *isl_union_map_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_universe

```
__isl_give isl_basic_set *isl_basic_set_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_universe

```
__isl_give isl_set *isl_set_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_universe_set

```
__isl_give isl_set *isl_space_universe_set(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_universe

```
__isl_give isl_basic_map *isl_basic_map_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_universe

```
__isl_give isl_map *isl_map_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_universe_map

```
__isl_give isl_map *isl_space_universe_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_universe

```
__isl_give isl_union_set *isl_union_set_universe(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_universe

```
__isl_give isl_union_map *isl_union_map_universe(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_nat_universe

```
__isl_give isl_basic_set *isl_basic_set_nat_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_nat_universe

```
__isl_give isl_basic_map *isl_basic_map_nat_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_nat_universe

```
__isl_give isl_set *isl_set_nat_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_nat_universe

```
__isl_give isl_map *isl_map_nat_universe(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_identity

```
__isl_give isl_basic_map *isl_basic_map_identity(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_identity

```
__isl_give isl_map *isl_map_identity(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_lt

```
__isl_give isl_map *isl_map_lex_lt(
        __isl_take isl_space *set_space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_le

```
__isl_give isl_map *isl_map_lex_le(
        __isl_take isl_space *set_space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_gt

```
__isl_give isl_map *isl_map_lex_gt(
        __isl_take isl_space *set_space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_ge

```
__isl_give isl_map *isl_map_lex_ge(
        __isl_take isl_space *set_space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_lt_first

```
__isl_give isl_map *isl_map_lex_lt_first(
        __isl_take isl_space *space, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_le_first

```
__isl_give isl_map *isl_map_lex_le_first(
        __isl_take isl_space *space, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_gt_first

```
__isl_give isl_map *isl_map_lex_gt_first(
        __isl_take isl_space *space, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_ge_first

```
__isl_give isl_map *isl_map_lex_ge_first(
        __isl_take isl_space *space, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_to_set

```
__isl_give isl_set *isl_basic_set_to_set(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_basic_set

```
__isl_give isl_set *isl_set_from_basic_set(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_basic_map

```
__isl_give isl_map *isl_map_from_basic_map(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_from_basic_set

```
__isl_give isl_union_set *isl_union_set_from_basic_set(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_basic_map

```
__isl_give isl_union_map *isl_union_map_from_basic_map(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_to_union_set

```
__isl_give isl_union_set *isl_set_to_union_set(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_from_set

```
__isl_give isl_union_set *isl_union_set_from_set(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_to_union_map

```
__isl_give isl_union_map *isl_map_to_union_map(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_map

```
__isl_give isl_union_map *isl_union_map_from_map(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_isa_set

```
isl_bool isl_union_set_isa_set(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_as_set

```
__isl_give isl_set *isl_union_set_as_set(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_union_set

```
__isl_give isl_set *isl_set_from_union_set(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_isa_map

```
isl_bool isl_union_map_isa_map(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_as_map

```
__isl_give isl_map *isl_union_map_as_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_union_map

```
__isl_give isl_map *isl_map_from_union_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_copy

```
__isl_give isl_basic_set *isl_basic_set_copy(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_copy

```
__isl_give isl_set *isl_set_copy(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_copy

```
__isl_give isl_union_set *isl_union_set_copy(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_copy

```
__isl_give isl_basic_map *isl_basic_map_copy(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_copy

```
__isl_give isl_map *isl_map_copy(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_copy

```
__isl_give isl_union_map *isl_union_map_copy(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_free

```
__isl_null isl_basic_set *isl_basic_set_free(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_free

```
__isl_null isl_set *isl_set_free(__isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_free

```
__isl_null isl_union_set *isl_union_set_free(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_free

```
__isl_null isl_basic_map *isl_basic_map_free(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_free

```
__isl_null isl_map *isl_map_free(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_free

```
__isl_null isl_union_map *isl_union_map_free(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_alloc_equality

```
__isl_give isl_constraint *isl_constraint_alloc_equality(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_alloc_inequality

```
__isl_give isl_constraint *isl_constraint_alloc_inequality(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_set_constant_si

```
__isl_give isl_constraint *isl_constraint_set_constant_si(
        __isl_take isl_constraint *constraint, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_set_constant_val

```
__isl_give isl_constraint *isl_constraint_set_constant_val(
        __isl_take isl_constraint *constraint,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_set_coefficient_si

```
__isl_give isl_constraint *isl_constraint_set_coefficient_si(
        __isl_take isl_constraint *constraint,
        enum isl_dim_type type, int pos, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_set_coefficient_val

```
__isl_give isl_constraint *
isl_constraint_set_coefficient_val(
        __isl_take isl_constraint *constraint,
        enum isl_dim_type type, int pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_add_constraint

```
__isl_give isl_basic_map *isl_basic_map_add_constraint(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_add_constraint

```
__isl_give isl_basic_set *isl_basic_set_add_constraint(
        __isl_take isl_basic_set *bset,
        __isl_take isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_add_constraint

```
__isl_give isl_map *isl_map_add_constraint(
        __isl_take isl_map *map,
        __isl_take isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_add_constraint

```
__isl_give isl_set *isl_set_add_constraint(
        __isl_take isl_set *set,
        __isl_take isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_project_out

```
bset = isl_basic_set_project_out(bset, isl_dim_set, 1, 1);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_add_unnamed_tuple_ui

```
space = isl_space_add_unnamed_tuple_ui(space, 1);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_identity_on_domain_space

```
ma = isl_multi_aff_identity_on_domain_space(
        isl_space_copy(space));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_at

```
var = isl_multi_aff_get_at(ma, 0);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_val_on_domain_space

```
cst = isl_aff_val_on_domain_space(isl_space_copy(space), v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_ge_basic_set

```
bset = isl_aff_ge_basic_set(isl_aff_copy(var), cst);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_intersect

```
bset = isl_basic_set_intersect(bset,
        isl_aff_le_basic_set(var, cst));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_scale_val

```
ma = isl_multi_aff_scale_val(ma, isl_val_copy(two));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_preimage_multi_aff

```
bset = isl_basic_set_preimage_multi_aff(bset,
        isl_multi_aff_copy(ma));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_scale_down_val

```
ma = isl_multi_aff_scale_down_val(ma, isl_val_copy(two));
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_read_from_str

```
bset = isl_basic_set_read_from_str(ctx,
        "{[i] : exists (a : i = 2a and i >= 10 and i <= 42)}");
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_from_constraint_matrices

```
__isl_give isl_basic_set *isl_basic_set_from_constraint_matrices(
        __isl_take isl_space *space,
        __isl_take isl_mat *eq, __isl_take isl_mat *ineq,
        enum isl_dim_type c1,
        enum isl_dim_type c2, enum isl_dim_type c3,
        enum isl_dim_type c4);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_from_constraint_matrices

```
__isl_give isl_basic_map *isl_basic_map_from_constraint_matrices(
        __isl_take isl_space *space,
        __isl_take isl_mat *eq, __isl_take isl_mat *ineq,
        enum isl_dim_type c1,
        enum isl_dim_type c2, enum isl_dim_type c3,
        enum isl_dim_type c4, enum isl_dim_type c5);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_from_multi_aff

```
__isl_give isl_basic_set *isl_basic_set_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_as_set

```
__isl_give isl_set *isl_multi_aff_as_set(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_multi_aff

```
__isl_give isl_set *isl_set_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_from_aff

```
__isl_give isl_basic_map *isl_basic_map_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_aff

```
__isl_give isl_map *isl_map_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_from_aff_list

```
__isl_give isl_basic_map *isl_basic_map_from_aff_list(
        __isl_take isl_space *domain_space,
        __isl_take isl_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_from_multi_aff

```
__isl_give isl_basic_map *isl_basic_map_from_multi_aff(
        __isl_take isl_multi_aff *maff)
__isl_give isl_map *isl_multi_aff_as_map(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_multi_aff

```
__isl_give isl_map *isl_map_from_multi_aff(
        __isl_take isl_multi_aff *maff)
__isl_give isl_set *isl_set_from_pw_aff(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_as_map

```
__isl_give isl_map *isl_pw_aff_as_map(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_pw_aff

```
__isl_give isl_map *isl_map_from_pw_aff(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_as_set

```
__isl_give isl_set *isl_pw_multi_aff_as_set(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_pw_multi_aff

```
__isl_give isl_set *isl_set_from_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_as_map

```
__isl_give isl_map *isl_pw_multi_aff_as_map(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_pw_multi_aff

```
__isl_give isl_map *isl_map_from_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_as_set

```
__isl_give isl_set *isl_multi_pw_aff_as_set(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_multi_pw_aff

```
__isl_give isl_set *isl_set_from_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_as_map

```
__isl_give isl_map *isl_multi_pw_aff_as_map(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_multi_pw_aff

```
__isl_give isl_map *isl_map_from_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_union_pw_aff

```
__isl_give isl_union_map *isl_union_map_from_union_pw_aff(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_as_union_map

```
__isl_give isl_union_map *
isl_union_pw_multi_aff_as_union_map(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_union_pw_multi_aff

```
__isl_give isl_union_map *
isl_union_map_from_union_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_from_multi_union_pw_aff(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_compute_divs

```
__isl_give isl_set *isl_set_compute_divs(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_compute_divs

```
__isl_give isl_map *isl_map_compute_divs(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_compute_divs

```
__isl_give isl_union_set *isl_union_set_compute_divs(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_compute_divs

```
__isl_give isl_union_map *isl_union_map_compute_divs(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_remove_divs

```
__isl_give isl_basic_set *isl_basic_set_remove_divs(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_remove_divs

```
__isl_give isl_set *isl_set_remove_divs(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_remove_divs

```
__isl_give isl_basic_map *isl_basic_map_remove_divs(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_remove_divs

```
__isl_give isl_map *isl_map_remove_divs(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_remove_divs

```
__isl_give isl_union_set *isl_union_set_remove_divs(
        __isl_take isl_union_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_remove_divs

```
__isl_give isl_union_map *isl_union_map_remove_divs(
        __isl_take isl_union_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_remove_divs_involving_dims

```
__isl_give isl_basic_set *
isl_basic_set_remove_divs_involving_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_remove_divs_involving_dims

```
__isl_give isl_basic_map *
isl_basic_map_remove_divs_involving_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_remove_divs_involving_dims

```
__isl_give isl_set *isl_set_remove_divs_involving_dims(
        __isl_take isl_set *set, enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_remove_divs_involving_dims

```
__isl_give isl_map *isl_map_remove_divs_involving_dims(
        __isl_take isl_map *map, enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_remove_unknown_divs

```
__isl_give isl_basic_set *
isl_basic_set_remove_unknown_divs(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_remove_unknown_divs

```
__isl_give isl_set *isl_set_remove_unknown_divs(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_remove_unknown_divs

```
__isl_give isl_map *isl_map_remove_unknown_divs(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_foreach_set

```
isl_stat isl_union_set_foreach_set(
        __isl_keep isl_union_set *uset,
        isl_stat (*fn)(__isl_take isl_set *set, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_every_set

```
isl_bool isl_union_set_every_set(
        __isl_keep isl_union_set *uset,
        isl_bool (*test)(__isl_keep isl_set *set,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_foreach_map

```
isl_stat isl_union_map_foreach_map(
        __isl_keep isl_union_map *umap,
        isl_stat (*fn)(__isl_take isl_map *map, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_every_map

```
isl_bool isl_union_map_every_map(
        __isl_keep isl_union_map *umap,
        isl_bool (*test)(__isl_keep isl_map *map,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_n_set

```
isl_size isl_union_set_n_set(__isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_n_map

```
isl_size isl_union_map_n_map(__isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_extract_set

```
__isl_give isl_set *isl_union_set_extract_set(
        __isl_keep isl_union_set *uset,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_extract_map

```
__isl_give isl_map *isl_union_map_extract_map(
        __isl_keep isl_union_map *umap,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_foreach_basic_set

```
isl_stat isl_set_foreach_basic_set(__isl_keep isl_set *set,
        isl_stat (*fn)(__isl_take isl_basic_set *bset,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_foreach_basic_map

```
isl_stat isl_map_foreach_basic_map(__isl_keep isl_map *map,
        isl_stat (*fn)(__isl_take isl_basic_map *bmap,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_make_disjoint

```
__isl_give isl_set *isl_set_make_disjoint(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_make_disjoint

```
__isl_give isl_map *isl_map_make_disjoint(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_n_basic_set

```
isl_size isl_set_n_basic_set(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_n_basic_map

```
isl_size isl_map_n_basic_map(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_basic_set_list

```
__isl_give isl_basic_set_list *isl_set_get_basic_set_list(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_get_basic_set_list

```
__isl_give isl_basic_set_list *
isl_union_set_get_basic_set_list(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_get_set_list

```
__isl_give isl_set_list *isl_union_set_get_set_list(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_basic_map_list

```
__isl_give isl_basic_map_list *isl_map_get_basic_map_list(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_get_map_list

```
__isl_give isl_map_list *isl_union_map_get_map_list(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_n_constraint

```
isl_size isl_basic_set_n_constraint(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_foreach_constraint

```
isl_stat isl_basic_set_foreach_constraint(
        __isl_keep isl_basic_set *bset,
        isl_stat (*fn)(__isl_take isl_constraint *c,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_n_constraint

```
isl_size isl_basic_map_n_constraint(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_foreach_constraint

```
isl_stat isl_basic_map_foreach_constraint(
        __isl_keep isl_basic_map *bmap,
        isl_stat (*fn)(__isl_take isl_constraint *c,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_free

```
__isl_null isl_constraint *isl_constraint_free(
        __isl_take isl_constraint *c);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_is_equality

```
isl_bool isl_constraint_is_equality(
        __isl_keep isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_get_constraint_list

```
__isl_give isl_constraint_list *
isl_basic_map_get_constraint_list(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_get_constraint_list

```
__isl_give isl_constraint_list *
isl_basic_set_get_constraint_list(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_is_lower_bound

```
isl_bool isl_constraint_is_lower_bound(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_is_upper_bound

```
isl_bool isl_constraint_is_upper_bound(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_constant_val

```
__isl_give isl_val *isl_constraint_get_constant_val(
        __isl_keep isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_coefficient_val

```
__isl_give isl_val *isl_constraint_get_coefficient_val(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_div

```
__isl_give isl_aff *isl_constraint_get_div(
        __isl_keep isl_constraint *constraint, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_equalities_matrix

```
__isl_give isl_mat *isl_basic_set_equalities_matrix(
        __isl_keep isl_basic_set *bset,
        enum isl_dim_type c1, enum isl_dim_type c2,
        enum isl_dim_type c3, enum isl_dim_type c4);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_inequalities_matrix

```
__isl_give isl_mat *isl_basic_set_inequalities_matrix(
        __isl_keep isl_basic_set *bset,
        enum isl_dim_type c1, enum isl_dim_type c2,
        enum isl_dim_type c3, enum isl_dim_type c4);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_equalities_matrix

```
__isl_give isl_mat *isl_basic_map_equalities_matrix(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type c1,
        enum isl_dim_type c2, enum isl_dim_type c3,
        enum isl_dim_type c4, enum isl_dim_type c5);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_inequalities_matrix

```
__isl_give isl_mat *isl_basic_map_inequalities_matrix(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type c1,
        enum isl_dim_type c2, enum isl_dim_type c3,
        enum isl_dim_type c4, enum isl_dim_type c5);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_zero

```
__isl_give isl_point *isl_point_zero(__isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_get_multi_val

```
__isl_give isl_multi_val *isl_point_get_multi_val(
        __isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_get_coordinate_val

```
__isl_give isl_val *isl_point_get_coordinate_val(
        __isl_keep isl_point *pnt,
        enum isl_dim_type type, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_set_coordinate_val

```
__isl_give isl_point *isl_point_set_coordinate_val(
        __isl_take isl_point *pnt,
        enum isl_dim_type type, int pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_add_ui

```
__isl_give isl_point *isl_point_add_ui(
        __isl_take isl_point *pnt,
        enum isl_dim_type type, int pos, unsigned val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_sub_ui

```
__isl_give isl_point *isl_point_sub_ui(
        __isl_take isl_point *pnt,
        enum isl_dim_type type, int pos, unsigned val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_copy

```
__isl_give isl_point *isl_point_copy(
        __isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_free

```
__isl_null isl_point *isl_point_free(
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_from_point

```
__isl_give isl_basic_set *isl_basic_set_from_point(
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_to_set

```
__isl_give isl_set *isl_point_to_set(
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_point

```
__isl_give isl_set *isl_set_from_point(
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_from_point

```
__isl_give isl_union_set *isl_union_set_from_point(
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_box_from_points

```
__isl_give isl_basic_set *isl_basic_set_box_from_points(
        __isl_take isl_point *pnt1,
        __isl_take isl_point *pnt2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_box_from_points

```
__isl_give isl_set *isl_set_box_from_points(
        __isl_take isl_point *pnt1,
        __isl_take isl_point *pnt2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_foreach_point

```
isl_stat isl_set_foreach_point(__isl_keep isl_set *set,
        isl_stat (*fn)(__isl_take isl_point *pnt,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_foreach_point

```
isl_stat isl_union_set_foreach_point(
        __isl_keep isl_union_set *uset,
        isl_stat (*fn)(__isl_take isl_point *pnt,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_sample_point

```
__isl_give isl_point *isl_basic_set_sample_point(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_sample_point

```
__isl_give isl_point *isl_set_sample_point(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_sample_point

```
__isl_give isl_point *isl_union_set_sample_point(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_is_void

```
isl_bool isl_point_is_void(__isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_zero_on_domain_space

```
__isl_give isl_aff *isl_aff_zero_on_domain_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zero_aff_on_domain

```
__isl_give isl_aff *isl_space_zero_aff_on_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_zero_on_domain

```
__isl_give isl_aff *isl_aff_zero_on_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_val_on_domain

```
__isl_give isl_aff *isl_aff_val_on_domain(
        __isl_take isl_local_space *ls,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_param_on_domain_space_id

```
__isl_give isl_aff *isl_aff_param_on_domain_space_id(
        __isl_take isl_space *space,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_param_aff_on_domain_id

```
__isl_give isl_aff *isl_space_param_aff_on_domain_id(
        __isl_take isl_space *space,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_var_on_domain

```
__isl_give isl_aff *isl_aff_var_on_domain(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_nan_on_domain_space

```
__isl_give isl_aff *isl_aff_nan_on_domain_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_nan_on_domain

```
__isl_give isl_aff *isl_aff_nan_on_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_copy

```
__isl_give isl_aff *isl_aff_copy(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_free

```
__isl_null isl_aff *isl_aff_free(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_bound

```
__isl_give isl_aff *isl_constraint_get_bound(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_get_aff

```
__isl_give isl_aff *isl_constraint_get_aff(
        __isl_keep isl_constraint *constraint);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_equality_from_aff

```
__isl_give isl_constraint *isl_equality_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_inequality_from_aff

```
__isl_give isl_constraint *isl_inequality_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_constant_val

```
__isl_give isl_val *isl_aff_get_constant_val(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_coefficient_val

```
__isl_give isl_val *isl_aff_get_coefficient_val(
        __isl_keep isl_aff *aff,
        enum isl_dim_type type, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_coefficient_sgn

```
int isl_aff_coefficient_sgn(__isl_keep isl_aff *aff,
        enum isl_dim_type type, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_denominator_val

```
__isl_give isl_val *isl_aff_get_denominator_val(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_get_div

```
__isl_give isl_aff *isl_aff_get_div(
        __isl_keep isl_aff *aff, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_constant_si

```
__isl_give isl_aff *isl_aff_set_constant_si(
        __isl_take isl_aff *aff, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_constant_val

```
__isl_give isl_aff *isl_aff_set_constant_val(
        __isl_take isl_aff *aff, __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_coefficient_si

```
__isl_give isl_aff *isl_aff_set_coefficient_si(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, int pos, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_set_coefficient_val

```
__isl_give isl_aff *isl_aff_set_coefficient_val(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, int pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_constant_si

```
__isl_give isl_aff *isl_aff_add_constant_si(
        __isl_take isl_aff *aff, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_constant_val

```
__isl_give isl_aff *isl_aff_add_constant_val(
        __isl_take isl_aff *aff, __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_constant_num_si

```
__isl_give isl_aff *isl_aff_add_constant_num_si(
        __isl_take isl_aff *aff, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_coefficient_si

```
__isl_give isl_aff *isl_aff_add_coefficient_si(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, int pos, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_coefficient_val

```
__isl_give isl_aff *isl_aff_add_coefficient_val(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, int pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_zero_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_zero_on_domain(
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_one_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_one_on_domain(
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_infty_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_infty_on_domain(
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_neginfty_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_neginfty_on_domain(
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_nan_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_nan_on_domain(
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_val_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_val_on_domain(
        __isl_take isl_space *domain,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_var_on_domain

```
__isl_give isl_qpolynomial *isl_qpolynomial_var_on_domain(
        __isl_take isl_space *domain,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_from_aff

```
__isl_give isl_qpolynomial *isl_qpolynomial_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_isa_aff

```
isl_bool isl_qpolynomial_isa_aff(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_as_aff

```
__isl_give isl_aff *isl_qpolynomial_as_aff(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_copy

```
__isl_give isl_qpolynomial *isl_qpolynomial_copy(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_free

```
__isl_null isl_qpolynomial *isl_qpolynomial_free(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_get_constant_val

```
__isl_give isl_val *isl_qpolynomial_get_constant_val(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_foreach_term

```
isl_stat isl_qpolynomial_foreach_term(
        __isl_keep isl_qpolynomial *qp,
        isl_stat (*fn)(__isl_take isl_term *term,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_term_dim

```
isl_size isl_term_dim(__isl_keep isl_term *term,
        enum isl_dim_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_term_get_coefficient_val

```
__isl_give isl_val *isl_term_get_coefficient_val(
        __isl_keep isl_term *term);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_term_get_exp

```
isl_size isl_term_get_exp(__isl_keep isl_term *term,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_term_get_div

```
__isl_give isl_aff *isl_term_get_div(
        __isl_keep isl_term *term, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_term_free

```
__isl_null isl_term *isl_term_free(
        __isl_take isl_term *term);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_copy

```
__isl_give isl_qpolynomial_fold *
isl_qpolynomial_fold_copy(
        __isl_keep isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_free

```
__isl_null isl_qpolynomial_fold *
isl_qpolynomial_fold_free(
        __isl_take isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_get_type

```
enum isl_fold isl_qpolynomial_fold_get_type(
        __isl_keep isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_get_type

```
enum isl_fold isl_pw_qpolynomial_fold_get_type(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_get_type

```
enum isl_fold isl_union_pw_qpolynomial_fold_get_type(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_foreach_qpolynomial

```
isl_stat isl_qpolynomial_fold_foreach_qpolynomial(
        __isl_keep isl_qpolynomial_fold *fold,
        isl_stat (*fn)(__isl_take isl_qpolynomial *qp,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_zero

```
__isl_give isl_multi_val *isl_multi_val_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zero_multi_val

```
__isl_give isl_multi_val *isl_space_zero_multi_val(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_zero

```
__isl_give isl_multi_aff *isl_multi_aff_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zero_multi_aff

```
__isl_give isl_multi_aff *isl_space_zero_multi_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_zero

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zero_multi_pw_aff

```
__isl_give isl_multi_pw_aff *isl_space_zero_multi_pw_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_zero

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zero_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_space_zero_multi_union_pw_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_identity_multi_aff_on_domain

```
__isl_give isl_multi_aff *
isl_space_identity_multi_aff_on_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_identity_on_domain_space

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_identity_on_domain_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_identity_multi_pw_aff_on_domain

```
__isl_give isl_multi_pw_aff *
isl_space_identity_multi_pw_aff_on_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_identity

```
__isl_give isl_multi_aff *isl_multi_aff_identity(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_identity

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_identity(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_identity_multi_aff

```
__isl_give isl_multi_aff *
isl_multi_aff_identity_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_identity_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_identity_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_domain_map

```
__isl_give isl_multi_aff *isl_multi_aff_domain_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_map_multi_aff

```
__isl_give isl_multi_aff *isl_space_domain_map_multi_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_map

```
__isl_give isl_multi_aff *isl_multi_aff_range_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_map_multi_aff

```
__isl_give isl_multi_aff *isl_space_range_map_multi_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_project_out_map

```
__isl_give isl_multi_aff *isl_multi_aff_project_out_map(
        __isl_take isl_space *space,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_from_aff

```
__isl_give isl_multi_aff *isl_multi_aff_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_pw_aff

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_from_pw_aff(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_union_pw_aff(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_from_id_list

```
__isl_give isl_multi_id *isl_multi_id_from_id_list(
        __isl_take isl_space *space,
        __isl_take isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_id

```
__isl_give isl_multi_id *isl_space_multi_id(
        __isl_take isl_space *space,
        __isl_take isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_from_val_list

```
__isl_give isl_multi_val *isl_multi_val_from_val_list(
        __isl_take isl_space *space,
        __isl_take isl_val_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_val

```
__isl_give isl_multi_val *isl_space_multi_val(
        __isl_take isl_space *space,
        __isl_take isl_val_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_from_aff_list

```
__isl_give isl_multi_aff *isl_multi_aff_from_aff_list(
        __isl_take isl_space *space,
        __isl_take isl_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_aff

```
__isl_give isl_multi_aff *isl_space_multi_aff(
        __isl_take isl_space *space,
        __isl_take isl_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_pw_aff_list

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_from_pw_aff_list(
        __isl_take isl_space *space,
        __isl_take isl_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_space_multi_pw_aff(
        __isl_take isl_space *space,
        __isl_take isl_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_union_pw_aff_list

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_union_pw_aff_list(
        __isl_take isl_space *space,
        __isl_take isl_union_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_space_multi_union_pw_aff(
        __isl_take isl_space *space,
        __isl_take isl_union_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_aff

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_to_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_aff_to_multi_pw_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_multi_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_multi_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_to_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_aff_to_multi_union_pw_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_multi_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_multi_val_on_domain_space

```
__isl_give isl_multi_aff *
isl_multi_aff_multi_val_on_domain_space(
        __isl_take isl_space *space,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_multi_aff_on_domain_multi_val

```
__isl_give isl_multi_aff *
isl_space_multi_aff_on_domain_multi_val(
        __isl_take isl_space *space,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_multi_val_on_space

```
__isl_give isl_multi_aff *
isl_multi_aff_multi_val_on_space(
        __isl_take isl_space *space,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_multi_val_on_domain

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_multi_val_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_multi_aff_on_domain

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_multi_aff_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_pw_multi_aff_on_domain

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_pw_multi_aff_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_copy

```
__isl_give isl_multi_id *isl_multi_id_copy(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_free

```
__isl_null isl_multi_id *isl_multi_id_free(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_copy

```
__isl_give isl_multi_val *isl_multi_val_copy(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_free

```
__isl_null isl_multi_val *isl_multi_val_free(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_copy

```
__isl_give isl_multi_aff *isl_multi_aff_copy(
        __isl_keep isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_free

```
__isl_null isl_multi_aff *isl_multi_aff_free(
        __isl_take isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_copy

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_copy(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_free

```
__isl_null isl_multi_pw_aff *isl_multi_pw_aff_free(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_copy

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_copy(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_free

```
__isl_null isl_multi_union_pw_aff *
isl_multi_union_pw_aff_free(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_size

```
int isl_multi_id_size(__isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_size

```
isl_size isl_multi_val_size(__isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_size

```
isl_size isl_multi_aff_size(
        __isl_keep isl_multi_aff *multi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_size

```
isl_size isl_multi_pw_aff_size(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_size

```
isl_size isl_multi_union_pw_aff_size(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_get_at

```
__isl_give isl_id *isl_multi_id_get_at(
        __isl_keep isl_multi_id *mi, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_get_id

```
__isl_give isl_id *isl_multi_id_get_id(
        __isl_keep isl_multi_id *mi, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_at

```
__isl_give isl_val *isl_multi_val_get_at(
        __isl_keep isl_multi_val *mv, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_val

```
__isl_give isl_val *isl_multi_val_get_val(
        __isl_keep isl_multi_val *mv, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_aff

```
__isl_give isl_aff *isl_multi_aff_get_aff(
        __isl_keep isl_multi_aff *multi, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_at

```
__isl_give isl_pw_aff *isl_multi_pw_aff_get_at(
        __isl_keep isl_multi_pw_aff *mpa, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_pw_aff

```
__isl_give isl_pw_aff *isl_multi_pw_aff_get_pw_aff(
        __isl_keep isl_multi_pw_aff *mpa, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_at

```
__isl_give isl_union_pw_aff *
isl_multi_union_pw_aff_get_at(
        __isl_keep isl_multi_union_pw_aff *mupa, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_get_union_pw_aff

```
__isl_give isl_union_pw_aff *
isl_multi_union_pw_aff_get_union_pw_aff(
        __isl_keep isl_multi_union_pw_aff *mupa, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_set_at

```
__isl_give isl_multi_id *isl_multi_id_set_at(
        __isl_take isl_multi_id *mi, int pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_set_id

```
__isl_give isl_multi_id *isl_multi_id_set_id(
        __isl_take isl_multi_id *mi, int pos,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_at

```
__isl_give isl_multi_val *isl_multi_val_set_at(
        __isl_take isl_multi_val *mv, int pos,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_set_val

```
__isl_give isl_multi_val *isl_multi_val_set_val(
        __isl_take isl_multi_val *mv, int pos,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_at

```
__isl_give isl_multi_aff *isl_multi_aff_set_at(
        __isl_take isl_multi_aff *ma, int pos,
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_set_aff

```
__isl_give isl_multi_aff *isl_multi_aff_set_aff(
        __isl_take isl_multi_aff *multi, int pos,
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_at

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_set_at(
        __isl_take isl_multi_pw_aff *mpa, int pos,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_set_pw_aff

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_set_pw_aff(
        __isl_take isl_multi_pw_aff *mpa, int pos,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_at

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_at(
        __isl_take isl_multi_union_pw_aff *mupa, int pos,
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_set_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_set_union_pw_aff(
        __isl_take isl_multi_union_pw_aff *mupa, int pos,
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_get_list

```
__isl_give isl_id_list *isl_multi_id_get_list(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_get_list

```
__isl_give isl_val_list *isl_multi_val_get_list(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_list

```
__isl_give isl_aff_list *isl_multi_aff_get_list(
        __isl_keep isl_multi_aff *multi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_get_list

```
__isl_give isl_pw_aff_list *isl_multi_pw_aff_get_list(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_list

```
__isl_give isl_union_pw_aff_list *
isl_multi_union_pw_aff_list(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_get_constant_multi_val

```
__isl_give isl_multi_val *
isl_multi_aff_get_constant_multi_val(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_extract_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_union_pw_aff_extract_multi_pw_aff(
        __isl_keep isl_multi_union_pw_aff *mupa,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_as_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_union_pw_multi_aff_as_multi_union_pw_aff(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_union_pw_multi_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_union_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_multi_union_pw_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_multi_union_pw_aff(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_empty

```
__isl_give isl_pw_aff *isl_pw_aff_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_empty

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_from_aff

```
__isl_give isl_pw_aff *isl_pw_aff_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_to_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_multi_aff_to_pw_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_from_qpolynomial

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_from_qpolynomial(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_from_qpolynomial_fold

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_from_qpolynomial_fold(
        __isl_take isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_isa_aff

```
isl_bool isl_pw_aff_isa_aff(__isl_keep isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_as_aff

```
__isl_give isl_aff *isl_pw_aff_as_aff(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_isa_multi_aff

```
isl_bool isl_multi_pw_aff_isa_multi_aff(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_as_multi_aff

```
__isl_give isl_multi_aff *isl_multi_pw_aff_as_multi_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_isa_multi_aff

```
isl_bool isl_pw_multi_aff_isa_multi_aff(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_as_multi_aff

```
__isl_give isl_multi_aff *isl_pw_multi_aff_as_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_isa_qpolynomial

```
isl_bool isl_pw_qpolynomial_isa_qpolynomial(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_as_qpolynomial

```
__isl_give isl_qpolynomial *
isl_pw_qpolynomial_as_qpolynomial(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_isa_qpolynomial_fold

```
isl_bool isl_pw_qpolynomial_fold_isa_qpolynomial_fold(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_as_qpolynomial_fold

```
__isl_give isl_qpolynomial_fold *
isl_pw_qpolynomial_fold_as_qpolynomial_fold(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_alloc

```
__isl_give isl_pw_aff *isl_pw_aff_alloc(
        __isl_take isl_set *set, __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_alloc

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_alloc(
        __isl_take isl_set *set,
        __isl_take isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_alloc

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_alloc(
        __isl_take isl_set *set,
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_zero_on_domain

```
__isl_give isl_pw_aff *isl_pw_aff_zero_on_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_var_on_domain

```
__isl_give isl_pw_aff *isl_pw_aff_var_on_domain(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_nan_on_domain_space

```
__isl_give isl_pw_aff *isl_pw_aff_nan_on_domain_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_nan_on_domain

```
__isl_give isl_pw_aff *isl_pw_aff_nan_on_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_zero

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_identity_on_domain_space

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_identity_on_domain_space(
        __isl_take isl_space *space)
__isl_give isl_pw_multi_aff *
isl_space_identity_pw_multi_aff_on_domain(
        __isl_take isl_space *space)
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_identity(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_domain_map

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_domain_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_map_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_space_domain_map_pw_multi_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_range_map

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_range_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_map_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_space_range_map_pw_multi_aff(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_project_out_map

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_project_out_map(
        __isl_take isl_space *space,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_zero

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_val_on_domain

```
__isl_give isl_pw_aff *isl_pw_aff_val_on_domain(
        __isl_take isl_set *domain,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_pw_aff_on_domain_val

```
__isl_give isl_pw_aff *isl_set_pw_aff_on_domain_val(
        __isl_take isl_set *domain,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_multi_val_on_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_multi_val_on_domain(
        __isl_take isl_set *domain,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_pw_multi_aff_on_domain_multi_val

```
__isl_give isl_pw_multi_aff *
isl_set_pw_multi_aff_on_domain_multi_val(
        __isl_take isl_set *domain,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_param_on_domain_id

```
__isl_give isl_pw_aff *isl_pw_aff_param_on_domain_id(
        __isl_take isl_set *domain,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_param_pw_aff_on_domain_id

```
__isl_give isl_pw_aff *isl_set_param_pw_aff_on_domain_id(
        __isl_take isl_set *domain,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_pw_aff

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_from_pw_aff(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_from_pw_aff

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_from_pw_aff(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_copy

```
__isl_give isl_pw_aff *isl_pw_aff_copy(
        __isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_free

```
__isl_null isl_pw_aff *isl_pw_aff_free(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_copy

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_copy(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_free

```
__isl_null isl_pw_multi_aff *isl_pw_multi_aff_free(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_copy

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_copy(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_free

```
__isl_null isl_pw_qpolynomial *isl_pw_qpolynomial_free(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_copy

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_copy(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_free

```
__isl_null isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_free(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_is_empty

```
isl_bool isl_pw_aff_is_empty(__isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_n_piece

```
isl_size isl_pw_aff_n_piece(__isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_foreach_piece

```
isl_stat isl_pw_aff_foreach_piece(
        __isl_keep isl_pw_aff *pwaff,
        isl_stat (*fn)(__isl_take isl_set *set,
                  __isl_take isl_aff *aff,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_every_piece

```
isl_bool isl_pw_aff_every_piece(__isl_keep isl_pw_aff *pa,
        isl_bool (*test)(__isl_keep isl_set *set,
                __isl_keep isl_aff *aff, void *user),
                void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_n_piece

```
isl_size isl_pw_multi_aff_n_piece(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_foreach_piece

```
isl_stat isl_pw_multi_aff_foreach_piece(
        __isl_keep isl_pw_multi_aff *pma,
        isl_stat (*fn)(__isl_take isl_set *set,
                    __isl_take isl_multi_aff *maff,
                    void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_every_piece

```
isl_bool isl_pw_multi_aff_every_piece(
        __isl_keep isl_pw_multi_aff *pma,
        isl_bool (*test)(__isl_keep isl_set *set,
                __isl_keep isl_multi_aff *ma, void *user),
                void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_n_piece

```
isl_size isl_pw_qpolynomial_n_piece(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_foreach_piece

```
isl_stat isl_pw_qpolynomial_foreach_piece(
        __isl_keep isl_pw_qpolynomial *pwqp,
        isl_stat (*fn)(__isl_take isl_set *set,
                  __isl_take isl_qpolynomial *qp,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_every_piece

```
isl_bool isl_pw_qpolynomial_every_piece(
        __isl_keep isl_pw_qpolynomial *pwqp,
        isl_bool (*test)(__isl_keep isl_set *set,
                __isl_keep isl_qpolynomial *qp,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_foreach_lifted_piece

```
isl_stat isl_pw_qpolynomial_foreach_lifted_piece(
        __isl_keep isl_pw_qpolynomial *pwqp,
        isl_stat (*fn)(__isl_take isl_set *set,
                  __isl_take isl_qpolynomial *qp,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_n_piece

```
isl_size isl_pw_qpolynomial_fold_n_piece(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_foreach_piece

```
isl_stat isl_pw_qpolynomial_fold_foreach_piece(
        __isl_keep isl_pw_qpolynomial_fold *pwf,
        isl_stat (*fn)(__isl_take isl_set *set,
                  __isl_take isl_qpolynomial_fold *fold,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_every_piece

```
isl_bool isl_pw_qpolynomial_fold_every_piece(
        __isl_keep isl_pw_qpolynomial_fold *pwf,
        isl_bool (*test)(__isl_keep isl_set *set,
                __isl_keep isl_qpolynomial_fold *fold,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_foreach_lifted_piece

```
isl_stat isl_pw_qpolynomial_fold_foreach_lifted_piece(
        __isl_keep isl_pw_qpolynomial_fold *pwf,
        isl_stat (*fn)(__isl_take isl_set *set,
                  __isl_take isl_qpolynomial_fold *fold,
                  void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_at

```
__isl_give isl_pw_aff *isl_pw_multi_aff_get_at(
        __isl_keep isl_pw_multi_aff *pma, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_get_pw_aff

```
__isl_give isl_pw_aff *isl_pw_multi_aff_get_pw_aff(
        __isl_keep isl_pw_multi_aff *pma, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_set_pw_aff

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_set_pw_aff(
        __isl_take isl_pw_multi_aff *pma, unsigned pos,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_multi_pw_aff

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_from_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_to_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_pw_multi_aff_to_multi_pw_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_pw_multi_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_from_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_empty_ctx

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_empty_ctx(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_empty_space

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_empty_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_empty

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_empty_ctx

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_empty_ctx(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_empty_space

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_empty_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_empty

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_zero_ctx

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_zero_ctx(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_zero_space

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_zero_space(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_zero

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_zero(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_to_union_pw_aff

```
__isl_give isl_union_pw_aff *
isl_pw_aff_to_union_pw_aff(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_from_pw_aff

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_from_pw_aff(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_to_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_pw_multi_aff_to_union_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_to_union_pw_qpolynomial

```
__isl_give isl_union_pw_qpolynomial *
isl_pw_qpolynomial_to_union_pw_qpolynomial(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_from_pw_qpolynomial

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_from_pw_qpolynomial(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_to_union_pw_qpolynomial_fold

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_to_union_pw_qpolynomial_fold(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_isa_pw_multi_aff

```
isl_bool isl_union_pw_multi_aff_isa_pw_multi_aff(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_as_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_union_pw_multi_aff_as_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_from_aff

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_from_aff(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_multi_aff(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_union_pw_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_union_pw_aff(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_get_union_pw_aff

```
__isl_give isl_union_pw_aff *
isl_union_pw_multi_aff_get_union_pw_aff(
        __isl_keep isl_union_pw_multi_aff *upma, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_val_on_domain

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_val_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_multi_val_on_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_multi_val_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_param_on_domain_id

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_param_on_domain_id(
        __isl_take isl_union_set *domain,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_aff_on_domain

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_aff_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_pw_aff_on_domain

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_pw_aff_on_domain(
        __isl_take isl_union_set *domain,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_add_pw_aff

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_add_pw_aff(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_add_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_add_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_add_pw_qpolynomial

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_add_pw_qpolynomial(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_copy

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_copy(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_free

```
__isl_null isl_union_pw_aff *isl_union_pw_aff_free(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_copy

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_copy(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_free

```
__isl_null isl_union_pw_multi_aff *
isl_union_pw_multi_aff_free(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_copy

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_copy(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_free

```
__isl_null isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_free(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_copy

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_copy(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_free

```
__isl_null isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_free(
        __isl_take isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_n_pw_aff

```
isl_size isl_union_pw_aff_n_pw_aff(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_foreach_pw_aff

```
isl_stat isl_union_pw_aff_foreach_pw_aff(
        __isl_keep isl_union_pw_aff *upa,
        isl_stat (*fn)(__isl_take isl_pw_aff *pa,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_every_pw_aff

```
isl_bool isl_union_pw_aff_every_pw_aff(
        __isl_keep isl_union_pw_aff *upa,
        isl_bool (*test)(__isl_keep isl_pw_aff *pa,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_n_pw_multi_aff

```
isl_size isl_union_pw_multi_aff_n_pw_multi_aff(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_foreach_pw_multi_aff

```
isl_stat isl_union_pw_multi_aff_foreach_pw_multi_aff(
        __isl_keep isl_union_pw_multi_aff *upma,
        isl_stat (*fn)(__isl_take isl_pw_multi_aff *pma,
                    void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_every_pw_multi_aff

```
isl_bool isl_union_pw_multi_aff_every_pw_multi_aff(
        __isl_keep isl_union_pw_multi_aff *upma,
        isl_bool (*test)(
                __isl_keep isl_pw_multi_aff *pma,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_n_pw_qpolynomial

```
isl_size isl_union_pw_qpolynomial_n_pw_qpolynomial(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_foreach_pw_qpolynomial

```
isl_stat isl_union_pw_qpolynomial_foreach_pw_qpolynomial(
        __isl_keep isl_union_pw_qpolynomial *upwqp,
        isl_stat (*fn)(__isl_take isl_pw_qpolynomial *pwqp,
                    void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_every_pw_qpolynomial

```
isl_bool isl_union_pw_qpolynomial_every_pw_qpolynomial(
        __isl_keep isl_union_pw_qpolynomial *upwqp,
        isl_bool (*test)(
                __isl_keep isl_pw_qpolynomial *pwqp,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_n_pw_qpolynomial_fold

```
isl_size isl_union_pw_qpolynomial_fold_n_pw_qpolynomial_fold(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold

```
isl_stat isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf,
        isl_stat (*fn)(__isl_take isl_pw_qpolynomial_fold *pwf,
                    void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_every_pw_qpolynomial_fold

```
isl_bool
isl_union_pw_qpolynomial_fold_every_pw_qpolynomial_fold(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf,
        isl_bool (*test)(
                __isl_keep isl_pw_qpolynomial_fold *pwf,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_extract_pw_aff

```
__isl_give isl_pw_aff *isl_union_pw_aff_extract_pw_aff(
        __isl_keep isl_union_pw_aff *upa,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_extract_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_union_pw_multi_aff_extract_pw_multi_aff(
        __isl_keep isl_union_pw_multi_aff *upma,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_extract_pw_qpolynomial

```
__isl_give isl_pw_qpolynomial *
isl_union_pw_qpolynomial_extract_pw_qpolynomial(
        __isl_keep isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_get_pw_aff_list

```
__isl_give isl_pw_aff_list *
isl_union_pw_aff_get_pw_aff_list(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_get_pw_multi_aff_list

```
__isl_give isl_pw_multi_aff_list *
isl_union_pw_multi_aff_get_pw_multi_aff_list(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_get_pw_qpolynomial_list

```
__isl_give isl_pw_qpolynomial_list *
isl_union_pw_qpolynomial_get_pw_qpolynomial_list(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_get_pw_qpolynomial_fold_list

```
__isl_give isl_pw_qpolynomial_fold_list *
isl_union_pw_qpolynomial_fold_get_pw_qpolynomial_fold_list(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_read_from_str

```
__isl_give isl_id *isl_id_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_read_from_str

```
__isl_give isl_multi_id *isl_multi_id_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_read_from_str

```
__isl_give isl_val *isl_val_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_read_from_str

```
__isl_give isl_multi_val *isl_multi_val_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_read_from_str

```
__isl_give isl_space *isl_space_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_read_from_file

```
__isl_give isl_basic_set *isl_basic_set_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_read_from_file

```
__isl_give isl_set *isl_set_read_from_file(isl_ctx *ctx,
        FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_read_from_str

```
__isl_give isl_set *isl_set_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_read_from_file

```
__isl_give isl_basic_map *isl_basic_map_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_read_from_str

```
__isl_give isl_basic_map *isl_basic_map_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_read_from_file

```
__isl_give isl_map *isl_map_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_read_from_str

```
__isl_give isl_map *isl_map_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_read_from_file

```
__isl_give isl_union_set *isl_union_set_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_read_from_str

```
__isl_give isl_union_set *isl_union_set_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_read_from_file

```
__isl_give isl_union_map *isl_union_map_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_read_from_str

```
__isl_give isl_union_map *isl_union_map_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_read_from_str

```
__isl_give isl_aff *isl_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_read_from_str

```
__isl_give isl_multi_aff *isl_multi_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_read_from_str

```
__isl_give isl_pw_aff *isl_pw_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_read_from_str

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_read_from_str

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_read_from_str

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_read_from_str

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_read_from_str

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_read_from_str

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_read_from_str

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_to_file

```
__isl_give isl_printer *isl_printer_to_file(isl_ctx *ctx,
        FILE *file);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_to_str

```
__isl_give isl_printer *isl_printer_to_str(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_free

```
__isl_null isl_printer *isl_printer_free(
        __isl_take isl_printer *printer);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_get_str

```
__isl_give char *isl_printer_get_str(
        __isl_keep isl_printer *printer);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_get_file

```
FILE *isl_printer_get_file(
        __isl_keep isl_printer *printer);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_get_output_format

```
int isl_printer_get_output_format(
        __isl_keep isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_get_yaml_style

```
int isl_printer_get_yaml_style(__isl_keep isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_output_format

```
__isl_give isl_printer *isl_printer_set_output_format(
        __isl_take isl_printer *p, int output_format);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_indent

```
__isl_give isl_printer *isl_printer_set_indent(
        __isl_take isl_printer *p, int indent);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_indent_prefix

```
__isl_give isl_printer *isl_printer_set_indent_prefix(
        __isl_take isl_printer *p, const char *prefix);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_indent

```
__isl_give isl_printer *isl_printer_indent(
        __isl_take isl_printer *p, int indent);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_prefix

```
__isl_give isl_printer *isl_printer_set_prefix(
        __isl_take isl_printer *p, const char *prefix);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_suffix

```
__isl_give isl_printer *isl_printer_set_suffix(
        __isl_take isl_printer *p, const char *suffix);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_yaml_style

```
__isl_give isl_printer *isl_printer_set_yaml_style(
        __isl_take isl_printer *p, int yaml_style);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_double

```
__isl_give isl_printer *isl_printer_print_double(
        __isl_take isl_printer *p, double d);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_val

```
__isl_give isl_printer *isl_printer_print_val(
        __isl_take isl_printer *p, __isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_multi_val

```
__isl_give isl_printer *isl_printer_print_multi_val(
        __isl_take isl_printer *p,
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_basic_set

```
__isl_give isl_printer *isl_printer_print_basic_set(
        __isl_take isl_printer *printer,
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_set

```
__isl_give isl_printer *isl_printer_print_set(
        __isl_take isl_printer *printer,
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_basic_map

```
__isl_give isl_printer *isl_printer_print_basic_map(
        __isl_take isl_printer *printer,
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_map

```
__isl_give isl_printer *isl_printer_print_map(
        __isl_take isl_printer *printer,
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_set

```
__isl_give isl_printer *isl_printer_print_union_set(
        __isl_take isl_printer *p,
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_map

```
__isl_give isl_printer *isl_printer_print_union_map(
        __isl_take isl_printer *p,
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_multi_id

```
__isl_give isl_printer *isl_printer_print_multi_id(
        __isl_take isl_printer *p,
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_aff

```
__isl_give isl_printer *isl_printer_print_aff(
        __isl_take isl_printer *p, __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_multi_aff

```
__isl_give isl_printer *isl_printer_print_multi_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_multi_aff *maff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_pw_aff

```
__isl_give isl_printer *isl_printer_print_pw_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_pw_multi_aff

```
__isl_give isl_printer *isl_printer_print_pw_multi_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_multi_pw_aff

```
__isl_give isl_printer *isl_printer_print_multi_pw_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_pw_aff

```
__isl_give isl_printer *isl_printer_print_union_pw_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_pw_multi_aff

```
__isl_give isl_printer *isl_printer_print_union_pw_multi_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_multi_union_pw_aff

```
__isl_give isl_printer *
isl_printer_print_multi_union_pw_aff(
        __isl_take isl_printer *p,
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_qpolynomial

```
__isl_give isl_printer *isl_printer_print_qpolynomial(
        __isl_take isl_printer *p,
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_pw_qpolynomial

```
__isl_give isl_printer *isl_printer_print_pw_qpolynomial(
        __isl_take isl_printer *p,
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_pw_qpolynomial

```
__isl_give isl_printer *isl_printer_print_union_pw_qpolynomial(
        __isl_take isl_printer *p,
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_pw_qpolynomial_fold

```
__isl_give isl_printer *
isl_printer_print_pw_qpolynomial_fold(
        __isl_take isl_printer *p,
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_pw_qpolynomial_fold

```
__isl_give isl_printer *
isl_printer_print_union_pw_qpolynomial_fold(
        __isl_take isl_printer *p,
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_yaml_start_mapping

```
__isl_give isl_printer *isl_printer_yaml_start_mapping(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_yaml_end_mapping

```
__isl_give isl_printer *isl_printer_yaml_end_mapping(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_yaml_start_sequence

```
__isl_give isl_printer *isl_printer_yaml_start_sequence(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_yaml_end_sequence

```
__isl_give isl_printer *isl_printer_yaml_end_sequence(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_yaml_next

```
__isl_give isl_printer *isl_printer_yaml_next(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_flush

```
__isl_give isl_printer *isl_printer_flush(
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_has_note

```
isl_bool isl_printer_has_note(__isl_keep isl_printer *p,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_get_note

```
__isl_give isl_id *isl_printer_get_note(
        __isl_keep isl_printer *p, __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_set_note

```
__isl_give isl_printer *isl_printer_set_note(
        __isl_take isl_printer *p,
        __isl_take isl_id *id, __isl_take isl_id *note);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_str

```
__isl_give char *isl_id_to_str(
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_to_str

```
__isl_give char *isl_multi_id_to_str(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_to_str

```
__isl_give char *isl_space_to_str(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_to_str

```
__isl_give char *isl_val_to_str(__isl_keep isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_to_str

```
__isl_give char *isl_multi_val_to_str(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_to_str

```
__isl_give char *isl_basic_set_to_str(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_to_str

```
__isl_give char *isl_set_to_str(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_to_str

```
__isl_give char *isl_union_set_to_str(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_to_str

```
__isl_give char *isl_basic_map_to_str(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_to_str

```
__isl_give char *isl_map_to_str(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_to_str

```
__isl_give char *isl_union_map_to_str(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_to_str

```
__isl_give char *isl_aff_to_str(__isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_to_str

```
__isl_give char *isl_pw_aff_to_str(
        __isl_keep isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_to_str

```
__isl_give char *isl_multi_aff_to_str(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_to_str

```
__isl_give char *isl_pw_multi_aff_to_str(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_to_str

```
__isl_give char *isl_multi_pw_aff_to_str(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_to_str

```
__isl_give char *isl_union_pw_aff_to_str(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_to_str

```
__isl_give char *isl_union_pw_multi_aff_to_str(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_to_str

```
__isl_give char *isl_multi_union_pw_aff_to_str(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_point_to_str

```
__isl_give char *isl_point_to_str(
        __isl_keep isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_to_str

```
__isl_give char *isl_pw_qpolynomial_to_str(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_to_str

```
__isl_give char *isl_union_pw_qpolynomial_to_str(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_plain_is_empty

```
isl_bool isl_basic_set_plain_is_empty(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_empty

```
isl_bool isl_basic_set_is_empty(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_is_empty

```
isl_bool isl_set_plain_is_empty(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_empty

```
isl_bool isl_set_is_empty(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_empty

```
isl_bool isl_union_set_is_empty(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_plain_is_empty

```
isl_bool isl_basic_map_plain_is_empty(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_empty

```
isl_bool isl_basic_map_is_empty(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_is_empty

```
isl_bool isl_map_plain_is_empty(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_empty

```
isl_bool isl_map_is_empty(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_plain_is_empty

```
isl_bool isl_union_map_plain_is_empty(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_empty

```
isl_bool isl_union_map_is_empty(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_plain_is_empty

```
isl_bool isl_union_pw_multi_aff_plain_is_empty(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_plain_is_universe

```
isl_bool isl_basic_set_plain_is_universe(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_universe

```
isl_bool isl_basic_set_is_universe(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_plain_is_universe

```
isl_bool isl_basic_map_plain_is_universe(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_universe

```
isl_bool isl_basic_map_is_universe(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_is_universe

```
isl_bool isl_set_plain_is_universe(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_is_universe

```
isl_bool isl_map_plain_is_universe(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_singleton

```
isl_bool isl_set_is_singleton(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_single_valued

```
isl_bool isl_basic_map_is_single_valued(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_is_single_valued

```
isl_bool isl_map_plain_is_single_valued(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_single_valued

```
isl_bool isl_map_is_single_valued(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_single_valued

```
isl_bool isl_union_map_is_single_valued(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_is_injective

```
isl_bool isl_map_plain_is_injective(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_injective

```
isl_bool isl_map_is_injective(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_plain_is_injective

```
isl_bool isl_union_map_plain_is_injective(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_injective

```
isl_bool isl_union_map_is_injective(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_bijective

```
isl_bool isl_map_is_bijective(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_bijective

```
isl_bool isl_union_map_is_bijective(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_identity

```
isl_bool isl_map_is_identity(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_identity

```
isl_bool isl_union_map_is_identity(
        __isl_keep isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_plain_get_val_if_fixed

```
__isl_give isl_val *
isl_basic_map_plain_get_val_if_fixed(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_get_val_if_fixed

```
__isl_give isl_val *isl_set_plain_get_val_if_fixed(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_plain_multi_val_if_fixed

```
__isl_give isl_multi_val *
isl_set_get_plain_multi_val_if_fixed(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_get_val_if_fixed

```
__isl_give isl_val *isl_map_plain_get_val_if_fixed(
        __isl_keep isl_map *map,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_residue_class_val

```
isl_stat isl_set_dim_residue_class_val(
        __isl_keep isl_set *set,
        int pos, __isl_give isl_val **modulo,
        __isl_give isl_val **residue);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_stride_info

```
__isl_give isl_stride_info *isl_set_get_stride_info(
        __isl_keep isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_stride

```
__isl_give isl_val *isl_set_get_stride(
        __isl_keep isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_lattice_tile

```
__isl_give isl_fixed_box *isl_set_get_lattice_tile(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_range_stride_info

```
__isl_give isl_stride_info *
isl_map_get_range_stride_info(
        __isl_keep isl_map *map, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_get_range_lattice_tile

```
__isl_give isl_fixed_box *
isl_map_get_range_lattice_tile(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stride_info_get_stride

```
__isl_give isl_val *isl_stride_info_get_stride(
        __isl_keep isl_stride_info *si);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stride_info_get_offset

```
__isl_give isl_aff *isl_stride_info_get_offset(
        __isl_keep isl_stride_info *si);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stride_info_copy

```
__isl_give isl_stride_info *isl_stride_info_copy(
        __isl_keep isl_stride_info *si);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_stride_info_free

```
__isl_null isl_stride_info *isl_stride_info_free(
        __isl_take isl_stride_info *si);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_involves_locals

```
isl_bool isl_set_involves_locals(
        __isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_involves_locals

```
isl_bool isl_aff_involves_locals(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_involves_locals

```
isl_bool isl_multi_aff_involves_locals(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_involves_locals

```
isl_bool isl_pw_multi_aff_involves_locals(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_involves_locals

```
isl_bool isl_union_pw_multi_aff_involves_locals(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_involves_dims

```
isl_bool isl_constraint_involves_dims(
        __isl_keep isl_constraint *constraint,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_involves_dims

```
isl_bool isl_basic_set_involves_dims(
        __isl_keep isl_basic_set *bset,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_involves_dims

```
isl_bool isl_set_involves_dims(__isl_keep isl_set *set,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_involves_dims

```
isl_bool isl_basic_map_involves_dims(
        __isl_keep isl_basic_map *bmap,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_involves_dims

```
isl_bool isl_map_involves_dims(__isl_keep isl_map *map,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_involves_dims

```
isl_bool isl_union_map_involves_dims(
        __isl_keep isl_union_map *umap,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_involves_dims

```
isl_bool isl_aff_involves_dims(__isl_keep isl_aff *aff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_involves_param_id

```
isl_bool isl_pw_aff_involves_param_id(
        __isl_keep isl_pw_aff *pa,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_involves_dims

```
isl_bool isl_pw_aff_involves_dims(
        __isl_keep isl_pw_aff *pwaff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_involves_dims

```
isl_bool isl_multi_aff_involves_dims(
        __isl_keep isl_multi_aff *ma,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_involves_param_id

```
isl_bool isl_pw_multi_aff_involves_param_id(
        __isl_keep isl_pw_multi_aff *pma,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_involves_dims

```
isl_bool isl_pw_multi_aff_involves_dims(
        __isl_keep isl_pw_multi_aff *pma,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_involves_dims

```
isl_bool isl_multi_pw_aff_involves_dims(
        __isl_keep isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_involves_param_id

```
isl_bool isl_multi_pw_aff_involves_param_id(
        __isl_keep isl_multi_pw_aff *mpa,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_involves_param_id_list

```
isl_bool isl_multi_pw_aff_involves_param_id_list(
        __isl_keep isl_multi_pw_aff *mpa,
        __isl_keep isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_involves_dims

```
isl_bool isl_qpolynomial_involves_dims(
        __isl_keep isl_qpolynomial *qp,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_involves_param_id

```
isl_bool isl_pw_qpolynomial_involves_param_id(
        __isl_keep isl_pw_qpolynomial *pwqp,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_involves_param_id

```
isl_bool isl_pw_qpolynomial_fold_involves_param_id(
        __isl_keep isl_pw_qpolynomial_fold *pwf,
        __isl_keep isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_has_any_lower_bound

```
isl_bool isl_set_dim_has_any_lower_bound(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_has_any_upper_bound

```
isl_bool isl_set_dim_has_any_upper_bound(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_has_lower_bound

```
isl_bool isl_set_dim_has_lower_bound(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_has_upper_bound

```
isl_bool isl_set_dim_has_upper_bound(
        __isl_keep isl_set *set,
        enum isl_dim_type type, unsigned pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_params

```
isl_bool isl_set_is_params(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_params

```
isl_bool isl_union_set_is_params(
        __isl_keep isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_wrapping

```
isl_bool isl_space_is_wrapping(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_is_wrapping

```
isl_bool isl_space_domain_is_wrapping(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_is_wrapping

```
isl_bool isl_space_range_is_wrapping(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_is_product

```
isl_bool isl_space_is_product(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_wrapping

```
isl_bool isl_basic_set_is_wrapping(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_wrapping

```
isl_bool isl_set_is_wrapping(__isl_keep isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_is_wrapping

```
isl_bool isl_map_domain_is_wrapping(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_is_wrapping

```
isl_bool isl_map_range_is_wrapping(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_product

```
isl_bool isl_map_is_product(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_range_is_wrapping

```
isl_bool isl_multi_id_range_is_wrapping(
        __isl_keep isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_range_is_wrapping

```
isl_bool isl_multi_val_range_is_wrapping(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_is_wrapping

```
isl_bool isl_multi_aff_range_is_wrapping(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_range_is_wrapping

```
isl_bool isl_multi_pw_aff_range_is_wrapping(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_range_is_wrapping

```
isl_bool isl_multi_union_pw_aff_range_is_wrapping(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_can_zip

```
isl_bool isl_basic_map_can_zip(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_can_zip

```
isl_bool isl_map_can_zip(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_can_curry

```
isl_bool isl_space_can_curry(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_can_curry

```
isl_bool isl_basic_map_can_curry(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_can_curry

```
isl_bool isl_map_can_curry(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_can_uncurry

```
isl_bool isl_space_can_uncurry(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_can_uncurry

```
isl_bool isl_basic_map_can_uncurry(
        __isl_keep isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_can_uncurry

```
isl_bool isl_map_can_uncurry(__isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_can_range_curry

```
isl_bool isl_space_can_range_curry(
        __isl_keep isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_can_range_curry

```
isl_bool isl_map_can_range_curry(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_is_cst

```
isl_bool isl_aff_is_cst(__isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_is_cst

```
isl_bool isl_pw_aff_is_cst(__isl_keep isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_is_cst

```
isl_bool isl_multi_pw_aff_is_cst(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_involves_nan

```
isl_bool isl_multi_val_involves_nan(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_is_nan

```
isl_bool isl_aff_is_nan(__isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_involves_nan

```
isl_bool isl_multi_aff_involves_nan(
        __isl_keep isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_involves_nan

```
isl_bool isl_pw_aff_involves_nan(
        __isl_keep isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_involves_nan

```
isl_bool isl_pw_multi_aff_involves_nan(
        __isl_keep isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_involves_nan

```
isl_bool isl_multi_pw_aff_involves_nan(
        __isl_keep isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_involves_nan

```
isl_bool isl_union_pw_aff_involves_nan(
        __isl_keep isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_involves_nan

```
isl_bool isl_union_pw_multi_aff_involves_nan(
        __isl_keep isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_involves_nan

```
isl_bool isl_multi_union_pw_aff_involves_nan(
        __isl_keep isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_is_nan

```
isl_bool isl_qpolynomial_is_nan(
        __isl_keep isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_is_nan

```
isl_bool isl_qpolynomial_fold_is_nan(
        __isl_keep isl_qpolynomial_fold *fold);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_involves_nan

```
isl_bool isl_pw_qpolynomial_involves_nan(
        __isl_keep isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_involves_nan

```
isl_bool isl_pw_qpolynomial_fold_involves_nan(
        __isl_keep isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_involves_nan

```
isl_bool isl_union_pw_qpolynomial_involves_nan(
        __isl_keep isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_involves_nan

```
isl_bool isl_union_pw_qpolynomial_fold_involves_nan(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_is_zero

```
isl_bool isl_multi_val_is_zero(
        __isl_keep isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_plain_is_zero

```
isl_bool isl_aff_plain_is_zero(
        __isl_keep isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_plain_is_equal

```
isl_bool isl_basic_set_plain_is_equal(
        __isl_keep isl_basic_set *bset1,
        __isl_keep isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_equal

```
isl_bool isl_basic_set_is_equal(
        __isl_keep isl_basic_set *bset1,
        __isl_keep isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_is_equal

```
isl_bool isl_set_plain_is_equal(
        __isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_equal

```
isl_bool isl_set_is_equal(__isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_equal

```
isl_bool isl_basic_map_is_equal(
        __isl_keep isl_basic_map *bmap1,
        __isl_keep isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_equal

```
isl_bool isl_map_is_equal(__isl_keep isl_map *map1,
        __isl_keep isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_is_equal

```
isl_bool isl_map_plain_is_equal(
        __isl_keep isl_map *map1,
        __isl_keep isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_equal

```
isl_bool isl_union_set_is_equal(
        __isl_keep isl_union_set *uset1,
        __isl_keep isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_equal

```
isl_bool isl_union_map_is_equal(
        __isl_keep isl_union_map *umap1,
        __isl_keep isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_plain_is_equal

```
isl_bool isl_multi_id_plain_is_equal(
        __isl_keep isl_multi_id *mi1,
        __isl_keep isl_multi_id *mi2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_plain_is_equal

```
isl_bool isl_multi_val_plain_is_equal(
        __isl_keep isl_multi_val *mv1,
        __isl_keep isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_plain_is_equal

```
isl_bool isl_aff_plain_is_equal(
        __isl_keep isl_aff *aff1,
        __isl_keep isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_plain_is_equal

```
isl_bool isl_multi_aff_plain_is_equal(
        __isl_keep isl_multi_aff *maff1,
        __isl_keep isl_multi_aff *maff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_plain_is_equal

```
isl_bool isl_pw_aff_plain_is_equal(
        __isl_keep isl_pw_aff *pwaff1,
        __isl_keep isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_is_equal

```
isl_bool isl_pw_aff_is_equal(
        __isl_keep isl_pw_aff *pa1,
        __isl_keep isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_plain_is_equal

```
isl_bool isl_pw_multi_aff_plain_is_equal(
        __isl_keep isl_pw_multi_aff *pma1,
        __isl_keep isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_is_equal

```
isl_bool isl_pw_multi_aff_is_equal(
        __isl_keep isl_pw_multi_aff *pma1,
        __isl_keep isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_plain_is_equal

```
isl_bool isl_multi_pw_aff_plain_is_equal(
        __isl_keep isl_multi_pw_aff *mpa1,
        __isl_keep isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_is_equal

```
isl_bool isl_multi_pw_aff_is_equal(
        __isl_keep isl_multi_pw_aff *mpa1,
        __isl_keep isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_plain_is_equal

```
isl_bool isl_union_pw_aff_plain_is_equal(
        __isl_keep isl_union_pw_aff *upa1,
        __isl_keep isl_union_pw_aff *upa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_plain_is_equal

```
isl_bool isl_union_pw_multi_aff_plain_is_equal(
        __isl_keep isl_union_pw_multi_aff *upma1,
        __isl_keep isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_plain_is_equal

```
isl_bool isl_multi_union_pw_aff_plain_is_equal(
        __isl_keep isl_multi_union_pw_aff *mupa1,
        __isl_keep isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_plain_is_equal

```
isl_bool isl_union_pw_qpolynomial_plain_is_equal(
        __isl_keep isl_union_pw_qpolynomial *upwqp1,
        __isl_keep isl_union_pw_qpolynomial *upwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_plain_is_equal

```
isl_bool isl_union_pw_qpolynomial_fold_plain_is_equal(
        __isl_keep isl_union_pw_qpolynomial_fold *upwf1,
        __isl_keep isl_union_pw_qpolynomial_fold *upwf2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_disjoint

```
isl_bool isl_basic_set_is_disjoint(
        __isl_keep isl_basic_set *bset1,
        __isl_keep isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_is_disjoint

```
isl_bool isl_set_plain_is_disjoint(
        __isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_disjoint

```
isl_bool isl_set_is_disjoint(__isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_disjoint

```
isl_bool isl_basic_map_is_disjoint(
        __isl_keep isl_basic_map *bmap1,
        __isl_keep isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_disjoint

```
isl_bool isl_map_is_disjoint(__isl_keep isl_map *map1,
        __isl_keep isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_disjoint

```
isl_bool isl_union_set_is_disjoint(
        __isl_keep isl_union_set *uset1,
        __isl_keep isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_disjoint

```
isl_bool isl_union_map_is_disjoint(
        __isl_keep isl_union_map *umap1,
        __isl_keep isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_is_subset

```
isl_bool isl_basic_set_is_subset(
        __isl_keep isl_basic_set *bset1,
        __isl_keep isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_subset

```
isl_bool isl_set_is_subset(__isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_is_strict_subset

```
isl_bool isl_set_is_strict_subset(
        __isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_subset

```
isl_bool isl_union_set_is_subset(
        __isl_keep isl_union_set *uset1,
        __isl_keep isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_is_strict_subset

```
isl_bool isl_union_set_is_strict_subset(
        __isl_keep isl_union_set *uset1,
        __isl_keep isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_subset

```
isl_bool isl_basic_map_is_subset(
        __isl_keep isl_basic_map *bmap1,
        __isl_keep isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_is_strict_subset

```
isl_bool isl_basic_map_is_strict_subset(
        __isl_keep isl_basic_map *bmap1,
        __isl_keep isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_subset

```
isl_bool isl_map_is_subset(
        __isl_keep isl_map *map1,
        __isl_keep isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_is_strict_subset

```
isl_bool isl_map_is_strict_subset(
        __isl_keep isl_map *map1,
        __isl_keep isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_subset

```
isl_bool isl_union_map_is_subset(
        __isl_keep isl_union_map *umap1,
        __isl_keep isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_is_strict_subset

```
isl_bool isl_union_map_is_strict_subset(
        __isl_keep isl_union_map *umap1,
        __isl_keep isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_plain_cmp

```
int isl_constraint_plain_cmp(
        __isl_keep isl_constraint *c1,
        __isl_keep isl_constraint *c2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_constraint_cmp_last_non_zero

```
int isl_constraint_cmp_last_non_zero(
        __isl_keep isl_constraint *c1,
        __isl_keep isl_constraint *c2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_cmp

```
int isl_set_plain_cmp(__isl_keep isl_set *set1,
        __isl_keep isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_plain_cmp

```
int isl_multi_aff_plain_cmp(
        __isl_keep isl_multi_aff *ma1,
        __isl_keep isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_plain_cmp

```
int isl_pw_aff_plain_cmp(__isl_keep isl_pw_aff *pa1,
        __isl_keep isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_complement

```
__isl_give isl_set *isl_set_complement(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_complement

```
__isl_give isl_map *isl_map_complement(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_reverse

```
__isl_give isl_space *isl_space_reverse(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_wrapped_reverse

```
__isl_give isl_space *isl_space_wrapped_reverse(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_reverse

```
__isl_give isl_space *isl_space_domain_reverse(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_reverse

```
__isl_give isl_space *isl_space_range_reverse(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_domain_reverse

```
__isl_give isl_aff *isl_aff_domain_reverse(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_domain_reverse

```
__isl_give isl_multi_aff *
isl_multi_aff_domain_reverse(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_domain_reverse

```
__isl_give isl_pw_aff *isl_pw_aff_domain_reverse(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_domain_reverse

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_domain_reverse(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_domain_reverse

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_domain_reverse(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_wrapped_reverse

```
__isl_give isl_set *isl_set_wrapped_reverse(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_reverse

```
__isl_give isl_basic_map *isl_basic_map_reverse(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_reverse

```
__isl_give isl_map *isl_map_reverse(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_reverse

```
__isl_give isl_map *isl_map_domain_reverse(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_reverse

```
__isl_give isl_map *isl_map_range_reverse(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_reverse

```
__isl_give isl_union_map *isl_union_map_reverse(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_reverse

```
__isl_give isl_union_map *isl_union_map_domain_reverse(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_reverse

```
__isl_give isl_union_map *isl_union_map_range_reverse(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_domain_reverse

```
__isl_give isl_qpolynomial *
isl_qpolynomial_domain_reverse(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_domain_reverse

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_domain_reverse(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_domain_reverse

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_domain_reverse(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_bind

```
__isl_give isl_set *isl_set_bind(
        __isl_take isl_set *set,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_bind_domain

```
__isl_give isl_set *isl_map_bind_domain(
        __isl_take isl_map *map,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_bind_range

```
__isl_give isl_set *isl_map_bind_range(
        __isl_take isl_map *map,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_bind_range

```
__isl_give isl_union_set *isl_union_map_bind_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_bind_domain

```
__isl_give isl_pw_aff *isl_pw_aff_bind_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_bind_domain

```
__isl_give isl_multi_aff *isl_multi_aff_bind_domain(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_bind_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_bind_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_bind_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_bind_domain(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_bind_domain_wrapped_domain

```
__isl_give isl_pw_aff *
isl_pw_aff_bind_domain_wrapped_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_bind_domain_wrapped_domain

```
__isl_give isl_multi_aff *
isl_multi_aff_bind_domain_wrapped_domain(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_bind_domain_wrapped_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_bind_domain_wrapped_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_bind_domain_wrapped_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_bind_domain_wrapped_domain(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_bind_id

```
__isl_give isl_basic_set *isl_aff_bind_id(
        __isl_take isl_aff *aff,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_bind_id

```
__isl_give isl_set *isl_pw_aff_bind_id(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_bind

```
__isl_give isl_basic_set *isl_multi_aff_bind(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_bind

```
__isl_give isl_set *isl_multi_pw_aff_bind(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_bind_id

```
__isl_give isl_union_set *isl_union_pw_aff_bind_id(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_bind

```
__isl_give isl_union_set *
isl_multi_union_pw_aff_bind(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain

```
__isl_give isl_space *isl_space_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range

```
__isl_give isl_space *isl_space_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_params

```
__isl_give isl_space *isl_space_params(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_wrapped_domain

```
__isl_give isl_space *
isl_space_domain_wrapped_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_wrapped_range

```
__isl_give isl_space *
isl_space_domain_wrapped_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_wrapped_domain

```
__isl_give isl_space *
isl_space_range_wrapped_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_wrapped_range

```
__isl_give isl_space *
isl_space_range_wrapped_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_domain

```
__isl_give isl_local_space *isl_local_space_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_range

```
__isl_give isl_local_space *isl_local_space_range(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_project_out_param_id

```
__isl_give isl_set *isl_set_project_out_param_id(
        __isl_take isl_set *set,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_project_out_param_id_list

```
__isl_give isl_set *
isl_set_project_out_param_id_list(
        __isl_take isl_set *set,
        __isl_take isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_project_out

```
__isl_give isl_set *isl_set_project_out(__isl_take isl_set *set,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_project_out_all_params

```
__isl_give isl_set *isl_set_project_out_all_params(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_project_onto_map

```
__isl_give isl_map *isl_set_project_onto_map(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned first,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_params

```
__isl_give isl_basic_set *isl_basic_set_params(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_params

```
__isl_give isl_set *isl_set_params(__isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_project_out

```
__isl_give isl_basic_map *isl_basic_map_project_out(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_project_out_param_id

```
__isl_give isl_map *isl_map_project_out_param_id(
        __isl_take isl_map *map,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_project_out_param_id_list

```
__isl_give isl_map *isl_map_project_out_param_id_list(
        __isl_take isl_map *map,
        __isl_take isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_project_out

```
__isl_give isl_map *isl_map_project_out(__isl_take isl_map *map,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_project_out_all_params

```
__isl_give isl_map *isl_map_project_out_all_params(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_domain

```
__isl_give isl_basic_set *isl_basic_map_domain(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_range

```
__isl_give isl_basic_set *isl_basic_map_range(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_params

```
__isl_give isl_set *isl_map_params(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain

```
__isl_give isl_set *isl_map_domain(
        __isl_take isl_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range

```
__isl_give isl_set *isl_map_range(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_project_out

```
__isl_give isl_union_set *isl_union_set_project_out(
        __isl_take isl_union_set *uset,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_project_out_all_params

```
__isl_give isl_union_set *
isl_union_set_project_out_all_params(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_params

```
__isl_give isl_set *isl_union_set_params(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_project_out_param_id

```
__isl_give isl_union_map *
isl_union_map_project_out_param_id(
        __isl_take isl_union_map *umap,
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_project_out_param_id_list

```
__isl_give isl_union_map *
isl_union_map_project_out_param_id_list(
        __isl_take isl_union_map *umap,
        __isl_take isl_id_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_project_out

```
__isl_give isl_union_map *isl_union_map_project_out(
        __isl_take isl_union_map *umap,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_project_out_all_params

```
__isl_give isl_union_map *
isl_union_map_project_out_all_params(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_params

```
__isl_give isl_set *isl_union_map_params(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain

```
__isl_give isl_union_set *isl_union_map_domain(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range

```
__isl_give isl_union_set *isl_union_map_range(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_project_domain_on_params

```
__isl_give isl_aff *isl_aff_project_domain_on_params(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_project_domain_on_params

```
__isl_give isl_multi_aff *
isl_multi_aff_project_domain_on_params(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_project_domain_on_params

```
__isl_give isl_pw_aff *
isl_pw_aff_project_domain_on_params(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_project_domain_on_params

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_project_domain_on_params(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_project_domain_on_params

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_project_domain_on_params(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_domain

```
__isl_give isl_set *isl_pw_aff_domain(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_domain

```
__isl_give isl_set *isl_pw_multi_aff_domain(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_domain

```
__isl_give isl_set *isl_multi_pw_aff_domain(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_domain

```
__isl_give isl_union_set *isl_union_pw_aff_domain(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_domain

```
__isl_give isl_union_set *isl_union_pw_multi_aff_domain(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_domain

```
__isl_give isl_union_set *
isl_multi_union_pw_aff_domain(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_params

```
__isl_give isl_set *isl_pw_aff_params(
        __isl_take isl_pw_aff *pwa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_project_domain_on_params

```
__isl_give isl_qpolynomial *
isl_qpolynomial_project_domain_on_params(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_project_domain_on_params

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_project_domain_on_params(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_project_domain_on_params

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_project_domain_on_params(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_domain

```
__isl_give isl_set *isl_pw_qpolynomial_domain(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_domain

```
__isl_give isl_union_set *isl_union_pw_qpolynomial_fold_domain(
        __isl_take isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_domain

```
__isl_give isl_union_set *isl_union_pw_qpolynomial_domain(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_map

```
__isl_give isl_space *isl_space_domain_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_map

```
__isl_give isl_space *isl_space_range_map(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_wrapped_domain_map

```
__isl_give isl_map *isl_set_wrapped_domain_map(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_domain_map

```
__isl_give isl_basic_map *isl_basic_map_domain_map(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_range_map

```
__isl_give isl_basic_map *isl_basic_map_range_map(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_map

```
__isl_give isl_map *isl_map_domain_map(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_map

```
__isl_give isl_map *isl_map_range_map(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_map

```
__isl_give isl_union_map *isl_union_map_domain_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_map_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_map_domain_map_union_pw_multi_aff(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_map

```
__isl_give isl_union_map *isl_union_map_range_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_wrapped_domain_map

```
__isl_give isl_union_map *
isl_union_set_wrapped_domain_map(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_eliminate

```
__isl_give isl_basic_set *isl_basic_set_eliminate(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_eliminate

```
__isl_give isl_set *isl_set_eliminate(
        __isl_take isl_set *set, enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_eliminate

```
__isl_give isl_basic_map *isl_basic_map_eliminate(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_eliminate

```
__isl_give isl_map *isl_map_eliminate(
        __isl_take isl_map *map, enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_add_named_tuple_id_ui

```
__isl_give isl_space *
isl_space_add_named_tuple_id_ui(
        __isl_take isl_space *space,
        __isl_take isl_id *tuple_id, unsigned dim);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_unbind_params

```
__isl_give isl_set *isl_set_unbind_params(
        __isl_take isl_set *set,
        __isl_take isl_multi_id *tuple);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_set_from_params

```
__isl_give isl_space *isl_space_set_from_params(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_set_from_params

```
__isl_give isl_local_space *
isl_local_space_set_from_params(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_from_params

```
__isl_give isl_basic_set *isl_basic_set_from_params(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_from_params

```
__isl_give isl_set *isl_set_from_params(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_unbind_params_insert_domain

```
__isl_give isl_map *
isl_set_unbind_params_insert_domain(
        __isl_take isl_set *set,
        __isl_take isl_multi_id *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_unbind_params_insert_domain

```
__isl_give isl_aff *
isl_aff_unbind_params_insert_domain(
        __isl_take isl_aff *aff,
        __isl_take isl_multi_id *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_unbind_params_insert_domain

```
__isl_give isl_multi_aff *
isl_multi_aff_unbind_params_insert_domain(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_id *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_unbind_params_insert_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_unbind_params_insert_domain(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_id *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_from_domain

```
__isl_give isl_space *isl_space_from_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_from_range

```
__isl_give isl_space *isl_space_from_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_map_from_set

```
__isl_give isl_space *isl_space_map_from_set(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_map_from_domain_and_range

```
__isl_give isl_space *isl_space_map_from_domain_and_range(
        __isl_take isl_space *domain,
        __isl_take isl_space *range);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_from_domain

```
__isl_give isl_local_space *isl_local_space_from_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_insert_domain

```
__isl_give isl_map *isl_set_insert_domain(
        __isl_take isl_set *set,
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_domain

```
__isl_give isl_map *isl_map_from_domain(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_from_range

```
__isl_give isl_map *isl_map_from_range(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_domain

```
__isl_give isl_union_map *isl_union_map_from_domain(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_range

```
__isl_give isl_union_map *isl_union_map_from_range(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_from_domain_and_range

```
__isl_give isl_union_map *
isl_union_map_from_domain_and_range(
        __isl_take isl_union_set *domain,
        __isl_take isl_union_set *range);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_from_range

```
__isl_give isl_multi_id *isl_multi_id_from_range(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_from_range

```
__isl_give isl_multi_val *isl_multi_val_from_range(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_insert_domain

```
__isl_give isl_multi_aff *
isl_multi_aff_insert_domain(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_insert_domain

```
__isl_give isl_pw_aff *isl_pw_aff_insert_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_insert_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_insert_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_insert_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_insert_domain(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_space *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_from_range

```
__isl_give isl_aff *isl_aff_from_range(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_from_range

```
__isl_give isl_multi_aff *isl_multi_aff_from_range(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_from_range

```
__isl_give isl_pw_aff *isl_pw_aff_from_range(
        __isl_take isl_pw_aff *pwa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_from_range

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_from_range(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_range

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_range(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_domain

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_from_domain(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_domain(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_from_range

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_from_range(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_from_range

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_from_range(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_fix_si

```
__isl_give isl_basic_set *isl_basic_set_fix_si(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_fix_val

```
__isl_give isl_basic_set *isl_basic_set_fix_val(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_fix_si

```
__isl_give isl_set *isl_set_fix_si(__isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_fix_val

```
__isl_give isl_set *isl_set_fix_val(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_fix_si

```
__isl_give isl_basic_map *isl_basic_map_fix_si(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_fix_val

```
__isl_give isl_basic_map *isl_basic_map_fix_val(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_fix_si

```
__isl_give isl_map *isl_map_fix_si(__isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_fix_val

```
__isl_give isl_map *isl_map_fix_val(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_fix_si

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_fix_si(
        __isl_take isl_pw_multi_aff *pma,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fix_val

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_fix_val(
        __isl_take isl_pw_qpolynomial *pwqp,
        enum isl_dim_type type, unsigned n,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_fix_val

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_fix_val(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        enum isl_dim_type type, unsigned n,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_lower_bound_val

```
__isl_give isl_basic_set *
isl_basic_set_lower_bound_val(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_upper_bound_val

```
__isl_give isl_basic_set *
isl_basic_set_upper_bound_val(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lower_bound_si

```
__isl_give isl_set *isl_set_lower_bound_si(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lower_bound_val

```
__isl_give isl_set *isl_set_lower_bound_val(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_upper_bound_si

```
__isl_give isl_set *isl_set_upper_bound_si(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_upper_bound_val

```
__isl_give isl_set *isl_set_upper_bound_val(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lower_bound_multi_val

```
__isl_give isl_set *isl_set_lower_bound_multi_val(
        __isl_take isl_set *set,
        __isl_take isl_multi_val *lower);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_upper_bound_multi_val

```
__isl_give isl_set *isl_set_upper_bound_multi_val(
        __isl_take isl_set *set,
        __isl_take isl_multi_val *upper);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lower_bound_multi_pw_aff

```
__isl_give isl_set *isl_set_lower_bound_multi_pw_aff(
        __isl_take isl_set *set,
        __isl_take isl_multi_pw_aff *lower);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_upper_bound_multi_pw_aff

```
__isl_give isl_set *isl_set_upper_bound_multi_pw_aff(
        __isl_take isl_set *set,
        __isl_take isl_multi_pw_aff *upper);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_lower_bound_si

```
__isl_give isl_basic_map *isl_basic_map_lower_bound_si(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_upper_bound_si

```
__isl_give isl_basic_map *isl_basic_map_upper_bound_si(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lower_bound_si

```
__isl_give isl_map *isl_map_lower_bound_si(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_upper_bound_si

```
__isl_give isl_map *isl_map_upper_bound_si(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos, int value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lower_bound_val

```
__isl_give isl_map *isl_map_lower_bound_val(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_upper_bound_val

```
__isl_give isl_map *isl_map_upper_bound_val(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos,
        __isl_take isl_val *value);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lower_bound_multi_pw_aff

```
__isl_give isl_map *isl_map_lower_bound_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *lower);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_upper_bound_multi_pw_aff

```
__isl_give isl_map *isl_map_upper_bound_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *upper);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_equate

```
__isl_give isl_set *isl_set_equate(__isl_take isl_set *set,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_equate

```
__isl_give isl_basic_map *isl_basic_map_equate(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_equate

```
__isl_give isl_map *isl_map_equate(__isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_oppose

```
__isl_give isl_map *isl_map_oppose(__isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_order_le

```
__isl_give isl_map *isl_map_order_le(
        __isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_order_ge

```
__isl_give isl_basic_map *isl_basic_map_order_ge(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_order_ge

```
__isl_give isl_map *isl_map_order_ge(
        __isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_order_lt

```
__isl_give isl_map *isl_map_order_lt(__isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_order_gt

```
__isl_give isl_basic_map *isl_basic_map_order_gt(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_order_gt

```
__isl_give isl_map *isl_map_order_gt(__isl_take isl_map *map,
        enum isl_dim_type type1, int pos1,
        enum isl_dim_type type2, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_remove_map_if

```
__isl_give isl_union_map *isl_union_map_remove_map_if(
        __isl_take isl_union_map *umap,
        isl_bool (*fn)(__isl_keep isl_map *map,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_zero_basic_set

```
__isl_give isl_basic_set *isl_aff_zero_basic_set(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_neg_basic_set

```
__isl_give isl_basic_set *isl_aff_neg_basic_set(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_pos_set

```
__isl_give isl_set *isl_pw_aff_pos_set(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_nonneg_set

```
__isl_give isl_set *isl_pw_aff_nonneg_set(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_zero_set

```
__isl_give isl_set *isl_pw_aff_zero_set(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_non_zero_set

```
__isl_give isl_set *isl_pw_aff_non_zero_set(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_zero_union_set

```
__isl_give isl_union_set *
isl_union_pw_aff_zero_union_set(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_zero_union_set

```
__isl_give isl_union_set *
isl_multi_union_pw_aff_zero_union_set(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_identity

```
__isl_give isl_map *isl_set_identity(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_identity

```
__isl_give isl_union_map *isl_union_set_identity(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_identity_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_set_identity_union_pw_multi_aff(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_indicator_function

```
__isl_give isl_pw_aff *isl_set_indicator_function(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_as_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_set_as_pw_multi_aff(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_set

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_from_set(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_as_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_map_as_pw_multi_aff(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_from_map

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_from_map(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_union_set

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_union_set(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_as_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_map_as_union_pw_multi_aff(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_from_union_map

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_from_union_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_as_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_union_map_as_multi_union_pw_aff(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_from_union_map

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_from_union_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_deltas

```
__isl_give isl_basic_set *isl_basic_map_deltas(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_deltas

```
__isl_give isl_set *isl_map_deltas(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_deltas

```
__isl_give isl_union_set *isl_union_map_deltas(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_deltas_map

```
__isl_give isl_basic_map *isl_basic_map_deltas_map(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_deltas_map

```
__isl_give isl_map *isl_map_deltas_map(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_deltas_map

```
__isl_give isl_union_map *isl_union_map_deltas_map(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_translation

```
__isl_give isl_map *isl_set_translation(
        __isl_take isl_set *deltas);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_coalesce

```
__isl_give isl_set *isl_set_coalesce(__isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_coalesce

```
__isl_give isl_map *isl_map_coalesce(__isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_coalesce

```
__isl_give isl_union_set *isl_union_set_coalesce(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_coalesce

```
__isl_give isl_union_map *isl_union_map_coalesce(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_coalesce

```
__isl_give isl_pw_aff *isl_pw_aff_coalesce(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_coalesce

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_coalesce(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_coalesce

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_coalesce(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_coalesce

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_coalesce(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_coalesce

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_coalesce(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_coalesce

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_coalesce(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_coalesce

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_coalesce(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_coalesce

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_coalesce(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_coalesce

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_coalesce(
        __isl_take isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_coalesce_bounded_wrapping

```
isl_stat isl_options_set_coalesce_bounded_wrapping(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_coalesce_bounded_wrapping

```
int isl_options_get_coalesce_bounded_wrapping(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_coalesce_preserve_locals

```
isl_stat isl_options_set_coalesce_preserve_locals(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_coalesce_preserve_locals

```
int isl_options_get_coalesce_preserve_locals(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_detect_equalities

```
__isl_give isl_basic_set *isl_basic_set_detect_equalities(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_detect_equalities

```
__isl_give isl_basic_map *isl_basic_map_detect_equalities(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_detect_equalities

```
__isl_give isl_set *isl_set_detect_equalities(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_detect_equalities

```
__isl_give isl_map *isl_map_detect_equalities(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_detect_equalities

```
__isl_give isl_union_set *isl_union_set_detect_equalities(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_detect_equalities

```
__isl_give isl_union_map *isl_union_map_detect_equalities(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_remove_redundancies

```
__isl_give isl_basic_set *isl_basic_set_remove_redundancies(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_remove_redundancies

```
__isl_give isl_set *isl_set_remove_redundancies(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_remove_redundancies

```
__isl_give isl_union_set *
isl_union_set_remove_redundancies(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_remove_redundancies

```
__isl_give isl_basic_map *isl_basic_map_remove_redundancies(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_remove_redundancies

```
__isl_give isl_map *isl_map_remove_redundancies(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_remove_redundancies

```
__isl_give isl_union_map *
isl_union_map_remove_redundancies(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_convex_hull

```
__isl_give isl_basic_set *isl_set_convex_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_convex_hull

```
__isl_give isl_basic_map *isl_map_convex_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_unshifted_simple_hull

```
__isl_give isl_basic_set *
isl_set_unshifted_simple_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_simple_hull

```
__isl_give isl_basic_set *isl_set_simple_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_plain_unshifted_simple_hull

```
__isl_give isl_basic_set *
isl_set_plain_unshifted_simple_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_unshifted_simple_hull_from_set_list

```
__isl_give isl_basic_set *
isl_set_unshifted_simple_hull_from_set_list(
        __isl_take isl_set *set,
        __isl_take isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_unshifted_simple_hull

```
__isl_give isl_basic_map *
isl_map_unshifted_simple_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_simple_hull

```
__isl_give isl_basic_map *isl_map_simple_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_plain_unshifted_simple_hull

```
__isl_give isl_basic_map *
isl_map_plain_unshifted_simple_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_unshifted_simple_hull_from_map_list

```
__isl_give isl_basic_map *
isl_map_unshifted_simple_hull_from_map_list(
        __isl_take isl_map *map,
        __isl_take isl_map_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_simple_hull

```
__isl_give isl_union_map *isl_union_map_simple_hull(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_affine_hull

```
__isl_give isl_basic_set *isl_basic_set_affine_hull(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_affine_hull

```
__isl_give isl_basic_set *isl_set_affine_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_affine_hull

```
__isl_give isl_union_set *isl_union_set_affine_hull(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_affine_hull

```
__isl_give isl_basic_map *isl_basic_map_affine_hull(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_affine_hull

```
__isl_give isl_basic_map *isl_map_affine_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_affine_hull

```
__isl_give isl_union_map *isl_union_map_affine_hull(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_polyhedral_hull

```
__isl_give isl_basic_set *isl_set_polyhedral_hull(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_polyhedral_hull

```
__isl_give isl_basic_map *isl_map_polyhedral_hull(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_polyhedral_hull

```
__isl_give isl_union_set *isl_union_set_polyhedral_hull(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_polyhedral_hull

```
__isl_give isl_union_map *isl_union_map_polyhedral_hull(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_get_simple_fixed_box_hull

```
__isl_give isl_fixed_box *
isl_set_get_simple_fixed_box_hull(
        __isl_keep isl_set *set)
__isl_give isl_fixed_box *
isl_map_get_range_simple_fixed_box_hull(
        __isl_keep isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_is_valid

```
isl_bool isl_fixed_box_is_valid(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_get_offset

```
__isl_give isl_multi_aff *isl_fixed_box_get_offset(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_get_size

```
__isl_give isl_multi_val *isl_fixed_box_get_size(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_copy

```
__isl_give isl_fixed_box *isl_fixed_box_copy(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_free

```
__isl_null isl_fixed_box *isl_fixed_box_free(
        __isl_take isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_read_from_str

```
__isl_give isl_fixed_box *
isl_fixed_box_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_fixed_box

```
__isl_give isl_printer *isl_printer_print_fixed_box(
        __isl_take isl_printer *p,
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_fixed_box_to_str

```
__isl_give char *isl_fixed_box_to_str(
        __isl_keep isl_fixed_box *box);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_drop_constraints_involving_dims

```
__isl_give isl_basic_set *
isl_basic_set_drop_constraints_involving_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_drop_constraints_not_involving_dims

```
__isl_give isl_basic_set *
isl_basic_set_drop_constraints_not_involving_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_drop_constraints_involving_dims

```
__isl_give isl_set *
isl_set_drop_constraints_involving_dims(
        __isl_take isl_set *set,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_drop_constraints_not_involving_dims

```
__isl_give isl_set *
isl_set_drop_constraints_not_involving_dims(
        __isl_take isl_set *set,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_drop_constraints_involving_dims

```
__isl_give isl_basic_map *
isl_basic_map_drop_constraints_involving_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_drop_constraints_not_involving_dims

```
__isl_give isl_basic_map *
isl_basic_map_drop_constraints_not_involving_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_drop_constraints_involving_dims

```
__isl_give isl_map *
isl_map_drop_constraints_involving_dims(
        __isl_take isl_map *map,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_drop_constraints_not_involving_dims

```
__isl_give isl_map *
isl_map_drop_constraints_not_involving_dims(
        __isl_take isl_map *map,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_to_polynomial

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_to_polynomial(
        __isl_take isl_pw_qpolynomial *pwqp, int sign);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_to_polynomial

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_to_polynomial(
        __isl_take isl_union_pw_qpolynomial *upwqp, int sign);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_sample

```
__isl_give isl_basic_set *isl_basic_set_sample(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_sample

```
__isl_give isl_basic_set *isl_set_sample(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_sample

```
__isl_give isl_basic_map *isl_basic_map_sample(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_sample

```
__isl_give isl_basic_map *isl_map_sample(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_max_val

```
__isl_give isl_val *isl_basic_set_max_val(
        __isl_keep isl_basic_set *bset,
        __isl_keep isl_aff *obj);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_min_val

```
__isl_give isl_val *isl_set_min_val(
        __isl_keep isl_set *set,
        __isl_keep isl_aff *obj);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_max_val

```
__isl_give isl_val *isl_set_max_val(
        __isl_keep isl_set *set,
        __isl_keep isl_aff *obj);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_min_multi_union_pw_aff

```
__isl_give isl_multi_val *
isl_union_set_min_multi_union_pw_aff(
        __isl_keep isl_union_set *uset,
        __isl_keep isl_multi_union_pw_aff *obj);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_min_val

```
__isl_give isl_val *isl_pw_aff_min_val(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_max_val

```
__isl_give isl_val *isl_pw_aff_max_val(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_min_multi_val

```
__isl_give isl_multi_val *
isl_pw_multi_aff_min_multi_val(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_max_multi_val

```
__isl_give isl_multi_val *
isl_pw_multi_aff_max_multi_val(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_min_multi_val

```
__isl_give isl_multi_val *
isl_multi_pw_aff_min_multi_val(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_max_multi_val

```
__isl_give isl_multi_val *
isl_multi_pw_aff_max_multi_val(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_min_val

```
__isl_give isl_val *isl_union_pw_aff_min_val(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_max_val

```
__isl_give isl_val *isl_union_pw_aff_max_val(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_min_multi_val

```
__isl_give isl_multi_val *
isl_multi_union_pw_aff_min_multi_val(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_max_multi_val

```
__isl_give isl_multi_val *
isl_multi_union_pw_aff_max_multi_val(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_dim_max_val

```
__isl_give isl_val *isl_basic_set_dim_max_val(
        __isl_take isl_basic_set *bset, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_min_val

```
__isl_give isl_val *isl_set_dim_min_val(
        __isl_take isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_max_val

```
__isl_give isl_val *isl_set_dim_max_val(
        __isl_take isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_min

```
__isl_give isl_pw_aff *isl_set_dim_min(
        __isl_take isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_dim_max

```
__isl_give isl_pw_aff *isl_set_dim_max(
        __isl_take isl_set *set, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_dim_min

```
__isl_give isl_pw_aff *isl_map_dim_min(
        __isl_take isl_map *map, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_dim_max

```
__isl_give isl_pw_aff *isl_map_dim_max(
        __isl_take isl_map *map, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_min_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_set_min_multi_pw_aff(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_max_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_set_max_multi_pw_aff(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_min_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_map_min_multi_pw_aff(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_max_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_map_max_multi_pw_aff(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_coefficients

```
__isl_give isl_basic_set *isl_basic_set_coefficients(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_list_coefficients

```
__isl_give isl_basic_set_list *
isl_basic_set_list_coefficients(
        __isl_take isl_basic_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_coefficients

```
__isl_give isl_basic_set *isl_set_coefficients(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_coefficients

```
__isl_give isl_union_set *isl_union_set_coefficients(
        __isl_take isl_union_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_solutions

```
__isl_give isl_basic_set *isl_basic_set_solutions(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_solutions

```
__isl_give isl_basic_set *isl_set_solutions(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_solutions

```
__isl_give isl_union_set *isl_union_set_solutions(
        __isl_take isl_union_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_fixed_power_val

```
__isl_give isl_map *isl_map_fixed_power_val(
        __isl_take isl_map *map,
        __isl_take isl_val *exp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_fixed_power_val

```
__isl_give isl_union_map *
isl_union_map_fixed_power_val(
        __isl_take isl_union_map *umap,
        __isl_take isl_val *exp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_power

```
__isl_give isl_map *isl_map_power(__isl_take isl_map *map,
        isl_bool *exact);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_power

```
__isl_give isl_union_map *isl_union_map_power(
        __isl_take isl_union_map *umap, isl_bool *exact);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_transitive_closure

```
__isl_give isl_map *isl_map_transitive_closure(
        __isl_take isl_map *map, isl_bool *exact);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_transitive_closure

```
__isl_give isl_union_map *isl_union_map_transitive_closure(
        __isl_take isl_union_map *umap, isl_bool *exact);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_reaching_path_lengths

```
__isl_give isl_map *isl_map_reaching_path_lengths(
        __isl_take isl_map *map, isl_bool *exact);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_wrap

```
__isl_give isl_space *isl_space_wrap(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_unwrap

```
__isl_give isl_space *isl_space_unwrap(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_wrap

```
__isl_give isl_local_space *isl_local_space_wrap(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_unwrap

```
__isl_give isl_basic_map *isl_basic_set_unwrap(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_unwrap

```
__isl_give isl_map *isl_set_unwrap(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_wrap

```
__isl_give isl_basic_set *isl_basic_map_wrap(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_wrap

```
__isl_give isl_set *isl_map_wrap(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_unwrap

```
__isl_give isl_union_map *isl_union_set_unwrap(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_wrap

```
__isl_give isl_union_set *isl_union_map_wrap(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_flatten_domain

```
__isl_give isl_space *isl_space_flatten_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_flatten_range

```
__isl_give isl_space *isl_space_flatten_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_flatten_domain

```
__isl_give isl_local_space *
isl_local_space_flatten_domain(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_flatten_range

```
__isl_give isl_local_space *
isl_local_space_flatten_range(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_flatten

```
__isl_give isl_basic_set *isl_basic_set_flatten(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_flatten

```
__isl_give isl_set *isl_set_flatten(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_flatten_domain

```
__isl_give isl_basic_map *isl_basic_map_flatten_domain(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_flatten_range

```
__isl_give isl_basic_map *isl_basic_map_flatten_range(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flatten_range

```
__isl_give isl_map *isl_map_flatten_range(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flatten_domain

```
__isl_give isl_map *isl_map_flatten_domain(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_flatten

```
__isl_give isl_basic_map *isl_basic_map_flatten(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flatten

```
__isl_give isl_map *isl_map_flatten(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_flatten_range

```
__isl_give isl_multi_id *isl_multi_id_flatten_range(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_flatten_range

```
__isl_give isl_multi_val *isl_multi_val_flatten_range(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_flatten_domain

```
__isl_give isl_multi_aff *isl_multi_aff_flatten_domain(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_flatten_range

```
__isl_give isl_multi_aff *isl_multi_aff_flatten_range(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_flatten_range

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_flatten_range(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_flatten_range

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_flatten_range(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_flatten_map

```
__isl_give isl_map *isl_set_flatten_map(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_lift

```
__isl_give isl_basic_set *isl_basic_set_lift(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lift

```
__isl_give isl_set *isl_set_lift(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_lift

```
__isl_give isl_union_set *isl_union_set_lift(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_lifting

```
__isl_give isl_basic_map *isl_local_space_lifting(
        __isl_take isl_local_space *ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_lift

```
__isl_give isl_multi_aff *isl_multi_aff_lift(
        __isl_take isl_multi_aff *maff,
        __isl_give isl_local_space **ls);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_zip

```
__isl_give isl_space *isl_space_zip(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_zip

```
__isl_give isl_basic_map *isl_basic_map_zip(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_zip

```
__isl_give isl_map *isl_map_zip(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_zip

```
__isl_give isl_union_map *isl_union_map_zip(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_curry

```
__isl_give isl_space *isl_space_curry(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_uncurry

```
__isl_give isl_space *isl_space_uncurry(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_curry

```
__isl_give isl_basic_map *isl_basic_map_curry(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_uncurry

```
__isl_give isl_basic_map *isl_basic_map_uncurry(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_curry

```
__isl_give isl_map *isl_map_curry(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_uncurry

```
__isl_give isl_map *isl_map_uncurry(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_curry

```
__isl_give isl_union_map *isl_union_map_curry(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_uncurry

```
__isl_give isl_union_map *isl_union_map_uncurry(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_curry

```
__isl_give isl_space *isl_space_range_curry(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_curry

```
__isl_give isl_map *isl_map_range_curry(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_curry

```
__isl_give isl_union_map *isl_union_map_range_curry(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_align_params

```
__isl_give isl_space *isl_space_align_params(
        __isl_take isl_space *space1,
        __isl_take isl_space *space2)
__isl_give isl_basic_set *isl_basic_set_align_params(
        __isl_take isl_basic_set *bset,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_align_params

```
__isl_give isl_set *isl_set_align_params(
        __isl_take isl_set *set,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_align_params

```
__isl_give isl_basic_map *isl_basic_map_align_params(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_align_params

```
__isl_give isl_map *isl_map_align_params(
        __isl_take isl_map *map,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_align_params

```
__isl_give isl_multi_id *isl_multi_id_align_params(
        __isl_take isl_multi_id *mi,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_align_params

```
__isl_give isl_multi_val *isl_multi_val_align_params(
        __isl_take isl_multi_val *mv,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_align_params

```
__isl_give isl_aff *isl_aff_align_params(
        __isl_take isl_aff *aff,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_align_params

```
__isl_give isl_multi_aff *isl_multi_aff_align_params(
        __isl_take isl_multi_aff *multi,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_align_params

```
__isl_give isl_pw_aff *isl_pw_aff_align_params(
        __isl_take isl_pw_aff *pwaff,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_align_params

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_align_params(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_align_params

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_align_params(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_align_params

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_align_params(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_align_params

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_align_params(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_align_params

```
__isl_give isl_qpolynomial *isl_qpolynomial_align_params(
        __isl_take isl_qpolynomial *qp,
        __isl_take isl_space *model);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_drop_unused_params

```
__isl_give isl_basic_set *
isl_basic_set_drop_unused_params(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_drop_unused_params

```
__isl_give isl_set *isl_set_drop_unused_params(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_drop_unused_params

```
__isl_give isl_basic_map *
isl_basic_map_drop_unused_params(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_drop_unused_params

```
__isl_give isl_map *isl_map_drop_unused_params(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_drop_unused_params

```
__isl_give isl_union_set *
isl_union_set_drop_unused_params(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_drop_unused_params

```
__isl_give isl_union_map *
isl_union_map_drop_unused_params(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_drop_unused_params

```
__isl_give isl_pw_aff *isl_pw_aff_drop_unused_params(
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_drop_unused_params

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_drop_unused_params(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_drop_unused_params

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_drop_unused_params(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_drop_unused_params

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_drop_unused_params(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_drop_unused_params

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_drop_unused_params(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_drop_unused_params

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_drop_unused_params(
        __isl_take isl_pw_qpolynomial_fold *pwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_drop_unused_params

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_drop_unused_params(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_drop_unused_params

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_drop_unused_params(
        __isl_take isl_union_pw_qpolynomial_fold *upwf);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_neg

```
__isl_give isl_set *isl_set_neg(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_neg

```
__isl_give isl_map *isl_map_neg(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_neg

```
__isl_give isl_multi_val *isl_multi_val_neg(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_neg

```
__isl_give isl_aff *isl_aff_neg(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_neg

```
__isl_give isl_multi_aff *isl_multi_aff_neg(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_neg

```
__isl_give isl_pw_aff *isl_pw_aff_neg(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_neg

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_neg(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_neg

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_neg(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_neg

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_neg(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_neg

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_neg(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_neg

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_neg(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_ceil

```
__isl_give isl_aff *isl_aff_ceil(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_ceil

```
__isl_give isl_pw_aff *isl_pw_aff_ceil(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_floor

```
__isl_give isl_aff *isl_aff_floor(
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_floor

```
__isl_give isl_multi_aff *isl_multi_aff_floor(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_floor

```
__isl_give isl_pw_aff *isl_pw_aff_floor(
        __isl_take isl_pw_aff *pwaff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_floor

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_floor(
        __isl_take isl_union_pw_aff *upa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_floor

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_floor(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_min

```
__isl_give isl_pw_aff *isl_pw_aff_list_min(
        __isl_take isl_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_max

```
__isl_give isl_pw_aff *isl_pw_aff_list_max(
        __isl_take isl_pw_aff_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_neg

```
__isl_give isl_qpolynomial *isl_qpolynomial_neg(
        __isl_take isl_qpolynomial *qp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_neg

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_neg(
        __isl_take isl_pw_qpolynomial *pwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_neg

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_neg(
        __isl_take isl_union_pw_qpolynomial *upwqp);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_pow

```
__isl_give isl_qpolynomial *isl_qpolynomial_pow(
        __isl_take isl_qpolynomial *qp,
        unsigned exponent);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_pow

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_pow(
        __isl_take isl_pw_qpolynomial *pwqp,
        unsigned exponent);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_eval

```
__isl_give isl_val *isl_aff_eval(
        __isl_take isl_aff *aff,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_eval

```
__isl_give isl_val *isl_pw_aff_eval(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_eval

```
__isl_give isl_val *isl_pw_qpolynomial_eval(
        __isl_take isl_pw_qpolynomial *pwqp,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_eval

```
__isl_give isl_val *isl_pw_qpolynomial_fold_eval(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_eval

```
__isl_give isl_val *isl_union_pw_qpolynomial_eval(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_eval

```
__isl_give isl_val *isl_union_pw_qpolynomial_fold_eval(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_point *pnt);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_add_dims

```
__isl_give isl_space *isl_space_add_dims(
        __isl_take isl_space *space,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_insert_dims

```
__isl_give isl_space *isl_space_insert_dims(
        __isl_take isl_space *space,
        enum isl_dim_type type, unsigned pos, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_drop_dims

```
__isl_give isl_space *isl_space_drop_dims(
        __isl_take isl_space *space,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_move_dims

```
__isl_give isl_space *isl_space_move_dims(
        __isl_take isl_space *space,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_add_dims

```
__isl_give isl_local_space *isl_local_space_add_dims(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_insert_dims

```
__isl_give isl_local_space *isl_local_space_insert_dims(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_drop_dims

```
__isl_give isl_local_space *isl_local_space_drop_dims(
        __isl_take isl_local_space *ls,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_add_dims

```
__isl_give isl_basic_set *isl_basic_set_add_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_add_dims

```
__isl_give isl_set *isl_set_add_dims(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_insert_dims

```
__isl_give isl_basic_set *isl_basic_set_insert_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type type, unsigned pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_insert_dims

```
__isl_give isl_set *isl_set_insert_dims(
        __isl_take isl_set *set,
        enum isl_dim_type type, unsigned pos, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_move_dims

```
__isl_give isl_basic_set *isl_basic_set_move_dims(
        __isl_take isl_basic_set *bset,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_move_dims

```
__isl_give isl_set *isl_set_move_dims(
        __isl_take isl_set *set,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_add_dims

```
__isl_give isl_basic_map *isl_basic_map_add_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_add_dims

```
__isl_give isl_map *isl_map_add_dims(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_insert_dims

```
__isl_give isl_basic_map *isl_basic_map_insert_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type type, unsigned pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_insert_dims

```
__isl_give isl_map *isl_map_insert_dims(
        __isl_take isl_map *map,
        enum isl_dim_type type, unsigned pos, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_move_dims

```
__isl_give isl_basic_map *isl_basic_map_move_dims(
        __isl_take isl_basic_map *bmap,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_move_dims

```
__isl_give isl_map *isl_map_move_dims(
        __isl_take isl_map *map,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_insert_dims

```
__isl_give isl_multi_val *isl_multi_val_insert_dims(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_add_dims

```
__isl_give isl_multi_val *isl_multi_val_add_dims(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_drop_dims

```
__isl_give isl_multi_val *isl_multi_val_drop_dims(
        __isl_take isl_multi_val *mv,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_insert_dims

```
__isl_give isl_aff *isl_aff_insert_dims(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_insert_dims

```
__isl_give isl_multi_aff *isl_multi_aff_insert_dims(
        __isl_take isl_multi_aff *ma,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_insert_dims

```
__isl_give isl_pw_aff *isl_pw_aff_insert_dims(
        __isl_take isl_pw_aff *pwaff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_insert_dims

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_insert_dims(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add_dims

```
__isl_give isl_aff *isl_aff_add_dims(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_add_dims

```
__isl_give isl_multi_aff *isl_multi_aff_add_dims(
        __isl_take isl_multi_aff *ma,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_add_dims

```
__isl_give isl_pw_aff *isl_pw_aff_add_dims(
        __isl_take isl_pw_aff *pwaff,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_add_dims

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_add_dims(
        __isl_take isl_multi_pw_aff *mpa,
        enum isl_dim_type type, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_drop_dims

```
__isl_give isl_aff *isl_aff_drop_dims(
        __isl_take isl_aff *aff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_drop_dims

```
__isl_give isl_multi_aff *isl_multi_aff_drop_dims(
        __isl_take isl_multi_aff *maff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_drop_dims

```
__isl_give isl_pw_aff *isl_pw_aff_drop_dims(
        __isl_take isl_pw_aff *pwaff,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_drop_dims

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_drop_dims(
        __isl_take isl_pw_multi_aff *pma,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_drop_dims

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_drop_dims(
        __isl_take isl_union_pw_aff *upa,
        enum isl_dim_type type, unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_drop_dims

```
__isl_give isl_union_pw_multi_aff *
        isl_union_pw_multi_aff_drop_dims(
        __isl_take isl_union_pw_multi_aff *upma,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_drop_dims

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_drop_dims(
        __isl_take isl_multi_union_pw_aff *mupa,
        enum isl_dim_type type, unsigned first,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_move_dims

```
__isl_give isl_aff *isl_aff_move_dims(
        __isl_take isl_aff *aff,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_move_dims

```
__isl_give isl_multi_aff *isl_multi_aff_move_dims(
        __isl_take isl_multi_aff *ma,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_move_dims

```
__isl_give isl_pw_aff *isl_pw_aff_move_dims(
        __isl_take isl_pw_aff *pa,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_move_dims

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_move_dims(
        __isl_take isl_multi_pw_aff *pma,
        enum isl_dim_type dst_type, unsigned dst_pos,
        enum isl_dim_type src_type, unsigned src_pos,
        unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_drop_dims

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_drop_dims(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_drop_dims

```
__isl_give isl_union_pw_qpolynomial_fold *
        isl_union_pw_qpolynomial_fold_drop_dims(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        enum isl_dim_type type,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_local_space_intersect

```
__isl_give isl_local_space *isl_local_space_intersect(
        __isl_take isl_local_space *ls1,
        __isl_take isl_local_space *ls2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_intersect_params

```
__isl_give isl_basic_set *isl_basic_set_intersect_params(
        __isl_take isl_basic_set *bset1,
        __isl_take isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_list_intersect

```
__isl_give isl_basic_set *isl_basic_set_list_intersect(
        __isl_take struct isl_basic_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_intersect_params

```
__isl_give isl_set *isl_set_intersect_params(
        __isl_take isl_set *set,
        __isl_take isl_set *params);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_intersect

```
__isl_give isl_set *isl_set_intersect(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_intersect_factor_domain

```
__isl_give isl_set *isl_set_intersect_factor_domain(
        __isl_take isl_set *set,
        __isl_take isl_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_intersect_factor_range

```
__isl_give isl_set *isl_set_intersect_factor_range(
        __isl_take isl_set *set,
        __isl_take isl_set *range);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_intersect_params

```
__isl_give isl_basic_map *isl_basic_map_intersect_params(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_intersect_domain

```
__isl_give isl_basic_map *isl_basic_map_intersect_domain(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_intersect_range

```
__isl_give isl_basic_map *isl_basic_map_intersect_range(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_intersect

```
__isl_give isl_basic_map *isl_basic_map_intersect(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_list_intersect

```
__isl_give isl_basic_map *isl_basic_map_list_intersect(
        __isl_take isl_basic_map_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_params

```
__isl_give isl_map *isl_map_intersect_params(
        __isl_take isl_map *map,
        __isl_take isl_set *params);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_domain

```
__isl_give isl_map *isl_map_intersect_domain(
        __isl_take isl_map *map,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_range

```
__isl_give isl_map *isl_map_intersect_range(
        __isl_take isl_map *map,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect

```
__isl_give isl_map *isl_map_intersect(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_domain_factor_domain

```
__isl_give isl_map *
isl_map_intersect_domain_factor_domain(
        __isl_take isl_map *map,
        __isl_take isl_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_domain_factor_range

```
__isl_give isl_map *
isl_map_intersect_domain_factor_range(
        __isl_take isl_map *map,
        __isl_take isl_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_range_factor_domain

```
__isl_give isl_map *
isl_map_intersect_range_factor_domain(
        __isl_take isl_map *map,
        __isl_take isl_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_range_factor_range

```
__isl_give isl_map *
isl_map_intersect_range_factor_range(
        __isl_take isl_map *map,
        __isl_take isl_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_domain_wrapped_domain

```
__isl_give isl_map *
isl_map_intersect_domain_wrapped_domain(
        __isl_take isl_map *map,
        __isl_take isl_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_intersect_range_wrapped_domain

```
__isl_give isl_map *
isl_map_intersect_range_wrapped_domain(
        __isl_take isl_map *map,
        __isl_take isl_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_intersect_params

```
__isl_give isl_union_set *isl_union_set_intersect_params(
        __isl_take isl_union_set *uset,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_intersect

```
__isl_give isl_union_set *isl_union_set_intersect(
        __isl_take isl_union_set *uset1,
        __isl_take isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_params

```
__isl_give isl_union_map *isl_union_map_intersect_params(
        __isl_take isl_union_map *umap,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain_union_set

```
__isl_give isl_union_map *
isl_union_map_intersect_domain_union_set(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain_space

```
__isl_give isl_union_map *
isl_union_map_intersect_domain_space(
        __isl_take isl_union_map *umap,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain

```
__isl_give isl_union_map *isl_union_map_intersect_domain(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range_union_set

```
__isl_give isl_union_map *
isl_union_map_intersect_range_union_set(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range_space

```
__isl_give isl_union_map *
isl_union_map_intersect_range_space(
        __isl_take isl_union_map *umap,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range

```
__isl_give isl_union_map *isl_union_map_intersect_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect

```
__isl_give isl_union_map *isl_union_map_intersect(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain_factor_domain

```
__isl_give isl_union_map *
isl_union_map_intersect_domain_factor_domain(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain_factor_range

```
__isl_give isl_union_map *
isl_union_map_intersect_domain_factor_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range_factor_domain

```
__isl_give isl_union_map *
isl_union_map_intersect_range_factor_domain(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range_factor_range

```
__isl_give isl_union_map *
isl_union_map_intersect_range_factor_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_map *factor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_domain_wrapped_domain_union_set

```
__isl_give isl_union_map *
isl_union_map_intersect_domain_wrapped_domain_union_set(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_intersect_range_wrapped_domain_union_set

```
__isl_give isl_union_map *
isl_union_map_intersect_range_wrapped_domain_union_set(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_intersect_domain

```
__isl_give isl_pw_aff *isl_pw_aff_intersect_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_intersect_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_intersect_domain(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_intersect_domain

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_intersect_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_domain_space

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_intersect_domain_space(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_domain_union_set

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_intersect_domain_union_set(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_domain

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_intersect_domain(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_domain_space

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_domain_space(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_domain_union_set

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_domain_union_set(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_domain(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_intersect_domain

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_intersect_domain(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_intersect_domain_wrapped_domain

```
__isl_give isl_pw_aff *
isl_pw_aff_intersect_domain_wrapped_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_intersect_domain_wrapped_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_intersect_domain_wrapped_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_domain_wrapped_domain

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_intersect_domain_wrapped_domain(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_domain_wrapped_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_domain_wrapped_domain(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_intersect_domain_wrapped_range

```
__isl_give isl_pw_aff *
isl_pw_aff_intersect_domain_wrapped_range(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_intersect_domain_wrapped_range

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_intersect_domain_wrapped_range(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_domain_wrapped_range

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_domain_wrapped_range(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_domain_wrapped_range

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_intersect_domain_wrapped_range(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_intersect_params

```
__isl_give isl_pw_aff *isl_pw_aff_intersect_params(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_intersect_params

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_intersect_params(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_intersect_params

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_intersect_params(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_intersect_params

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_intersect_params(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_intersect_params

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_intersect_params(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_intersect_params

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_intersect_params(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_set *params);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_intersect_range

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_intersect_range(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_intersect_domain

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_intersect_domain(
        __isl_take isl_pw_qpolynomial *pwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_domain_space

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_domain_space(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_domain_union_set

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_domain_union_set(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_domain

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_domain(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_domain_space

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_domain_space(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_domain_union_set

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_domain_union_set(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_domain

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_domain(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_intersect_domain_wrapped_domain

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_intersect_domain_wrapped_domain(
        __isl_take isl_pw_qpolynomial *pwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_intersect_domain_wrapped_domain

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_intersect_domain_wrapped_domain(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_domain_wrapped_domain

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_domain_wrapped_domain(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_domain

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_domain(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_intersect_domain_wrapped_range

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_intersect_domain_wrapped_range(
        __isl_take isl_pw_qpolynomial *pwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_intersect_domain_wrapped_range

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_intersect_domain_wrapped_range(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_domain_wrapped_range

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_domain_wrapped_range(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_range

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_range(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_intersect_params

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_intersect_params(
        __isl_take isl_pw_qpolynomial *pwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_intersect_params

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_intersect_params(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_intersect_params

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_intersect_params(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_intersect_params

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_intersect_params(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_union

```
__isl_give isl_set *isl_basic_set_union(
        __isl_take isl_basic_set *bset1,
        __isl_take isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_union

```
__isl_give isl_set *isl_set_union(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_union

```
__isl_give isl_set *isl_set_list_union(
        __isl_take isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_union

```
__isl_give isl_map *isl_basic_map_union(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_union

```
__isl_give isl_map *isl_map_union(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_union

```
__isl_give isl_union_set *isl_union_set_union(
        __isl_take isl_union_set *uset1,
        __isl_take isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_list_union

```
__isl_give isl_union_set *isl_union_set_list_union(
        __isl_take isl_union_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_union

```
__isl_give isl_union_map *isl_union_map_union(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_subtract

```
__isl_give isl_set *isl_set_subtract(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_subtract

```
__isl_give isl_map *isl_map_subtract(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_subtract_domain

```
__isl_give isl_map *isl_map_subtract_domain(
        __isl_take isl_map *map,
        __isl_take isl_set *dom);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_subtract_range

```
__isl_give isl_map *isl_map_subtract_range(
        __isl_take isl_map *map,
        __isl_take isl_set *dom);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_subtract

```
__isl_give isl_union_set *isl_union_set_subtract(
        __isl_take isl_union_set *uset1,
        __isl_take isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_subtract

```
__isl_give isl_union_map *isl_union_map_subtract(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_subtract_domain

```
__isl_give isl_union_map *isl_union_map_subtract_domain(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *dom);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_subtract_range

```
__isl_give isl_union_map *isl_union_map_subtract_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *dom);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_subtract_domain

```
__isl_give isl_pw_aff *isl_pw_aff_subtract_domain(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_subtract_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_subtract_domain(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_subtract_domain_union_set

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_subtract_domain_union_set(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_subtract_domain_space

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_subtract_domain_space(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_subtract_domain

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_subtract_domain(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_subtract_domain_union_set

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_subtract_domain_union_set(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_subtract_domain_space

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_subtract_domain_space(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_subtract_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_subtract_domain(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_subtract_domain

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_subtract_domain(
        __isl_take isl_pw_qpolynomial *pwpq,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_subtract_domain

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_subtract_domain(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_subtract_domain_union_set

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_subtract_domain_union_set(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_subtract_domain_space

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_subtract_domain_space(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_subtract_domain

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_subtract_domain(
        __isl_take isl_union_pw_qpolynomial *upwpq,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_subtract_domain_union_set

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_subtract_domain_union_set(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_subtract_domain_space

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_subtract_domain_space(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_subtract_domain

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_subtract_domain(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_join

```
__isl_give isl_space *isl_space_join(
        __isl_take isl_space *left,
        __isl_take isl_space *right);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_apply

```
__isl_give isl_basic_set *isl_basic_set_apply(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_apply

```
__isl_give isl_set *isl_set_apply(
        __isl_take isl_set *set,
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_apply

```
__isl_give isl_union_set *isl_union_set_apply(
        __isl_take isl_union_set *uset,
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_apply_domain

```
__isl_give isl_basic_map *isl_basic_map_apply_domain(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_apply_range

```
__isl_give isl_basic_map *isl_basic_map_apply_range(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_apply_domain

```
__isl_give isl_map *isl_map_apply_domain(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_apply_range

```
__isl_give isl_map *isl_map_apply_range(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_apply_domain

```
__isl_give isl_union_map *isl_union_map_apply_domain(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_apply_range

```
__isl_give isl_union_map *isl_union_map_apply_range(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_apply_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_apply_union_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_apply_aff

```
__isl_give isl_union_pw_aff *
isl_multi_union_pw_aff_apply_aff(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_aff *aff);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_apply_pw_aff

```
__isl_give isl_union_pw_aff *
isl_multi_union_pw_aff_apply_pw_aff(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_apply_multi_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_apply_multi_aff(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_apply_pw_multi_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_apply_pw_multi_aff(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_apply_pw_qpolynomial_fold

```
__isl_give isl_pw_qpolynomial_fold *
isl_set_apply_pw_qpolynomial_fold(
        __isl_take isl_set *set,
        __isl_take isl_pw_qpolynomial_fold *pwf,
        isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_apply_pw_qpolynomial_fold

```
__isl_give isl_pw_qpolynomial_fold *
isl_map_apply_pw_qpolynomial_fold(
        __isl_take isl_map *map,
        __isl_take isl_pw_qpolynomial_fold *pwf,
        isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_apply_union_pw_qpolynomial_fold

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_set_apply_union_pw_qpolynomial_fold(
        __isl_take isl_union_set *uset,
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_apply_union_pw_qpolynomial_fold

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_map_apply_union_pw_qpolynomial_fold(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_preimage_multi_aff

```
__isl_give isl_set *isl_set_preimage_multi_aff(
        __isl_take isl_set *set,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_preimage_pw_multi_aff

```
__isl_give isl_set *isl_set_preimage_pw_multi_aff(
        __isl_take isl_set *set,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_preimage_multi_pw_aff

```
__isl_give isl_set *isl_set_preimage_multi_pw_aff(
        __isl_take isl_set *set,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_preimage_multi_aff

```
__isl_give isl_union_set *
isl_union_set_preimage_multi_aff(
        __isl_take isl_union_set *uset,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_preimage_pw_multi_aff

```
__isl_give isl_union_set *
isl_union_set_preimage_pw_multi_aff(
        __isl_take isl_union_set *uset,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_preimage_union_pw_multi_aff

```
__isl_give isl_union_set *
isl_union_set_preimage_union_pw_multi_aff(
        __isl_take isl_union_set *uset,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_preimage_domain_multi_aff

```
__isl_give isl_basic_map *
isl_basic_map_preimage_domain_multi_aff(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_preimage_domain_multi_aff

```
__isl_give isl_map *isl_map_preimage_domain_multi_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_preimage_range_multi_aff

```
__isl_give isl_map *isl_map_preimage_range_multi_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_preimage_domain_pw_multi_aff

```
__isl_give isl_map *
isl_map_preimage_domain_pw_multi_aff(
        __isl_take isl_map *map,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_preimage_range_pw_multi_aff

```
__isl_give isl_map *
isl_map_preimage_range_pw_multi_aff(
        __isl_take isl_map *map,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_preimage_domain_multi_pw_aff

```
__isl_give isl_map *
isl_map_preimage_domain_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_preimage_range_multi_aff

```
__isl_give isl_basic_map *
isl_basic_map_preimage_range_multi_aff(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_domain_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_domain_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_range_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_range_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_domain_pw_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_domain_pw_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_range_pw_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_range_pw_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_domain_union_pw_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_domain_union_pw_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_preimage_range_union_pw_multi_aff

```
__isl_give isl_union_map *
isl_union_map_preimage_range_union_pw_multi_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_preimage_domain_wrapped_domain_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_preimage_domain_wrapped_domain_union_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_pullback_aff

```
__isl_give isl_aff *isl_aff_pullback_aff(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_pullback_multi_aff

```
__isl_give isl_aff *isl_aff_pullback_multi_aff(
        __isl_take isl_aff *aff,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_pullback_multi_aff

```
__isl_give isl_pw_aff *isl_pw_aff_pullback_multi_aff(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_pullback_pw_multi_aff

```
__isl_give isl_pw_aff *isl_pw_aff_pullback_pw_multi_aff(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_pullback_multi_pw_aff

```
__isl_give isl_pw_aff *isl_pw_aff_pullback_multi_pw_aff(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_pullback_multi_aff

```
__isl_give isl_multi_aff *isl_multi_aff_pullback_multi_aff(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_pullback_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_pullback_multi_aff(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_pullback_multi_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_pullback_multi_aff(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_pullback_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_pullback_pw_multi_aff(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_pullback_pw_multi_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_pullback_pw_multi_aff(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_pullback_multi_pw_aff

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_pullback_multi_pw_aff(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_pullback_union_pw_multi_aff

```
__isl_give isl_union_pw_aff *
isl_union_pw_aff_pullback_union_pw_multi_aff(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_pullback_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_pullback_union_pw_multi_aff(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_pullback_union_pw_multi_aff

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_pullback_union_pw_multi_aff(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_eq_basic_set

```
__isl_give isl_basic_set *isl_aff_eq_basic_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_eq_set

```
__isl_give isl_set *isl_aff_eq_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_ne_set

```
__isl_give isl_set *isl_aff_ne_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_le_basic_set

```
__isl_give isl_basic_set *isl_aff_le_basic_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_le_set

```
__isl_give isl_set *isl_aff_le_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_lt_basic_set

```
__isl_give isl_basic_set *isl_aff_lt_basic_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_lt_set

```
__isl_give isl_set *isl_aff_lt_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_ge_set

```
__isl_give isl_set *isl_aff_ge_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_gt_basic_set

```
__isl_give isl_basic_set *isl_aff_gt_basic_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_gt_set

```
__isl_give isl_set *isl_aff_gt_set(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_eq_set

```
__isl_give isl_set *isl_pw_aff_eq_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_ne_set

```
__isl_give isl_set *isl_pw_aff_ne_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_le_set

```
__isl_give isl_set *isl_pw_aff_le_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_lt_set

```
__isl_give isl_set *isl_pw_aff_lt_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_ge_set

```
__isl_give isl_set *isl_pw_aff_ge_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_gt_set

```
__isl_give isl_set *isl_pw_aff_gt_set(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_lex_le_set

```
__isl_give isl_set *isl_multi_aff_lex_le_set(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_lex_lt_set

```
__isl_give isl_set *isl_multi_aff_lex_lt_set(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_lex_ge_set

```
__isl_give isl_set *isl_multi_aff_lex_ge_set(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_lex_gt_set

```
__isl_give isl_set *isl_multi_aff_lex_gt_set(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_eq_set

```
__isl_give isl_set *isl_pw_aff_list_eq_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_ne_set

```
__isl_give isl_set *isl_pw_aff_list_ne_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_le_set

```
__isl_give isl_set *isl_pw_aff_list_le_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_lt_set

```
__isl_give isl_set *isl_pw_aff_list_lt_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_ge_set

```
__isl_give isl_set *isl_pw_aff_list_ge_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_gt_set

```
__isl_give isl_set *isl_pw_aff_list_gt_set(
        __isl_take isl_pw_aff_list *list1,
        __isl_take isl_pw_aff_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_eq_map

```
__isl_give isl_map *isl_pw_aff_eq_map(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_le_map

```
__isl_give isl_map *isl_pw_aff_le_map(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_lt_map

```
__isl_give isl_map *isl_pw_aff_lt_map(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_ge_map

```
__isl_give isl_map *isl_pw_aff_ge_map(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_gt_map

```
__isl_give isl_map *isl_pw_aff_gt_map(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_eq_map

```
__isl_give isl_map *isl_multi_pw_aff_eq_map(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_lex_le_map

```
__isl_give isl_map *isl_multi_pw_aff_lex_le_map(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_lex_lt_map

```
__isl_give isl_map *isl_multi_pw_aff_lex_lt_map(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_lex_ge_map

```
__isl_give isl_map *isl_multi_pw_aff_lex_ge_map(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_lex_gt_map

```
__isl_give isl_map *isl_multi_pw_aff_lex_gt_map(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_eq_at_multi_pw_aff

```
__isl_give isl_map *isl_map_eq_at_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_lt_at_multi_pw_aff

```
__isl_give isl_map *isl_map_lex_lt_at_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_le_at_multi_pw_aff

```
__isl_give isl_map *isl_map_lex_le_at_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_gt_at_multi_pw_aff

```
__isl_give isl_map *isl_map_lex_gt_at_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lex_ge_at_multi_pw_aff

```
__isl_give isl_map *isl_map_lex_ge_at_multi_pw_aff(
        __isl_take isl_map *map,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_eq_at_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_eq_at_multi_union_pw_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lex_lt_at_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_lex_lt_at_multi_union_pw_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lex_le_at_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_lex_le_at_multi_union_pw_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lex_gt_at_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_lex_gt_at_multi_union_pw_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lex_ge_at_multi_union_pw_aff

```
__isl_give isl_union_map *
isl_union_map_lex_ge_at_multi_union_pw_aff(
        __isl_take isl_union_map *umap,
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_product

```
__isl_give isl_space *isl_space_product(
        __isl_take isl_space *space1,
        __isl_take isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_product

```
__isl_give isl_space *isl_space_domain_product(
        __isl_take isl_space *space1,
        __isl_take isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_product

```
__isl_give isl_space *isl_space_range_product(
        __isl_take isl_space *space1,
        __isl_take isl_space *space2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_product

```
__isl_give isl_set *isl_set_product(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_domain_product

```
__isl_give isl_basic_map *isl_basic_map_domain_product(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_range_product

```
__isl_give isl_basic_map *isl_basic_map_range_product(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_product

```
__isl_give isl_basic_map *isl_basic_map_product(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_product

```
__isl_give isl_map *isl_map_domain_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_product

```
__isl_give isl_map *isl_map_range_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_product

```
__isl_give isl_map *isl_map_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_product

```
__isl_give isl_union_set *isl_union_set_product(
        __isl_take isl_union_set *uset1,
        __isl_take isl_union_set *uset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_product

```
__isl_give isl_union_map *isl_union_map_domain_product(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_product

```
__isl_give isl_union_map *isl_union_map_range_product(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_product

```
__isl_give isl_union_map *isl_union_map_product(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_range_product

```
__isl_give isl_multi_id *isl_multi_id_range_product(
        __isl_take isl_multi_id *mi1,
        __isl_take isl_multi_id *mi2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_range_product

```
__isl_give isl_multi_val *isl_multi_val_range_product(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_product

```
__isl_give isl_multi_val *isl_multi_val_product(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_product

```
__isl_give isl_multi_aff *isl_multi_aff_range_product(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_product

```
__isl_give isl_multi_aff *isl_multi_aff_product(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_range_product

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_range_product(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_product

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_product(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_range_product

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_range_product(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_product

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_product(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_range_product

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_range_product(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_range_product

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_range_product(
        __isl_take isl_multi_union_pw_aff *mupa1,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_flat_product

```
__isl_give isl_basic_set *isl_basic_set_flat_product(
        __isl_take isl_basic_set *bset1,
        __isl_take isl_basic_set *bset2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_flat_product

```
__isl_give isl_set *isl_set_flat_product(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_flat_range_product

```
__isl_give isl_basic_map *isl_basic_map_flat_range_product(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flat_domain_product

```
__isl_give isl_map *isl_map_flat_domain_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flat_range_product

```
__isl_give isl_map *isl_map_flat_range_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_flat_product

```
__isl_give isl_basic_map *isl_basic_map_flat_product(
        __isl_take isl_basic_map *bmap1,
        __isl_take isl_basic_map *bmap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_flat_product

```
__isl_give isl_map *isl_map_flat_product(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_flat_domain_product

```
__isl_give isl_union_map *
isl_union_map_flat_domain_product(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_flat_range_product

```
__isl_give isl_union_map *
isl_union_map_flat_range_product(
        __isl_take isl_union_map *umap1,
        __isl_take isl_union_map *umap2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_flat_range_product

```
__isl_give isl_multi_id *
isl_multi_id_flat_range_product(
        __isl_take isl_multi_id *mi1,
        __isl_take isl_multi_id *mi2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_flat_range_product

```
__isl_give isl_multi_val *isl_multi_val_flat_range_product(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_flat_range_product

```
__isl_give isl_multi_aff *isl_multi_aff_flat_range_product(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_flat_range_product

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_flat_range_product(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_flat_range_product

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_flat_range_product(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_flat_range_product

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_flat_range_product(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_flat_range_product

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_flat_range_product(
        __isl_take isl_multi_union_pw_aff *mupa1,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_factor_domain

```
__isl_give isl_space *isl_space_factor_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_factor_range

```
__isl_give isl_space *isl_space_factor_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_factor_domain

```
__isl_give isl_space *isl_space_domain_factor_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_domain_factor_range

```
__isl_give isl_space *isl_space_domain_factor_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_factor_domain

```
__isl_give isl_space *isl_space_range_factor_domain(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_space_range_factor_range

```
__isl_give isl_space *isl_space_range_factor_range(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_factor_domain

```
__isl_give isl_map *isl_map_factor_domain(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_factor_range

```
__isl_give isl_map *isl_map_factor_range(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_factor_domain

```
__isl_give isl_map *isl_map_domain_factor_domain(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_domain_factor_range

```
__isl_give isl_map *isl_map_domain_factor_range(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_factor_domain

```
__isl_give isl_map *isl_map_range_factor_domain(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_range_factor_range

```
__isl_give isl_map *isl_map_range_factor_range(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_factor_domain

```
__isl_give isl_union_map *isl_union_map_factor_domain(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_factor_range

```
__isl_give isl_union_map *isl_union_map_factor_range(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_factor_domain

```
__isl_give isl_union_map *
isl_union_map_domain_factor_domain(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_domain_factor_range

```
__isl_give isl_union_map *
isl_union_map_domain_factor_range(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_factor_domain

```
__isl_give isl_union_map *
isl_union_map_range_factor_domain(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_range_factor_range

```
__isl_give isl_union_map *
isl_union_map_range_factor_range(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_factor_range

```
__isl_give isl_multi_id *isl_multi_id_factor_range(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_range_factor_domain

```
__isl_give isl_multi_id *
isl_multi_id_range_factor_domain(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_range_factor_range

```
__isl_give isl_multi_id *
isl_multi_id_range_factor_range(
        __isl_take isl_multi_id *mi);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_factor_range

```
__isl_give isl_multi_val *isl_multi_val_factor_range(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_range_factor_domain

```
__isl_give isl_multi_val *
isl_multi_val_range_factor_domain(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_range_factor_range

```
__isl_give isl_multi_val *
isl_multi_val_range_factor_range(
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_factor_range

```
__isl_give isl_multi_aff *isl_multi_aff_factor_range(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_factor_domain

```
__isl_give isl_multi_aff *
isl_multi_aff_range_factor_domain(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_factor_range

```
__isl_give isl_multi_aff *
isl_multi_aff_range_factor_range(
        __isl_take isl_multi_aff *ma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_factor_range

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_factor_range(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_range_factor_domain

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_range_factor_domain(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_range_factor_range

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_range_factor_range(
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_range_factor_domain

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_range_factor_domain(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_range_factor_range

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_range_factor_range(
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_range_factor_domain

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_range_factor_domain(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_range_factor_range

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_range_factor_range(
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_factor_range

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_factor_range(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_range_factor_domain

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_range_factor_domain(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_range_factor_range

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_range_factor_range(
        __isl_take isl_multi_union_pw_aff *mupa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_id_range_splice

```
__isl_give isl_multi_id *isl_multi_id_range_splice(
        __isl_take isl_multi_id *mi1, unsigned pos,
        __isl_take isl_multi_id *mi2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_range_splice

```
__isl_give isl_multi_val *isl_multi_val_range_splice(
        __isl_take isl_multi_val *mv1, unsigned pos,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_range_splice

```
__isl_give isl_multi_aff *isl_multi_aff_range_splice(
        __isl_take isl_multi_aff *ma1, unsigned pos,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_splice

```
__isl_give isl_multi_aff *isl_multi_aff_splice(
        __isl_take isl_multi_aff *ma1,
        unsigned in_pos, unsigned out_pos,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_range_splice

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_range_splice(
        __isl_take isl_multi_pw_aff *mpa1, unsigned pos,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_splice

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_splice(
        __isl_take isl_multi_pw_aff *mpa1,
        unsigned in_pos, unsigned out_pos,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_range_splice

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_range_splice(
        __isl_take isl_multi_union_pw_aff *mupa1,
        unsigned pos,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_gist

```
__isl_give isl_basic_set *isl_basic_set_gist(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_gist

```
__isl_give isl_set *isl_set_gist(__isl_take isl_set *set,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_gist_params

```
__isl_give isl_set *isl_set_gist_params(
        __isl_take isl_set *set,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_gist

```
__isl_give isl_basic_map *isl_basic_map_gist(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_map *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_gist_domain

```
__isl_give isl_basic_map *isl_basic_map_gist_domain(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_gist

```
__isl_give isl_map *isl_map_gist(__isl_take isl_map *map,
        __isl_take isl_map *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_gist_params

```
__isl_give isl_map *isl_map_gist_params(
        __isl_take isl_map *map,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_gist_domain

```
__isl_give isl_map *isl_map_gist_domain(
        __isl_take isl_map *map,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_gist_range

```
__isl_give isl_map *isl_map_gist_range(
        __isl_take isl_map *map,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_gist

```
__isl_give isl_union_set *isl_union_set_gist(
        __isl_take isl_union_set *uset,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_gist_params

```
__isl_give isl_union_set *isl_union_set_gist_params(
        __isl_take isl_union_set *uset,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_gist

```
__isl_give isl_union_map *isl_union_map_gist(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_map *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_gist_params

```
__isl_give isl_union_map *isl_union_map_gist_params(
        __isl_take isl_union_map *umap,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_gist_domain

```
__isl_give isl_union_map *isl_union_map_gist_domain(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_gist_range

```
__isl_give isl_union_map *isl_union_map_gist_range(
        __isl_take isl_union_map *umap,
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_gist_params

```
__isl_give isl_aff *isl_aff_gist_params(
        __isl_take isl_aff *aff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_gist

```
__isl_give isl_aff *isl_aff_gist(__isl_take isl_aff *aff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_gist_params

```
__isl_give isl_multi_aff *isl_multi_aff_gist_params(
        __isl_take isl_multi_aff *maff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_gist

```
__isl_give isl_multi_aff *isl_multi_aff_gist(
        __isl_take isl_multi_aff *maff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_gist_params

```
__isl_give isl_pw_aff *isl_pw_aff_gist_params(
        __isl_take isl_pw_aff *pwaff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_gist

```
__isl_give isl_pw_aff *isl_pw_aff_gist(
        __isl_take isl_pw_aff *pwaff,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_gist_params

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_gist_params(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_gist

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_gist(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_gist_params

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_gist_params(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_gist

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_gist(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_gist

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_gist(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_gist_params

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_gist_params(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_gist_params

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_gist_params(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_gist

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_gist(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_gist_params

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_gist_params(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_gist

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_gist(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_gist_params

```
__isl_give isl_qpolynomial *isl_qpolynomial_gist_params(
        __isl_take isl_qpolynomial *qp,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_gist

```
__isl_give isl_qpolynomial *isl_qpolynomial_gist(
        __isl_take isl_qpolynomial *qp,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_gist_params

```
__isl_give isl_qpolynomial_fold *
isl_qpolynomial_fold_gist_params(
        __isl_take isl_qpolynomial_fold *fold,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_gist

```
__isl_give isl_qpolynomial_fold *isl_qpolynomial_fold_gist(
        __isl_take isl_qpolynomial_fold *fold,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_gist_params

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_gist_params(
        __isl_take isl_pw_qpolynomial *pwqp,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_gist

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_gist(
        __isl_take isl_pw_qpolynomial *pwqp,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_gist

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_gist(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_gist_params

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_gist_params(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_gist_params

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_gist_params(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_gist

```
__isl_give isl_union_pw_qpolynomial *isl_union_pw_qpolynomial_gist(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_gist

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_gist(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_union_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_gist_params

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_gist_params(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_sum

```
__isl_give isl_set *isl_set_sum(
        __isl_take isl_set *set1,
        __isl_take isl_set *set2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_sum

```
__isl_give isl_map *isl_map_sum(
        __isl_take isl_map *map1,
        __isl_take isl_map *map2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_add

```
__isl_give isl_multi_val *isl_multi_val_add(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_sub

```
__isl_give isl_multi_val *isl_multi_val_sub(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_min

```
__isl_give isl_multi_val *isl_multi_val_min(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_max

```
__isl_give isl_multi_val *isl_multi_val_max(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_add

```
__isl_give isl_aff *isl_aff_add(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_add

```
__isl_give isl_multi_aff *isl_multi_aff_add(
        __isl_take isl_multi_aff *maff1,
        __isl_take isl_multi_aff *maff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_add

```
__isl_give isl_pw_aff *isl_pw_aff_add(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_add

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_add(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_add

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_add(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_add

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_add(
        __isl_take isl_union_pw_aff *upa1,
        __isl_take isl_union_pw_aff *upa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_add

```
__isl_give isl_union_pw_multi_aff *isl_union_pw_multi_aff_add(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_add

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_add(
        __isl_take isl_multi_union_pw_aff *mupa1,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_add_constant_val

```
__isl_give isl_pw_aff *isl_pw_aff_add_constant_val(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_add_constant_val

```
__isl_give isl_multi_aff *
isl_multi_aff_add_constant_val(
        __isl_take isl_multi_aff *pa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_add_constant_val

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_add_constant_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_add_constant_multi_val

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_add_constant_multi_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_add_constant_val

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_add_constant_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_add_constant_multi_val

```
__isl_give isl_multi_aff *
isl_multi_aff_add_constant_multi_val(
        __isl_take isl_multi_aff *pa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_add_constant_multi_val

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_add_constant_multi_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_min

```
__isl_give isl_pw_aff *isl_pw_aff_min(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_max

```
__isl_give isl_pw_aff *isl_pw_aff_max(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_min

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_min(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_max

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_max(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_sub

```
__isl_give isl_aff *isl_aff_sub(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_sub

```
__isl_give isl_multi_aff *isl_multi_aff_sub(
        __isl_take isl_multi_aff *ma1,
        __isl_take isl_multi_aff *ma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_sub

```
__isl_give isl_pw_aff *isl_pw_aff_sub(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_sub

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_sub(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_sub

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_sub(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_sub

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_sub(
        __isl_take isl_union_pw_aff *upa1,
        __isl_take isl_union_pw_aff *upa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_sub

```
__isl_give isl_union_pw_multi_aff *isl_union_pw_multi_aff_sub(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_sub

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_sub(
        __isl_take isl_multi_union_pw_aff *mupa1,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_add

```
__isl_give isl_qpolynomial *isl_qpolynomial_add(
        __isl_take isl_qpolynomial *qp1,
        __isl_take isl_qpolynomial *qp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_add

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_add(
        __isl_take isl_pw_qpolynomial *pwqp1,
        __isl_take isl_pw_qpolynomial *pwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_add_disjoint

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_add_disjoint(
        __isl_take isl_pw_qpolynomial *pwqp1,
        __isl_take isl_pw_qpolynomial *pwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_add

```
__isl_give isl_pw_qpolynomial_fold *isl_pw_qpolynomial_fold_add(
        __isl_take isl_pw_qpolynomial_fold *pwf1,
        __isl_take isl_pw_qpolynomial_fold *pwf2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_add

```
__isl_give isl_union_pw_qpolynomial *isl_union_pw_qpolynomial_add(
        __isl_take isl_union_pw_qpolynomial *upwqp1,
        __isl_take isl_union_pw_qpolynomial *upwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_sub

```
__isl_give isl_qpolynomial *isl_qpolynomial_sub(
        __isl_take isl_qpolynomial *qp1,
        __isl_take isl_qpolynomial *qp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_sub

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_sub(
        __isl_take isl_pw_qpolynomial *pwqp1,
        __isl_take isl_pw_qpolynomial *pwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_sub

```
__isl_give isl_union_pw_qpolynomial *isl_union_pw_qpolynomial_sub(
        __isl_take isl_union_pw_qpolynomial *upwqp1,
        __isl_take isl_union_pw_qpolynomial *upwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_fold

```
__isl_give isl_pw_qpolynomial_fold *isl_pw_qpolynomial_fold_fold(
        __isl_take isl_pw_qpolynomial_fold *pwf1,
        __isl_take isl_pw_qpolynomial_fold *pwf2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_fold

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_fold(
        __isl_take isl_union_pw_qpolynomial_fold *upwf1,
        __isl_take isl_union_pw_qpolynomial_fold *upwf2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_union_add

```
__isl_give isl_pw_aff *isl_pw_aff_union_add(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_union_add

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_union_add(
        __isl_take isl_multi_pw_aff *mpa1,
        __isl_take isl_multi_pw_aff *mpa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_union_add

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_union_add(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_union_add

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_union_add(
        __isl_take isl_union_pw_aff *upa1,
        __isl_take isl_union_pw_aff *upa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_union_add

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_union_add(
        __isl_take isl_union_pw_multi_aff *upma1,
        __isl_take isl_union_pw_multi_aff *upma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_union_add

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_union_add(
        __isl_take isl_multi_union_pw_aff *mupa1,
        __isl_take isl_multi_union_pw_aff *mupa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_union_min

```
__isl_give isl_pw_aff *isl_pw_aff_union_min(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_union_max

```
__isl_give isl_pw_aff *isl_pw_aff_union_max(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_add_val

```
__isl_give isl_multi_val *isl_multi_val_add_val(
        __isl_take isl_multi_val *mv,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_mod_val

```
__isl_give isl_multi_val *isl_multi_val_mod_val(
        __isl_take isl_multi_val *mv,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_scale_val

```
__isl_give isl_multi_val *isl_multi_val_scale_val(
        __isl_take isl_multi_val *mv,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_scale_down_val

```
__isl_give isl_multi_val *isl_multi_val_scale_down_val(
        __isl_take isl_multi_val *mv,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_mod_val

```
__isl_give isl_aff *isl_aff_mod_val(__isl_take isl_aff *aff,
        __isl_take isl_val *mod);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_mod_val

```
__isl_give isl_pw_aff *isl_pw_aff_mod_val(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_val *mod);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_mod_val

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_mod_val(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_val *f);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_scale_val

```
__isl_give isl_aff *isl_aff_scale_val(__isl_take isl_aff *aff,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_scale_val

```
__isl_give isl_pw_aff *isl_pw_aff_scale_val(
        __isl_take isl_pw_aff *pa, __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_scale_val

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_scale_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_scale_val

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_scale_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_scale_val

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_scale_val(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_val *f);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_scale_val

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_scale_val(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_scale_val

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_scale_val(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_scale_down_ui

```
__isl_give isl_aff *isl_aff_scale_down_ui(
        __isl_take isl_aff *aff, unsigned f);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_scale_down_val

```
__isl_give isl_aff *isl_aff_scale_down_val(
        __isl_take isl_aff *aff, __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_scale_down_val

```
__isl_give isl_pw_aff *isl_pw_aff_scale_down_val(
        __isl_take isl_pw_aff *pa,
        __isl_take isl_val *f);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_scale_down_val

```
__isl_give isl_multi_pw_aff *isl_multi_pw_aff_scale_down_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_scale_down_val

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_scale_down_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_scale_down_val

```
__isl_give isl_union_pw_aff *isl_union_pw_aff_scale_down_val(
        __isl_take isl_union_pw_aff *upa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_scale_down_val

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_scale_down_val(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_val *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_scale_down_val

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_scale_down_val(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_scale_val

```
__isl_give isl_qpolynomial *isl_qpolynomial_scale_val(
        __isl_take isl_qpolynomial *qp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_scale_val

```
__isl_give isl_qpolynomial_fold *
isl_qpolynomial_fold_scale_val(
        __isl_take isl_qpolynomial_fold *fold,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_scale_val

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_scale_val(
        __isl_take isl_pw_qpolynomial *pwqp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_scale_val

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_scale_val(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_scale_val

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_scale_val(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_scale_val

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_scale_val(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_scale_down_val

```
__isl_give isl_qpolynomial *
isl_qpolynomial_scale_down_val(
        __isl_take isl_qpolynomial *qp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_fold_scale_down_val

```
__isl_give isl_qpolynomial_fold *
isl_qpolynomial_fold_scale_down_val(
        __isl_take isl_qpolynomial_fold *fold,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_scale_down_val

```
__isl_give isl_pw_qpolynomial *
isl_pw_qpolynomial_scale_down_val(
        __isl_take isl_pw_qpolynomial *pwqp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_fold_scale_down_val

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_fold_scale_down_val(
        __isl_take isl_pw_qpolynomial_fold *pwf,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_scale_down_val

```
__isl_give isl_union_pw_qpolynomial *
isl_union_pw_qpolynomial_scale_down_val(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_fold_scale_down_val

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_fold_scale_down_val(
        __isl_take isl_union_pw_qpolynomial_fold *upwf,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_mod_multi_val

```
__isl_give isl_multi_val *isl_multi_val_mod_multi_val(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_scale_multi_val

```
__isl_give isl_multi_val *isl_multi_val_scale_multi_val(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_val_scale_down_multi_val

```
__isl_give isl_multi_val *
isl_multi_val_scale_down_multi_val(
        __isl_take isl_multi_val *mv1,
        __isl_take isl_multi_val *mv2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_mod_multi_val

```
__isl_give isl_multi_aff *isl_multi_aff_mod_multi_val(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_mod_multi_val

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_mod_multi_val(
        __isl_take isl_multi_union_pw_aff *upma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_mod_multi_val

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_mod_multi_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_scale_multi_val

```
__isl_give isl_multi_aff *isl_multi_aff_scale_multi_val(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_scale_multi_val

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_scale_multi_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_scale_multi_val

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_scale_multi_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_scale_multi_val

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_scale_multi_val(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_multi_aff_scale_multi_val

```
__isl_give isl_union_pw_multi_aff *
isl_union_pw_multi_aff_scale_multi_val(
        __isl_take isl_union_pw_multi_aff *upma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_aff_scale_down_multi_val

```
__isl_give isl_multi_aff *
isl_multi_aff_scale_down_multi_val(
        __isl_take isl_multi_aff *ma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_scale_down_multi_val

```
__isl_give isl_pw_multi_aff *
isl_pw_multi_aff_scale_down_multi_val(
        __isl_take isl_pw_multi_aff *pma,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_pw_aff_scale_down_multi_val

```
__isl_give isl_multi_pw_aff *
isl_multi_pw_aff_scale_down_multi_val(
        __isl_take isl_multi_pw_aff *mpa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_multi_union_pw_aff_scale_down_multi_val

```
__isl_give isl_multi_union_pw_aff *
isl_multi_union_pw_aff_scale_down_multi_val(
        __isl_take isl_multi_union_pw_aff *mupa,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_mul

```
__isl_give isl_aff *isl_aff_mul(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_div

```
__isl_give isl_aff *isl_aff_div(
        __isl_take isl_aff *aff1,
        __isl_take isl_aff *aff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_mul

```
__isl_give isl_pw_aff *isl_pw_aff_mul(
        __isl_take isl_pw_aff *pwaff1,
        __isl_take isl_pw_aff *pwaff2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_div

```
__isl_give isl_pw_aff *isl_pw_aff_div(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_tdiv_q

```
__isl_give isl_pw_aff *isl_pw_aff_tdiv_q(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_tdiv_r

```
__isl_give isl_pw_aff *isl_pw_aff_tdiv_r(
        __isl_take isl_pw_aff *pa1,
        __isl_take isl_pw_aff *pa2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_qpolynomial_mul

```
__isl_give isl_qpolynomial *isl_qpolynomial_mul(
        __isl_take isl_qpolynomial *qp1,
        __isl_take isl_qpolynomial *qp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_mul

```
__isl_give isl_pw_qpolynomial *isl_pw_qpolynomial_mul(
        __isl_take isl_pw_qpolynomial *pwqp1,
        __isl_take isl_pw_qpolynomial *pwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_mul

```
__isl_give isl_union_pw_qpolynomial *isl_union_pw_qpolynomial_mul(
        __isl_take isl_union_pw_qpolynomial *upwqp1,
        __isl_take isl_union_pw_qpolynomial *upwqp2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_partial_lexmin

```
__isl_give isl_set *isl_basic_set_partial_lexmin(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_partial_lexmax

```
__isl_give isl_set *isl_basic_set_partial_lexmax(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_partial_lexmin

```
__isl_give isl_set *isl_set_partial_lexmin(
        __isl_take isl_set *set, __isl_take isl_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_partial_lexmax

```
__isl_give isl_set *isl_set_partial_lexmax(
        __isl_take isl_set *set, __isl_take isl_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_lexmin

```
__isl_give isl_set *isl_basic_set_lexmin(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_lexmax

```
__isl_give isl_set *isl_basic_set_lexmax(
        __isl_take isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lexmin

```
__isl_give isl_set *isl_set_lexmin(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lexmax

```
__isl_give isl_set *isl_set_lexmax(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_lexmin

```
__isl_give isl_union_set *isl_union_set_lexmin(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_lexmax

```
__isl_give isl_union_set *isl_union_set_lexmax(
        __isl_take isl_union_set *uset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_partial_lexmax

```
__isl_give isl_map *isl_basic_map_partial_lexmax(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_partial_lexmin

```
__isl_give isl_map *isl_basic_map_partial_lexmin(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_partial_lexmax

```
__isl_give isl_map *isl_map_partial_lexmax(
        __isl_take isl_map *map, __isl_take isl_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_partial_lexmin

```
__isl_give isl_map *isl_map_partial_lexmin(
        __isl_take isl_map *map, __isl_take isl_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_lexmin

```
__isl_give isl_map *isl_basic_map_lexmin(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_lexmax

```
__isl_give isl_map *isl_basic_map_lexmax(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lexmin

```
__isl_give isl_map *isl_map_lexmin(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lexmax

```
__isl_give isl_map *isl_map_lexmax(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lexmin

```
__isl_give isl_union_map *isl_union_map_lexmin(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_map_lexmax

```
__isl_give isl_union_map *isl_union_map_lexmax(
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_partial_lexmin_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_basic_set_partial_lexmin_pw_multi_aff(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_partial_lexmax_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_basic_set_partial_lexmax_pw_multi_aff(
        __isl_take isl_basic_set *bset,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lexmin_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_set_lexmin_pw_multi_aff(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_lexmax_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_set_lexmax_pw_multi_aff(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_lexmin_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_basic_map_lexmin_pw_multi_aff(
        __isl_take isl_basic_map *bmap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_partial_lexmin_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_basic_map_partial_lexmin_pw_multi_aff(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_map_partial_lexmax_pw_multi_aff

```
__isl_give isl_pw_multi_aff *
isl_basic_map_partial_lexmax_pw_multi_aff(
        __isl_take isl_basic_map *bmap,
        __isl_take isl_basic_set *dom,
        __isl_give isl_set **empty);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lexmin_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_map_lexmin_pw_multi_aff(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_lexmax_pw_multi_aff

```
__isl_give isl_pw_multi_aff *isl_map_lexmax_pw_multi_aff(
        __isl_take isl_map *map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_union_lexmin

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_union_lexmin(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_union_lexmax

```
__isl_give isl_pw_multi_aff *isl_pw_multi_aff_union_lexmax(
        __isl_take isl_pw_multi_aff *pma1,
        __isl_take isl_pw_multi_aff *pma2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_pip_symmetry

```
isl_stat isl_options_set_pip_symmetry(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_pip_symmetry

```
int isl_options_get_pip_symmetry(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_cond

```
__isl_give isl_pw_aff *isl_pw_aff_cond(
        __isl_take isl_pw_aff *cond,
        __isl_take isl_pw_aff *pwaff_true,
        __isl_take isl_pw_aff *pwaff_false);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_to_list

```
__isl_give isl_set_list *isl_set_to_list(
        __isl_take isl_set *el);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_from_set

```
__isl_give isl_set_list *isl_set_list_from_set(
        __isl_take isl_set *el);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_alloc

```
__isl_give isl_set_list *isl_set_list_alloc(
        isl_ctx *ctx, int n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_copy

```
__isl_give isl_set_list *isl_set_list_copy(
        __isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_insert

```
__isl_give isl_set_list *isl_set_list_insert(
        __isl_take isl_set_list *list, unsigned pos,
        __isl_take isl_set *el);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_add

```
__isl_give isl_set_list *isl_set_list_add(
        __isl_take isl_set_list *list,
        __isl_take isl_set *el);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_drop

```
__isl_give isl_set_list *isl_set_list_drop(
        __isl_take isl_set_list *list,
        unsigned first, unsigned n);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_clear

```
__isl_give isl_set_list *isl_set_list_clear(
        __isl_take isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_swap

```
__isl_give isl_set_list *isl_set_list_swap(
        __isl_take isl_set_list *list,
        unsigned pos1, unsigned pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_reverse

```
__isl_give isl_set_list *isl_set_list_reverse(
        __isl_take isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_set_at

```
__isl_give isl_set_list *isl_set_list_set_at(
        __isl_take isl_set_list *list, int index,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_set_set

```
__isl_give isl_set_list *isl_set_list_set_set(
        __isl_take isl_set_list *list, int index,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_concat

```
__isl_give isl_set_list *isl_set_list_concat(
        __isl_take isl_set_list *list1,
        __isl_take isl_set_list *list2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_map

```
__isl_give isl_set_list *isl_set_list_map(
        __isl_take isl_set_list *list,
        __isl_give isl_set *(*fn)(__isl_take isl_set *el,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_sort

```
__isl_give isl_set_list *isl_set_list_sort(
        __isl_take isl_set_list *list,
        int (*cmp)(__isl_keep isl_set *a,
                __isl_keep isl_set *b, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_free

```
__isl_null isl_set_list *isl_set_list_free(
        __isl_take isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_size

```
isl_size isl_set_list_size(__isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_n_set

```
isl_size isl_set_list_n_set(__isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_get_at

```
__isl_give isl_set *isl_set_list_get_at(
        __isl_keep isl_set_list *list, int index);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_get_set

```
__isl_give isl_set *isl_set_list_get_set(
        __isl_keep isl_set_list *list, int index);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_foreach

```
isl_stat isl_set_list_foreach(__isl_keep isl_set_list *list,
        isl_stat (*fn)(__isl_take isl_set *el, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_every

```
isl_bool isl_set_list_every(__isl_keep isl_set_list *list,
        isl_bool (*test)(__isl_take isl_set *el,
                void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_foreach_scc

```
isl_stat isl_set_list_foreach_scc(
        __isl_keep isl_set_list *list,
        isl_bool (*follows)(__isl_keep isl_set *a,
                __isl_keep isl_set *b, void *user),
        void *follows_user,
        isl_stat (*fn)(__isl_take isl_set_list *scc,
                void *user),
        void *fn_user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_set_list

```
__isl_give isl_printer *isl_printer_print_set_list(
        __isl_take isl_printer *p,
        __isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_to_str

```
__isl_give char *isl_set_list_to_str(
        __isl_keep isl_set_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_val_list_read_from_str

```
__isl_give isl_val_list *isl_val_list_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_list_read_from_str

```
__isl_give isl_id_list *isl_id_list_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_aff_list_read_from_str

```
__isl_give isl_aff_list *
isl_aff_list_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_aff_list_read_from_str

```
__isl_give isl_pw_aff_list *
isl_pw_aff_list_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_multi_aff_list_read_from_str

```
__isl_give isl_pw_multi_aff_list *
isl_pw_multi_aff_list_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_aff_list_read_from_str

```
__isl_give isl_union_pw_aff_list *
isl_union_pw_aff_list_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_set_list_read_from_str

```
__isl_give isl_set_list *isl_set_list_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_map_list_read_from_str

```
__isl_give isl_map_list *isl_map_list_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_list_read_from_str

```
__isl_give isl_union_set_list *
isl_union_set_list_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_alloc

```
__isl_give isl_id_to_ast_expr *isl_id_to_ast_expr_alloc(
        isl_ctx *ctx, int min_size);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_copy

```
__isl_give isl_id_to_ast_expr *isl_id_to_ast_expr_copy(
        __isl_keep isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_free

```
__isl_null isl_id_to_ast_expr *isl_id_to_ast_expr_free(
        __isl_take isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_try_get

```
__isl_give isl_maybe_isl_ast_expr
isl_id_to_ast_expr_try_get(
        __isl_keep isl_id_to_ast_expr *id2expr,
        __isl_keep isl_id *key);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_has

```
isl_bool isl_id_to_ast_expr_has(
        __isl_keep isl_id_to_ast_expr *id2expr,
        __isl_keep isl_id *key);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_get

```
__isl_give isl_ast_expr *isl_id_to_ast_expr_get(
        __isl_keep isl_id_to_ast_expr *id2expr,
        __isl_take isl_id *key);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_foreach

```
isl_stat isl_id_to_ast_expr_foreach(
        __isl_keep isl_id_to_ast_expr *id2expr,
        isl_stat (*fn)(__isl_take isl_id *key,
                __isl_take isl_ast_expr *val, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_every

```
isl_bool isl_id_to_ast_expr_every(
        __isl_keep isl_id_to_ast_expr *id2expr,
        isl_bool (*test)(__isl_keep isl_id *key,
                __isl_keep isl_ast_expr *val, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_set

```
__isl_give isl_id_to_ast_expr *isl_id_to_ast_expr_set(
        __isl_take isl_id_to_ast_expr *id2expr,
        __isl_take isl_id *key,
        __isl_take isl_ast_expr *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_drop

```
__isl_give isl_id_to_ast_expr *isl_id_to_ast_expr_drop(
        __isl_take isl_id_to_ast_expr *id2expr,
        __isl_take isl_id *key);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_is_equal

```
isl_bool isl_id_to_ast_expr_is_equal(
        __isl_take isl_id_to_ast_expr *id2expr1,
        __isl_take isl_id_to_ast_expr *id2expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_id_to_ast_expr

```
__isl_give isl_printer *isl_printer_print_id_to_ast_expr(
        __isl_take isl_printer *p,
        __isl_keep isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_to_str

```
__isl_give char *isl_id_to_ast_expr_to_str(
        __isl_keep isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_id_to_ast_expr_read_from_str

```
__isl_give isl_id_to_ast_expr *
isl_id_to_ast_expr_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_alloc

```
__isl_give isl_vec *isl_vec_alloc(isl_ctx *ctx,
        unsigned size);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_zero

```
__isl_give isl_vec *isl_vec_zero(isl_ctx *ctx,
        unsigned size);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_copy

```
__isl_give isl_vec *isl_vec_copy(__isl_keep isl_vec *vec);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_free

```
__isl_null isl_vec *isl_vec_free(__isl_take isl_vec *vec);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_size

```
isl_size isl_vec_size(__isl_keep isl_vec *vec);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_get_element_val

```
__isl_give isl_val *isl_vec_get_element_val(
        __isl_keep isl_vec *vec, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_set_element_si

```
__isl_give isl_vec *isl_vec_set_element_si(
        __isl_take isl_vec *vec, int pos, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_set_element_val

```
__isl_give isl_vec *isl_vec_set_element_val(
        __isl_take isl_vec *vec, int pos,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_set_si

```
__isl_give isl_vec *isl_vec_set_si(__isl_take isl_vec *vec,
        int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_set_val

```
__isl_give isl_vec *isl_vec_set_val(
        __isl_take isl_vec *vec, __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_cmp_element

```
int isl_vec_cmp_element(__isl_keep isl_vec *vec1,
        __isl_keep isl_vec *vec2, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vec_concat

```
__isl_give isl_vec *isl_vec_concat(__isl_take isl_vec *vec1,
        __isl_take isl_vec *vec2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_alloc

```
__isl_give isl_mat *isl_mat_alloc(isl_ctx *ctx,
        unsigned n_row, unsigned n_col);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_copy

```
__isl_give isl_mat *isl_mat_copy(__isl_keep isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_free

```
__isl_null isl_mat *isl_mat_free(__isl_take isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_rows

```
isl_size isl_mat_rows(__isl_keep isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_cols

```
isl_size isl_mat_cols(__isl_keep isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_get_element_val

```
__isl_give isl_val *isl_mat_get_element_val(
        __isl_keep isl_mat *mat, int row, int col);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_set_element_si

```
__isl_give isl_mat *isl_mat_set_element_si(__isl_take isl_mat *mat,
        int row, int col, int v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_set_element_val

```
__isl_give isl_mat *isl_mat_set_element_val(
        __isl_take isl_mat *mat, int row, int col,
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_rank

```
isl_size isl_mat_rank(__isl_keep isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_right_inverse

```
__isl_give isl_mat *isl_mat_right_inverse(__isl_take isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_right_kernel

```
__isl_give isl_mat *isl_mat_right_kernel(__isl_take isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_row_basis

```
__isl_give isl_mat *isl_mat_row_basis(
        __isl_take isl_mat *mat);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_row_basis_extension

```
__isl_give isl_mat *isl_mat_row_basis_extension(
        __isl_take isl_mat *mat1,
        __isl_take isl_mat *mat2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_mat_has_linearly_independent_rows

```
isl_bool isl_mat_has_linearly_independent_rows(
        __isl_keep isl_mat *mat1,
        __isl_keep isl_mat *mat2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_pw_qpolynomial_bound

```
__isl_give isl_pw_qpolynomial_fold *
isl_pw_qpolynomial_bound(
        __isl_take isl_pw_qpolynomial *pwqp,
        enum isl_fold type, isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_pw_qpolynomial_bound

```
__isl_give isl_union_pw_qpolynomial_fold *
isl_union_pw_qpolynomial_bound(
        __isl_take isl_union_pw_qpolynomial *upwqp,
        enum isl_fold type, isl_bool *tight);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_basic_set_compute_vertices

```
__isl_give isl_vertices *isl_basic_set_compute_vertices(
        __isl_keep isl_basic_set *bset);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertices_foreach_vertex

```
isl_stat isl_vertices_foreach_vertex(
        __isl_keep isl_vertices *vertices,
        isl_stat (*fn)(__isl_take isl_vertex *vertex,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertices_foreach_cell

```
isl_stat isl_vertices_foreach_cell(
        __isl_keep isl_vertices *vertices,
        isl_stat (*fn)(__isl_take isl_cell *cell,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_cell_foreach_vertex

```
isl_stat isl_cell_foreach_vertex(__isl_keep isl_cell *cell,
        isl_stat (*fn)(__isl_take isl_vertex *vertex,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertices_get_n_vertices

```
isl_size isl_vertices_get_n_vertices(
        __isl_keep isl_vertices *vertices);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertices_free

```
__isl_null isl_vertices *isl_vertices_free(
        __isl_take isl_vertices *vertices);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertex_get_id

```
isl_size isl_vertex_get_id(__isl_keep isl_vertex *vertex);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertex_get_domain

```
__isl_give isl_basic_set *isl_vertex_get_domain(
        __isl_keep isl_vertex *vertex);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertex_get_expr

```
__isl_give isl_multi_aff *isl_vertex_get_expr(
        __isl_keep isl_vertex *vertex);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_vertex_free

```
__isl_null isl_vertex *isl_vertex_free(
        __isl_take isl_vertex *vertex);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_cell_get_domain

```
__isl_give isl_basic_set *isl_cell_get_domain(
        __isl_keep isl_cell *cell);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_cell_free

```
__isl_null isl_cell *isl_cell_free(
        __isl_take isl_cell *cell);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_empty

```
__isl_give isl_schedule *isl_schedule_empty(
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_from_domain

```
__isl_give isl_schedule *isl_schedule_from_domain(
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_copy

```
__isl_give isl_schedule *isl_schedule_copy(
        __isl_keep isl_schedule *sched);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_free

```
__isl_null isl_schedule *isl_schedule_free(
        __isl_take isl_schedule *sched);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_plain_is_equal

```
isl_bool isl_schedule_plain_is_equal(
        __isl_keep isl_schedule *schedule1,
        __isl_keep isl_schedule *schedule2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_get_domain

```
__isl_give isl_union_set *isl_schedule_get_domain(
        __isl_keep isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_insert_partial_schedule

```
__isl_give isl_schedule *
isl_schedule_insert_partial_schedule(
        __isl_take isl_schedule *schedule,
        __isl_take isl_multi_union_pw_aff *partial);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_insert_context

```
__isl_give isl_schedule *isl_schedule_insert_context(
        __isl_take isl_schedule *schedule,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_insert_guard

```
__isl_give isl_schedule *isl_schedule_insert_guard(
        __isl_take isl_schedule *schedule,
        __isl_take isl_set *guard);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_sequence

```
__isl_give isl_schedule *isl_schedule_sequence(
        __isl_take isl_schedule *schedule1,
        __isl_take isl_schedule *schedule2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_set

```
__isl_give isl_schedule *isl_schedule_set(
        __isl_take isl_schedule *schedule1,
        __isl_take isl_schedule *schedule2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_intersect_domain

```
__isl_give isl_schedule *isl_schedule_intersect_domain(
        __isl_take isl_schedule *schedule,
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_gist_domain_params

```
__isl_give isl_schedule *isl_schedule_gist_domain_params(
        __isl_take isl_schedule *schedule,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_reset_user

```
__isl_give isl_schedule *isl_schedule_reset_user(
        __isl_take isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_align_params

```
__isl_give isl_schedule *isl_schedule_align_params(
        __isl_take isl_schedule *schedule,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_pullback_union_pw_multi_aff

```
__isl_give isl_schedule *
isl_schedule_pullback_union_pw_multi_aff(
        __isl_take isl_schedule *schedule,
        __isl_take isl_union_pw_multi_aff *upma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_expand

```
__isl_give isl_schedule *isl_schedule_expand(
        __isl_take isl_schedule *schedule,
        __isl_take isl_union_pw_multi_aff *contraction,
        __isl_take isl_schedule *expansion);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_get_map

```
__isl_give isl_union_map *isl_schedule_get_map(
        __isl_keep isl_schedule *sched);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_read_from_file

```
__isl_give isl_schedule *isl_schedule_read_from_file(
        isl_ctx *ctx, FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_read_from_str

```
__isl_give isl_schedule *isl_schedule_read_from_str(
        isl_ctx *ctx, const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_schedule

```
__isl_give isl_printer *isl_printer_print_schedule(
        __isl_take isl_printer *p,
        __isl_keep isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_to_str

```
__isl_give char *isl_schedule_to_str(
        __isl_keep isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_get_root

```
__isl_give isl_schedule_node *isl_schedule_get_root(
        __isl_keep isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_from_domain

```
__isl_give isl_schedule_node *
isl_schedule_node_from_domain(
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_from_extension

```
__isl_give isl_schedule_node *
isl_schedule_node_from_extension(
        __isl_take isl_union_map *extension);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_copy

```
__isl_give isl_schedule_node *isl_schedule_node_copy(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_free

```
__isl_null isl_schedule_node *isl_schedule_node_free(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_is_equal

```
isl_bool isl_schedule_node_is_equal(
        __isl_keep isl_schedule_node *node1,
        __isl_keep isl_schedule_node *node2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_type

```
enum isl_schedule_node_type isl_schedule_node_get_type(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_parent_type

```
enum isl_schedule_node_type
isl_schedule_node_get_parent_type(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_schedule

```
__isl_give isl_schedule *isl_schedule_node_get_schedule(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_has_parent

```
isl_bool isl_schedule_node_has_parent(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_parent

```
__isl_give isl_schedule_node *isl_schedule_node_parent(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_grandparent

```
__isl_give isl_schedule_node *
isl_schedule_node_grandparent(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_root

```
__isl_give isl_schedule_node *isl_schedule_node_root(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_ancestor

```
__isl_give isl_schedule_node *isl_schedule_node_ancestor(
        __isl_take isl_schedule_node *node,
        int generation);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_n_children

```
isl_size isl_schedule_node_n_children(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_child

```
__isl_give isl_schedule_node *isl_schedule_node_child(
        __isl_take isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_has_children

```
isl_bool isl_schedule_node_has_children(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_grandchild

```
__isl_give isl_schedule_node *
isl_schedule_node_grandchild(
        __isl_take isl_schedule_node *node,
        int pos1, int pos2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_first_child

```
__isl_give isl_schedule_node *isl_schedule_node_first_child(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_has_previous_sibling

```
isl_bool isl_schedule_node_has_previous_sibling(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_previous_sibling

```
__isl_give isl_schedule_node *
isl_schedule_node_previous_sibling(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_has_next_sibling

```
isl_bool isl_schedule_node_has_next_sibling(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_next_sibling

```
__isl_give isl_schedule_node *
isl_schedule_node_next_sibling(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_tree_depth

```
isl_size isl_schedule_node_get_tree_depth(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_child_position

```
isl_size isl_schedule_node_get_child_position(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_ancestor_child_position

```
isl_size isl_schedule_node_get_ancestor_child_position(
        __isl_keep isl_schedule_node *node,
        __isl_keep isl_schedule_node *ancestor);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_child

```
__isl_give isl_schedule_node *isl_schedule_node_get_child(
        __isl_keep isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_shared_ancestor

```
__isl_give isl_schedule_node *
isl_schedule_node_get_shared_ancestor(
        __isl_keep isl_schedule_node *node1,
        __isl_keep isl_schedule_node *node2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_foreach_schedule_node_top_down

```
isl_stat isl_schedule_foreach_schedule_node_top_down(
        __isl_keep isl_schedule *sched,
        isl_bool (*fn)(__isl_keep isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_foreach_descendant_top_down

```
isl_stat isl_schedule_node_foreach_descendant_top_down(
        __isl_keep isl_schedule_node *node,
        isl_bool (*fn)(__isl_keep isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_every_descendant

```
isl_bool isl_schedule_node_every_descendant(
        __isl_keep isl_schedule_node *node,
        isl_bool (*test)(__isl_keep isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_foreach_ancestor_top_down

```
isl_stat isl_schedule_node_foreach_ancestor_top_down(
        __isl_keep isl_schedule_node *node,
        isl_stat (*fn)(__isl_keep isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_map_schedule_node_bottom_up

```
__isl_give isl_schedule *
isl_schedule_map_schedule_node_bottom_up(
        __isl_take isl_schedule *schedule,
        __isl_give isl_schedule_node *(*fn)(
                __isl_take isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_map_descendant_bottom_up

```
__isl_give isl_schedule_node *
isl_schedule_node_map_descendant_bottom_up(
        __isl_take isl_schedule_node *node,
        __isl_give isl_schedule_node *(*fn)(
                __isl_take isl_schedule_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_cut

```
__isl_give isl_schedule_node *isl_schedule_node_cut(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_delete

```
__isl_give isl_schedule_node *isl_schedule_node_delete(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_is_subtree_anchored

```
isl_bool isl_schedule_node_is_subtree_anchored(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_reset_user

```
__isl_give isl_schedule_node *isl_schedule_node_reset_user(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_align_params

```
__isl_give isl_schedule_node *
isl_schedule_node_align_params(
        __isl_take isl_schedule_node *node,
        __isl_take isl_space *space);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_space

```
__isl_give isl_space *isl_schedule_node_band_get_space(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_partial_schedule

```
__isl_give isl_multi_union_pw_aff *
isl_schedule_node_band_get_partial_schedule(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_partial_schedule_union_map

```
__isl_give isl_union_map *
isl_schedule_node_band_get_partial_schedule_union_map(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_n_member

```
isl_size isl_schedule_node_band_n_member(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_get_coincident

```
isl_bool isl_schedule_node_band_member_get_coincident(
        __isl_keep isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_set_coincident

```
__isl_give isl_schedule_node *
isl_schedule_node_band_member_set_coincident(
        __isl_take isl_schedule_node *node, int pos,
        int coincident);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_permutable

```
isl_bool isl_schedule_node_band_get_permutable(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_set_permutable

```
__isl_give isl_schedule_node *
isl_schedule_node_band_set_permutable(
        __isl_take isl_schedule_node *node, int permutable);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_get_ast_loop_type

```
enum isl_ast_loop_type
isl_schedule_node_band_member_get_ast_loop_type(
        __isl_keep isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_set_ast_loop_type

```
__isl_give isl_schedule_node *
isl_schedule_node_band_member_set_ast_loop_type(
        __isl_take isl_schedule_node *node, int pos,
        enum isl_ast_loop_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_get_isolate_ast_loop_type

```
enum isl_ast_loop_type
isl_schedule_node_band_member_get_isolate_ast_loop_type(
        __isl_keep isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_member_set_isolate_ast_loop_type

```
__isl_give isl_schedule_node *
isl_schedule_node_band_member_set_isolate_ast_loop_type(
        __isl_take isl_schedule_node *node, int pos,
        enum isl_ast_loop_type type);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_ast_build_options

```
__isl_give isl_union_set *
isl_schedule_node_band_get_ast_build_options(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_set_ast_build_options

```
__isl_give isl_schedule_node *
isl_schedule_node_band_set_ast_build_options(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_get_ast_isolate_option

```
__isl_give isl_set *
isl_schedule_node_band_get_ast_isolate_option(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_context_get_context

```
__isl_give isl_set *
isl_schedule_node_context_get_context(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_domain_get_domain

```
__isl_give isl_union_set *
isl_schedule_node_domain_get_domain(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_expansion_get_expansion

```
__isl_give isl_union_map *
isl_schedule_node_expansion_get_expansion(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_expansion_get_contraction

```
__isl_give isl_union_pw_multi_aff *
isl_schedule_node_expansion_get_contraction(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_extension_get_extension

```
__isl_give isl_union_map *
isl_schedule_node_extension_get_extension(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_filter_get_filter

```
__isl_give isl_union_set *
isl_schedule_node_filter_get_filter(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_guard_get_guard

```
__isl_give isl_set *isl_schedule_node_guard_get_guard(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_mark_get_id

```
__isl_give isl_id *isl_schedule_node_mark_get_id(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_prefix_schedule_multi_union_pw_aff

```
__isl_give isl_multi_union_pw_aff *
isl_schedule_node_get_prefix_schedule_multi_union_pw_aff(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_prefix_schedule_union_pw_multi_aff

```
__isl_give isl_union_pw_multi_aff *
isl_schedule_node_get_prefix_schedule_union_pw_multi_aff(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_prefix_schedule_union_map

```
__isl_give isl_union_map *
isl_schedule_node_get_prefix_schedule_union_map(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_prefix_schedule_relation

```
__isl_give isl_union_map *
isl_schedule_node_get_prefix_schedule_relation(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_subtree_schedule_union_map

```
__isl_give isl_union_map *
isl_schedule_node_get_subtree_schedule_union_map(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_subtree_expansion

```
__isl_give isl_union_map *
isl_schedule_node_get_subtree_expansion(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_subtree_contraction

```
__isl_give isl_union_pw_multi_aff *
isl_schedule_node_get_subtree_contraction(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_schedule_depth

```
isl_size isl_schedule_node_get_schedule_depth(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_domain

```
__isl_give isl_union_set *
isl_schedule_node_get_domain(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_get_universe_domain

```
__isl_give isl_union_set *
isl_schedule_node_get_universe_domain(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_partial_schedule

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_partial_schedule(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_union_pw_aff *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_context

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_context(
        __isl_take isl_schedule_node *node,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_filter

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_filter(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set *filter);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_guard

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_guard(
        __isl_take isl_schedule_node *node,
        __isl_take isl_set *guard);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_mark

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_mark(
        __isl_take isl_schedule_node *node,
        __isl_take isl_id *mark);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_sequence

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_sequence(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set_list *filters);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_insert_set

```
__isl_give isl_schedule_node *
isl_schedule_node_insert_set(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set_list *filters);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_group

```
__isl_give isl_schedule_node *isl_schedule_node_group(
        __isl_take isl_schedule_node *node,
        __isl_take isl_id *group_id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_sequence_splice_child

```
__isl_give isl_schedule_node *
isl_schedule_node_sequence_splice_child(
        __isl_take isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_sequence_splice_children

```
__isl_give isl_schedule_node *
isl_schedule_node_sequence_splice_children(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_scale

```
__isl_give isl_schedule_node *
isl_schedule_node_band_scale(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_scale_down

```
__isl_give isl_schedule_node *
isl_schedule_node_band_scale_down(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_mod

```
__isl_give isl_schedule_node *
isl_schedule_node_band_mod(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_val *mv);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_shift

```
__isl_give isl_schedule_node *
isl_schedule_node_band_shift(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_union_pw_aff *shift);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_tile

```
__isl_give isl_schedule_node *isl_schedule_node_band_tile(
        __isl_take isl_schedule_node *node,
        __isl_take isl_multi_val *sizes);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_tile_scale_tile_loops

```
isl_stat isl_options_set_tile_scale_tile_loops(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_tile_scale_tile_loops

```
int isl_options_get_tile_scale_tile_loops(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_tile_shift_point_loops

```
isl_stat isl_options_set_tile_shift_point_loops(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_tile_shift_point_loops

```
int isl_options_get_tile_shift_point_loops(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_split

```
__isl_give isl_schedule_node *isl_schedule_node_band_split(
        __isl_take isl_schedule_node *node, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_band_sink

```
__isl_give isl_schedule_node *isl_schedule_node_band_sink(
        __isl_take isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_order_before

```
__isl_give isl_schedule_node *
isl_schedule_node_order_before(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set *filter);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_order_after

```
__isl_give isl_schedule_node *
isl_schedule_node_order_after(
        __isl_take isl_schedule_node *node,
        __isl_take isl_union_set *filter);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_graft_before

```
__isl_give isl_schedule_node *
isl_schedule_node_graft_before(
        __isl_take isl_schedule_node *node,
        __isl_take isl_schedule_node *graft);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_graft_after

```
__isl_give isl_schedule_node *
isl_schedule_node_graft_after(
        __isl_take isl_schedule_node *node,
        __isl_take isl_schedule_node *graft);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_schedule_node

```
__isl_give isl_printer *isl_printer_print_schedule_node(
        __isl_take isl_printer *p,
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_node_to_str

```
__isl_give char *isl_schedule_node_to_str(
        __isl_keep isl_schedule_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_compute_flow

```
__isl_give isl_union_flow *
isl_union_access_info_compute_flow(
        __isl_take isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_from_sink

```
__isl_give isl_union_access_info *
isl_union_access_info_from_sink(
        __isl_take isl_union_map *sink);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_set_kill

```
__isl_give isl_union_access_info *
isl_union_access_info_set_kill(
        __isl_take isl_union_access_info *access,
        __isl_take isl_union_map *kill);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_set_may_source

```
__isl_give isl_union_access_info *
isl_union_access_info_set_may_source(
        __isl_take isl_union_access_info *access,
        __isl_take isl_union_map *may_source);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_set_must_source

```
__isl_give isl_union_access_info *
isl_union_access_info_set_must_source(
        __isl_take isl_union_access_info *access,
        __isl_take isl_union_map *must_source);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_set_schedule

```
__isl_give isl_union_access_info *
isl_union_access_info_set_schedule(
        __isl_take isl_union_access_info *access,
        __isl_take isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_set_schedule_map

```
__isl_give isl_union_access_info *
isl_union_access_info_set_schedule_map(
        __isl_take isl_union_access_info *access,
        __isl_take isl_union_map *schedule_map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_copy

```
__isl_give isl_union_access_info *
isl_union_access_info_copy(
        __isl_keep isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_free

```
__isl_null isl_union_access_info *
isl_union_access_info_free(
        __isl_take isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_read_from_file

```
__isl_give isl_union_access_info *
isl_union_access_info_read_from_file(isl_ctx *ctx,
        FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_access_info

```
__isl_give isl_printer *
isl_printer_print_union_access_info(
        __isl_take isl_printer *p,
        __isl_keep isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_access_info_to_str

```
__isl_give char *isl_union_access_info_to_str(
        __isl_keep isl_union_access_info *access);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_must_dependence

```
__isl_give isl_union_map *isl_union_flow_get_must_dependence(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_may_dependence

```
__isl_give isl_union_map *isl_union_flow_get_may_dependence(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_full_must_dependence

```
__isl_give isl_union_map *
isl_union_flow_get_full_must_dependence(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_full_may_dependence

```
__isl_give isl_union_map *
isl_union_flow_get_full_may_dependence(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_must_no_source

```
__isl_give isl_union_map *isl_union_flow_get_must_no_source(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_get_may_no_source

```
__isl_give isl_union_map *isl_union_flow_get_may_no_source(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_copy

```
__isl_give isl_union_flow *isl_union_flow_copy(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_free

```
__isl_null isl_union_flow *isl_union_flow_free(
        __isl_take isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_union_flow

```
__isl_give isl_printer *isl_printer_print_union_flow(
        __isl_take isl_printer *p,
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_flow_to_str

```
__isl_give char *isl_union_flow_to_str(
        __isl_keep isl_union_flow *flow);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_access_info_alloc

```
__isl_give isl_access_info *isl_access_info_alloc(
        __isl_take isl_map *sink,
        void *sink_user, isl_access_level_before fn,
        int max_source);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_access_info_add_source

```
__isl_give isl_access_info *isl_access_info_add_source(
        __isl_take isl_access_info *acc,
        __isl_take isl_map *source, int must,
        void *source_user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_access_info_free

```
__isl_null isl_access_info *isl_access_info_free(
        __isl_take isl_access_info *acc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_access_info_compute_flow

```
__isl_give isl_flow *isl_access_info_compute_flow(
        __isl_take isl_access_info *acc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_flow_foreach

```
isl_stat isl_flow_foreach(__isl_keep isl_flow *deps,
        isl_stat (*fn)(__isl_take isl_map *dep, int must,
                  void *dep_user, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_flow_get_no_source

```
__isl_give isl_map *isl_flow_get_no_source(
        __isl_keep isl_flow *deps, int must);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_flow_free

```
__isl_null isl_flow *isl_flow_free(
        __isl_take isl_flow *deps);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_access_info_set_restrict

```
__isl_give isl_access_info *isl_access_info_set_restrict(
        __isl_take isl_access_info *acc,
        isl_access_restrict fn, void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_input

```
__isl_give isl_restriction *isl_restriction_input(
        __isl_take isl_set *source_restr,
        __isl_take isl_set *sink_restr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_output

```
__isl_give isl_restriction *isl_restriction_output(
        __isl_take isl_set *source_restr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_none

```
__isl_give isl_restriction *isl_restriction_none(
        __isl_take isl_map *source_map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_empty

```
__isl_give isl_restriction *isl_restriction_empty(
        __isl_take isl_map *source_map);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_restriction_free

```
__isl_null isl_restriction *isl_restriction_free(
        __isl_take isl_restriction *restr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_compute_schedule

```
__isl_give isl_schedule *
isl_schedule_constraints_compute_schedule(
        __isl_take isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_copy

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_copy(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_on_domain

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_on_domain(
        __isl_take isl_union_set *domain);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_set_context

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_set_context(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_set *context);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_set_validity

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_set_validity(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_union_map *validity);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_set_coincidence

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_set_coincidence(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_union_map *coincidence);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_set_proximity

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_set_proximity(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_union_map *proximity);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_set_conditional_validity

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_set_conditional_validity(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_union_map *condition,
        __isl_take isl_union_map *validity);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_apply

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_apply(
        __isl_take isl_schedule_constraints *sc,
        __isl_take isl_union_map *umap);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_free

```
__isl_null isl_schedule_constraints *
isl_schedule_constraints_free(
        __isl_take isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_domain

```
__isl_give isl_union_set *
isl_schedule_constraints_get_domain(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_context

```
__isl_give isl_set *isl_schedule_constraints_get_context(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_validity

```
__isl_give isl_union_map *
isl_schedule_constraints_get_validity(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_coincidence

```
__isl_give isl_union_map *
isl_schedule_constraints_get_coincidence(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_proximity

```
__isl_give isl_union_map *
isl_schedule_constraints_get_proximity(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_conditional_validity

```
__isl_give isl_union_map *
isl_schedule_constraints_get_conditional_validity(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_get_conditional_validity_condition

```
__isl_give isl_union_map *
isl_schedule_constraints_get_conditional_validity_condition(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_read_from_str

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_read_from_str(isl_ctx *ctx,
        const char *str);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_read_from_file

```
__isl_give isl_schedule_constraints *
isl_schedule_constraints_read_from_file(isl_ctx *ctx,
        FILE *input);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_schedule_constraints

```
__isl_give isl_printer *
isl_printer_print_schedule_constraints(
        __isl_take isl_printer *p,
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_schedule_constraints_to_str

```
__isl_give char *isl_schedule_constraints_to_str(
        __isl_keep isl_schedule_constraints *sc);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_union_set_compute_schedule

```
__isl_give isl_schedule *isl_union_set_compute_schedule(
        __isl_take isl_union_set *domain,
        __isl_take isl_union_map *validity,
        __isl_take isl_union_map *proximity);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_max_coefficient

```
isl_stat isl_options_set_schedule_max_coefficient(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_max_coefficient

```
int isl_options_get_schedule_max_coefficient(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_max_constant_term

```
isl_stat isl_options_set_schedule_max_constant_term(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_max_constant_term

```
int isl_options_get_schedule_max_constant_term(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_serialize_sccs

```
isl_stat isl_options_set_schedule_serialize_sccs(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_serialize_sccs

```
int isl_options_get_schedule_serialize_sccs(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_whole_component

```
isl_stat isl_options_set_schedule_whole_component(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_whole_component

```
int isl_options_get_schedule_whole_component(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_maximize_band_depth

```
isl_stat isl_options_set_schedule_maximize_band_depth(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_maximize_band_depth

```
int isl_options_get_schedule_maximize_band_depth(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_maximize_coincidence

```
isl_stat isl_options_set_schedule_maximize_coincidence(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_maximize_coincidence

```
int isl_options_get_schedule_maximize_coincidence(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_outer_coincidence

```
isl_stat isl_options_set_schedule_outer_coincidence(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_outer_coincidence

```
int isl_options_get_schedule_outer_coincidence(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_split_scaled

```
isl_stat isl_options_set_schedule_split_scaled(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_split_scaled

```
int isl_options_get_schedule_split_scaled(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_treat_coalescing

```
isl_stat isl_options_set_schedule_treat_coalescing(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_treat_coalescing

```
int isl_options_get_schedule_treat_coalescing(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_algorithm

```
isl_stat isl_options_set_schedule_algorithm(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_algorithm

```
int isl_options_get_schedule_algorithm(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_carry_self_first

```
isl_stat isl_options_set_schedule_carry_self_first(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_carry_self_first

```
int isl_options_get_schedule_carry_self_first(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_schedule_separate_components

```
isl_stat isl_options_set_schedule_separate_components(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_schedule_separate_components

```
int isl_options_get_schedule_separate_components(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_alloc

```
__isl_give isl_ast_build *isl_ast_build_alloc(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_from_context

```
__isl_give isl_ast_build *isl_ast_build_from_context(
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_copy

```
__isl_give isl_ast_build *isl_ast_build_copy(
        __isl_keep isl_ast_build *build);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_free

```
__isl_null isl_ast_build *isl_ast_build_free(
        __isl_take isl_ast_build *build);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_node_from_schedule

```
__isl_give isl_ast_node *isl_ast_build_node_from_schedule(
        __isl_keep isl_ast_build *build,
        __isl_take isl_schedule *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_node_from_schedule_map

```
__isl_give isl_ast_node *
isl_ast_build_node_from_schedule_map(
        __isl_keep isl_ast_build *build,
        __isl_take isl_union_map *schedule);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_get_type

```
enum isl_ast_node_type isl_ast_node_get_type(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_get_iterator

```
__isl_give isl_ast_expr *isl_ast_node_for_get_iterator(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_get_init

```
__isl_give isl_ast_expr *isl_ast_node_for_get_init(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_get_cond

```
__isl_give isl_ast_expr *isl_ast_node_for_get_cond(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_get_inc

```
__isl_give isl_ast_expr *isl_ast_node_for_get_inc(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_get_body

```
__isl_give isl_ast_node *isl_ast_node_for_get_body(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_is_degenerate

```
isl_bool isl_ast_node_for_is_degenerate(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_get_cond

```
__isl_give isl_ast_expr *isl_ast_node_if_get_cond(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_get_then_node

```
__isl_give isl_ast_node *isl_ast_node_if_get_then_node(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_get_then

```
__isl_give isl_ast_node *isl_ast_node_if_get_then(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_has_else_node

```
isl_bool isl_ast_node_if_has_else_node(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_has_else

```
isl_bool isl_ast_node_if_has_else(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_get_else_node

```
__isl_give isl_ast_node *isl_ast_node_if_get_else_node(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_get_else

```
__isl_give isl_ast_node *isl_ast_node_if_get_else(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_block_get_children

```
__isl_give isl_ast_node_list *
isl_ast_node_block_get_children(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_mark_get_id

```
__isl_give isl_id *isl_ast_node_mark_get_id(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_mark_get_node

```
__isl_give isl_ast_node *isl_ast_node_mark_get_node(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_user_get_expr

```
__isl_give isl_ast_expr *isl_ast_node_user_get_expr(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_foreach_descendant_top_down

```
isl_stat isl_ast_node_foreach_descendant_top_down(
        __isl_keep isl_ast_node *node,
        isl_bool (*fn)(__isl_keep isl_ast_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_type

```
enum isl_ast_expr_type isl_ast_expr_get_type(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_op_get_type

```
enum isl_ast_expr_op_type isl_ast_expr_op_get_type(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_op_type

```
enum isl_ast_expr_op_type isl_ast_expr_get_op_type(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_op_get_n_arg

```
isl_size isl_ast_expr_op_get_n_arg(__isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_op_n_arg

```
isl_size isl_ast_expr_get_op_n_arg(__isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_op_get_arg

```
__isl_give isl_ast_expr *isl_ast_expr_op_get_arg(
        __isl_keep isl_ast_expr *expr, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_op_arg

```
__isl_give isl_ast_expr *isl_ast_expr_get_op_arg(
        __isl_keep isl_ast_expr *expr, int pos);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_foreach_ast_expr_op_type

```
isl_stat isl_ast_expr_foreach_ast_expr_op_type(
        __isl_keep isl_ast_expr *expr,
        isl_stat (*fn)(enum isl_ast_expr_op_type type,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_foreach_ast_op_type

```
isl_stat isl_ast_expr_foreach_ast_op_type(
        __isl_keep isl_ast_expr *expr,
        isl_stat (*fn)(enum isl_ast_expr_op_type type,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_foreach_ast_expr_op_type

```
isl_stat isl_ast_node_foreach_ast_expr_op_type(
        __isl_keep isl_ast_node *node,
        isl_stat (*fn)(enum isl_ast_expr_op_type type,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_foreach_ast_op_type

```
isl_stat isl_ast_node_foreach_ast_op_type(
        __isl_keep isl_ast_node *node,
        isl_stat (*fn)(enum isl_ast_expr_op_type type,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_id_get_id

```
__isl_give isl_id *isl_ast_expr_id_get_id(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_id

```
__isl_give isl_id *isl_ast_expr_get_id(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_int_get_val

```
__isl_give isl_val *isl_ast_expr_int_get_val(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_get_val

```
__isl_give isl_val *isl_ast_expr_get_val(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_is_equal

```
isl_bool isl_ast_expr_is_equal(
        __isl_keep isl_ast_expr *expr1,
        __isl_keep isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_copy

```
__isl_give isl_ast_node *isl_ast_node_copy(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_free

```
__isl_null isl_ast_node *isl_ast_node_free(
        __isl_take isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_copy

```
__isl_give isl_ast_expr *isl_ast_expr_copy(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_free

```
__isl_null isl_ast_expr *isl_ast_expr_free(
        __isl_take isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_from_val

```
__isl_give isl_ast_expr *isl_ast_expr_from_val(
        __isl_take isl_val *v);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_from_id

```
__isl_give isl_ast_expr *isl_ast_expr_from_id(
        __isl_take isl_id *id);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_neg

```
__isl_give isl_ast_expr *isl_ast_expr_neg(
        __isl_take isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_address_of

```
__isl_give isl_ast_expr *isl_ast_expr_address_of(
        __isl_take isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_add

```
__isl_give isl_ast_expr *isl_ast_expr_add(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_sub

```
__isl_give isl_ast_expr *isl_ast_expr_sub(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_mul

```
__isl_give isl_ast_expr *isl_ast_expr_mul(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_div

```
__isl_give isl_ast_expr *isl_ast_expr_div(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_pdiv_q

```
__isl_give isl_ast_expr *isl_ast_expr_pdiv_q(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_pdiv_r

```
__isl_give isl_ast_expr *isl_ast_expr_pdiv_r(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_and

```
__isl_give isl_ast_expr *isl_ast_expr_and(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2)
__isl_give isl_ast_expr *isl_ast_expr_and_then(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2)
__isl_give isl_ast_expr *isl_ast_expr_or(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2)
__isl_give isl_ast_expr *isl_ast_expr_or_else(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2)
__isl_give isl_ast_expr *isl_ast_expr_eq(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_le

```
__isl_give isl_ast_expr *isl_ast_expr_le(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_lt

```
__isl_give isl_ast_expr *isl_ast_expr_lt(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_ge

```
__isl_give isl_ast_expr *isl_ast_expr_ge(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_gt

```
__isl_give isl_ast_expr *isl_ast_expr_gt(
        __isl_take isl_ast_expr *expr1,
        __isl_take isl_ast_expr *expr2);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_access

```
__isl_give isl_ast_expr *isl_ast_expr_access(
        __isl_take isl_ast_expr *array,
        __isl_take isl_ast_expr_list *indices);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_call

```
__isl_give isl_ast_expr *isl_ast_expr_call(
        __isl_take isl_ast_expr *function,
        __isl_take isl_ast_expr_list *arguments);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_expr_from_set

```
__isl_give isl_ast_expr *isl_ast_build_expr_from_set(
        __isl_keep isl_ast_build *build,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_expr_from_pw_aff

```
__isl_give isl_ast_expr *isl_ast_build_expr_from_pw_aff(
        __isl_keep isl_ast_build *build,
        __isl_take isl_pw_aff *pa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_access_from_pw_multi_aff

```
__isl_give isl_ast_expr *
isl_ast_build_access_from_pw_multi_aff(
        __isl_keep isl_ast_build *build,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_access_from_multi_pw_aff

```
__isl_give isl_ast_expr *
isl_ast_build_access_from_multi_pw_aff(
        __isl_keep isl_ast_build *build,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_call_from_pw_multi_aff

```
__isl_give isl_ast_expr *
isl_ast_build_call_from_pw_multi_aff(
        __isl_keep isl_ast_build *build,
        __isl_take isl_pw_multi_aff *pma);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_call_from_multi_pw_aff

```
__isl_give isl_ast_expr *
isl_ast_build_call_from_multi_pw_aff(
        __isl_keep isl_ast_build *build,
        __isl_take isl_multi_pw_aff *mpa);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_set_op_arg

```
__isl_give isl_ast_expr *isl_ast_expr_set_op_arg(
        __isl_take isl_ast_expr *expr, int pos,
        __isl_take isl_ast_expr *arg);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_substitute_ids

```
__isl_give isl_ast_expr *isl_ast_expr_substitute_ids(
        __isl_take isl_ast_expr *expr,
        __isl_take isl_id_to_ast_expr *id2expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_map_descendant_bottom_up

```
__isl_give isl_ast_node *
isl_ast_node_map_descendant_bottom_up(
        __isl_take isl_ast_node *node,
        __isl_give isl_ast_node *(*fn)(
                __isl_take isl_ast_node *node,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_set_annotation

```
__isl_give isl_ast_node *isl_ast_node_set_annotation(
        __isl_take isl_ast_node *node,
        __isl_take isl_id *annotation);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_get_annotation

```
__isl_give isl_id *isl_ast_node_get_annotation(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_ast_expr

```
__isl_give isl_printer *isl_printer_print_ast_expr(
        __isl_take isl_printer *p,
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_printer_print_ast_node

```
__isl_give isl_printer *isl_printer_print_ast_node(
        __isl_take isl_printer *p,
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_to_str

```
__isl_give char *isl_ast_expr_to_str(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_to_str

```
__isl_give char *isl_ast_node_to_str(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_to_C_str

```
__isl_give char *isl_ast_expr_to_C_str(
        __isl_keep isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_to_C_str

```
__isl_give char *isl_ast_node_to_C_str(
        __isl_keep isl_ast_node *node);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_op_type_set_print_name

```
__isl_give isl_printer *
isl_ast_expr_op_type_set_print_name(
        __isl_take isl_printer *p,
        enum isl_ast_expr_op_type type,
        __isl_keep const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_op_type_set_print_name

```
__isl_give isl_printer *isl_ast_op_type_set_print_name(
        __isl_take isl_printer *p,
        enum isl_ast_expr_op_type type,
        __isl_keep const char *name);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_print_macro_once

```
isl_stat isl_options_set_ast_print_macro_once(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_print_macro_once

```
int isl_options_get_ast_print_macro_once(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_op_type_print_macro

```
__isl_give isl_printer *isl_ast_expr_op_type_print_macro(
        enum isl_ast_expr_op_type type,
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_op_type_print_macro

```
__isl_give isl_printer *isl_ast_op_type_print_macro(
        enum isl_ast_expr_op_type type,
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_expr_print_macros

```
__isl_give isl_printer *isl_ast_expr_print_macros(
        __isl_keep isl_ast_expr *expr,
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_print_macros

```
__isl_give isl_printer *isl_ast_node_print_macros(
        __isl_keep isl_ast_node *node,
        __isl_take isl_printer *p);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_print

```
__isl_give isl_printer *isl_ast_node_print(
        __isl_keep isl_ast_node *node,
        __isl_take isl_printer *p,
        __isl_take isl_ast_print_options *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_for_print

```
__isl_give isl_printer *isl_ast_node_for_print(
        __isl_keep isl_ast_node *node,
        __isl_take isl_printer *p,
        __isl_take isl_ast_print_options *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_if_print

```
__isl_give isl_printer *isl_ast_node_if_print(
        __isl_keep isl_ast_node *node,
        __isl_take isl_printer *p,
        __isl_take isl_ast_print_options *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_print_options_alloc

```
__isl_give isl_ast_print_options *
isl_ast_print_options_alloc(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_print_options_copy

```
__isl_give isl_ast_print_options *
isl_ast_print_options_copy(
        __isl_keep isl_ast_print_options *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_print_options_free

```
__isl_null isl_ast_print_options *
isl_ast_print_options_free(
        __isl_take isl_ast_print_options *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_print_options_set_print_user

```
__isl_give isl_ast_print_options *
isl_ast_print_options_set_print_user(
        __isl_take isl_ast_print_options *options,
        __isl_give isl_printer *(*print_user)(
                __isl_take isl_printer *p,
                __isl_take isl_ast_print_options *options,
                __isl_keep isl_ast_node *node, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_print_options_set_print_for

```
__isl_give isl_ast_print_options *
isl_ast_print_options_set_print_for(
        __isl_take isl_ast_print_options *options,
        __isl_give isl_printer *(*print_for)(
                __isl_take isl_printer *p,
                __isl_take isl_ast_print_options *options,
                __isl_keep isl_ast_node *node, void *user),
        void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_iterator_type

```
isl_stat isl_options_set_ast_iterator_type(
        isl_ctx *ctx, const char *val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_iterator_type

```
const char *isl_options_get_ast_iterator_type(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_always_print_block

```
isl_stat isl_options_set_ast_always_print_block(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_always_print_block

```
int isl_options_get_ast_always_print_block(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_print_outermost_block

```
isl_stat isl_options_set_ast_print_outermost_block(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_print_outermost_block

```
int isl_options_get_ast_print_outermost_block(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_atomic_upper_bound

```
isl_stat isl_options_set_ast_build_atomic_upper_bound(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_atomic_upper_bound

```
int isl_options_get_ast_build_atomic_upper_bound(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_prefer_pdiv

```
isl_stat isl_options_set_ast_build_prefer_pdiv(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_prefer_pdiv

```
int isl_options_get_ast_build_prefer_pdiv(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_detect_min_max

```
isl_stat isl_options_set_ast_build_detect_min_max(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_detect_min_max

```
int isl_options_get_ast_build_detect_min_max(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_exploit_nested_bounds

```
isl_stat isl_options_set_ast_build_exploit_nested_bounds(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_exploit_nested_bounds

```
int isl_options_get_ast_build_exploit_nested_bounds(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_group_coscheduled

```
isl_stat isl_options_set_ast_build_group_coscheduled(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_group_coscheduled

```
int isl_options_get_ast_build_group_coscheduled(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_separation_bounds

```
isl_stat isl_options_set_ast_build_separation_bounds(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_separation_bounds

```
int isl_options_get_ast_build_separation_bounds(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_scale_strides

```
isl_stat isl_options_set_ast_build_scale_strides(
        isl_ctx *ctx, int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_scale_strides

```
int isl_options_get_ast_build_scale_strides(
        isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_allow_else

```
isl_stat isl_options_set_ast_build_allow_else(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_allow_else

```
int isl_options_get_ast_build_allow_else(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_set_ast_build_allow_or

```
isl_stat isl_options_set_ast_build_allow_or(isl_ctx *ctx,
        int val);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_options_get_ast_build_allow_or

```
int isl_options_get_ast_build_allow_or(isl_ctx *ctx);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_options

```
__isl_give isl_ast_build *
isl_ast_build_set_options(
        __isl_take isl_ast_build *build,
        __isl_take isl_union_map *options);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_iterators

```
__isl_give isl_ast_build *
isl_ast_build_set_iterators(
        __isl_take isl_ast_build *build,
        __isl_take isl_id_list *iterators);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_create_leaf

```
__isl_give isl_ast_build *
isl_ast_build_set_create_leaf(
        __isl_take isl_ast_build *build,
        __isl_give isl_ast_node *(*fn)(
                __isl_take isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_user_from_expr

```
__isl_give isl_ast_node *isl_ast_node_user_from_expr(
        __isl_take isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_alloc_user

```
__isl_give isl_ast_node *isl_ast_node_alloc_user(
        __isl_take isl_ast_expr *expr);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_node_block_from_children

```
__isl_give isl_ast_node *isl_ast_node_block_from_children(
        __isl_take isl_ast_node_list *list);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_at_each_domain

```
__isl_give isl_ast_build *
isl_ast_build_set_at_each_domain(
        __isl_take isl_ast_build *build,
        __isl_give isl_ast_node *(*fn)(
                __isl_take isl_ast_node *node,
                __isl_keep isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_before_each_for

```
__isl_give isl_ast_build *
isl_ast_build_set_before_each_for(
        __isl_take isl_ast_build *build,
        __isl_give isl_id *(*fn)(
                __isl_keep isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_after_each_for

```
__isl_give isl_ast_build *
isl_ast_build_set_after_each_for(
        __isl_take isl_ast_build *build,
        __isl_give isl_ast_node *(*fn)(
                __isl_take isl_ast_node *node,
                __isl_keep isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_before_each_mark

```
__isl_give isl_ast_build *
isl_ast_build_set_before_each_mark(
        __isl_take isl_ast_build *build,
        isl_stat (*fn)(__isl_keep isl_id *mark,
                __isl_keep isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_set_after_each_mark

```
__isl_give isl_ast_build *
isl_ast_build_set_after_each_mark(
        __isl_take isl_ast_build *build,
        __isl_give isl_ast_node *(*fn)(
                __isl_take isl_ast_node *node,
                __isl_keep isl_ast_build *build,
                void *user), void *user);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_get_schedule

```
__isl_give isl_union_map *isl_ast_build_get_schedule(
        __isl_keep isl_ast_build *build);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_get_schedule_space

```
__isl_give isl_space *isl_ast_build_get_schedule_space(
        __isl_keep isl_ast_build *build);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)

### isl_ast_build_restrict

```
__isl_give isl_ast_build *isl_ast_build_restrict(
        __isl_take isl_ast_build *build,
        __isl_take isl_set *set);
```

**Checklist**
- [ ] Implemented
- [ ] Reachable as a method in class (if so, implemented at `...`)
- [ ] Included in Mixin (if so, implemented at `...`)
