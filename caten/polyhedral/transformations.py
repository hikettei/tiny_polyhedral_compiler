import caten.isl as I


def schedule_node_sequence_full_fuse(sequence_node: "I.ScheduleNode") -> "I.ScheduleNode":
    """
    Fuse all children in the given schedule_node_sequence.
    Equivalent to schedule-node-sequence-full-fuse in Lisp reference.
    
    Takes a Sequence node where each child is a Filter node containing a Band node.
    Merges the Band schedules into a single Band node inserted above the Sequence.
    The original Band nodes are removed, leaving Filter -> Inner_Schedule.
    """
    # Magic numbers for ISL schedule node types
    TYPE_BAND = 0
    TYPE_FILTER = 5
    TYPE_SEQUENCE = 9
    TYPE_SET = 10
    
    node_type = sequence_node.get_type()
    if node_type not in (TYPE_SEQUENCE, TYPE_SET):
        raise ValueError(f"Node must be sequence or set, got {node_type}")
        
    n_child = sequence_node.n_children()
    mupa_sum = None
    current_seq = sequence_node
    
    # Iterate over children (Filters)
    # We perform deletion in this loop, so we must track current_seq.
    # The index 'i' remains valid because we don't delete filters, only their children.
    
    for i in range(n_child):
        # 1. Get Filter and Partial Schedule
        filter_node = current_seq.child(i)
        if filter_node.get_type() != TYPE_FILTER:
             # Skip or error? Lisp code assumes filter.
             raise ValueError(f"Child {i} of sequence must be filter, got {filter_node.get_type()}")
             
        filter_val = filter_node.filter_get_filter() # UnionSet
        
        band_node = filter_node.child(0)
        if band_node.get_type() != TYPE_BAND:
             raise ValueError(f"Child 0 of filter {i} is not a band (type: {band_node.get_type()})")
             
        partial = band_node.band_get_partial_schedule() # MultiUnionPwAff
        partial = partial.intersect_domain(filter_val)
        
        # reset_tuple_id(dim_type.out)
        # isl_dim_out is usually 3
        # Since I.dim_type might not be exposed as class, we use magic number.
        dim_out = 3
            
        partial = partial.reset_tuple_id(dim_out)
        
        if mupa_sum is None:
            mupa_sum = partial
        else:
            mupa_sum = mupa_sum.union_add(partial)
            
        # 2. Delete the Band Node
        # delete() returns the node replacing the deleted node (the child of band).
        replaced_node = band_node.delete()
        
        # Navigate back to Sequence
        # Filter -> Child
        # Child.parent() -> Filter
        # Filter.parent() -> Sequence
        current_seq = replaced_node.parent().parent()
        
    # 3. Insert Fused Band
    if mupa_sum:
        # Insert partial schedule at Sequence node position.
        # This creates Band -> Sequence.
        new_node = current_seq.insert_partial_schedule(mupa_sum)
        return new_node
    else:
        return current_seq
