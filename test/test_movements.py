import caten as C


def test_broadcast_fixed() -> None:
    A = C.Tensor([10, 10, 10])
    B = C.Tensor([10])
    assert [10, 10, 10] == [s.item for s in (A + B).shape]

    
    A = C.Tensor([10, 10, 10])
    B = C.Tensor([10, 10])
    assert [10, 10, 10] == [s.item for s in (A + B).shape]

def test_broadcast_dynamic() -> None:
    pass
    
def test_reshape() -> None:
    A = C.Tensor([10, 10])
    A = A.reshape(5, 2, 5, 2)
    assert [5, 2, 5, 2] == [s.item for s in A.shape]

def test_reshape_size_mismatch() -> None:
    pass

def test_reshape_dynamic() -> None:
    pass
